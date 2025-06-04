-- May be needed for MonadState
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solver (solve) where

import Ast
import Control.Monad.State
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Language.Fixpoint.Horn.Solve as HS
import qualified Language.Fixpoint.Horn.Types as H -- For H.Tag, H.NoTag, H.Var, H.Cstr etc.
import qualified Language.Fixpoint.Solver as S
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Types.Config as FC

-- Environment: maps variable names from AST to Fixpoint symbols and their sorts
type VEnv = Map.Map Id (F.Symbol, F.Sort)

-- Default Horn Tag to be used for metadata in Horn types
defaultHornTag :: H.Tag
defaultHornTag = H.NoTag

-- CG (Constraint Generation) Monad State
data CGState = CGState
  { cgKVars :: [H.Var H.Tag], -- KVars (Horn variables for unknown refinements) now use H.Tag
    cgConstraints :: [H.Cstr H.Tag] -- Generated Horn constraints now use H.Tag
  }

initialCGState :: CGState
initialCGState = CGState [] []

-- Helper to get a dummy source span (for uses like F.dummyLoc)
dummySpan :: F.SrcSpan
dummySpan = F.dummySpan -- Assuming F.dummySpan is available from Fixpoint.Types

-- Convert OatType to Fixpoint Sort
oatTypeToFixSort :: OatType -> F.Sort
oatTypeToFixSort TBool = F.boolSort
oatTypeToFixSort TInt = F.intSort
oatTypeToFixSort (TRef RString) = F.strSort
oatTypeToFixSort (TRef (RArray _)) = F.FTC $ F.symbolFTycon $ F.dummyLoc $ F.symbol "Array"
oatTypeToFixSort (TRef (RFun _ _)) = F.FTC $ F.symbolFTycon $ F.dummyLoc $ F.symbol "Fun"

-- Get F.Symbol for an AST Id from environment, or create one.
getVarSym :: Id -> VEnv -> F.Symbol
getVarSym x env = maybe (F.symbol x) fst (Map.lookup x env)

-- Convert Oat AST Expression to Fixpoint Expression
astToExp :: VEnv -> Exp -> F.Expr
astToExp _ (CNull _) = F.expr (0 :: Integer) -- Simplified: null is represented as 0
astToExp _ (CBool b) = if b then F.PTrue else F.PFalse
astToExp _ (CInt i) = F.expr (fromIntegral i :: Integer)
astToExp _ (CStr s) = F.expr (F.symbol s)
astToExp env (Id x) = F.EVar (getVarSym x env)
astToExp env (Bop op e1 e2) =
  let fe1 = astToExp env e1
      fe2 = astToExp env e2
   in case op of
        Add -> F.EBin F.Plus fe1 fe2
        Sub -> F.EBin F.Minus fe1 fe2
        Mul -> F.EBin F.Times fe1 fe2
        Eq -> F.PAtom F.Eq fe1 fe2
        Neq -> F.PAtom F.Ne fe1 fe2
        Lt -> F.PAtom F.Lt fe1 fe2
        Lte -> F.PAtom F.Le fe1 fe2
        Gt -> F.PAtom F.Gt fe1 fe2
        Gte -> F.PAtom F.Ge fe1 fe2
        And -> F.PAnd [fe1, fe2] -- Assumes fe1, fe2 are boolean expressions
        Or -> F.POr [fe1, fe2] -- Assumes fe1, fe2 are boolean expressions
astToExp env (Uop opEx e) =
  let fe = astToExp env e
   in case opEx of
        Neg -> F.ENeg fe
        Lognot -> F.PNot fe -- Assumes fe is a boolean expression
        -- Default for unhandled expressions (should ideally be an error or specific logic)
astToExp _ _ = F.PTrue -- Placeholder for CArr, NewArr, Index, Call etc.

-- Convert H.Pred to F.Expr for use in implications
predToExpr :: H.Pred -> F.Expr
predToExpr (H.Reft e) = e
predToExpr (H.Var k args) = F.eApps (F.EVar k) (map F.EVar args) -- Treat Horn KVar as uninterpreted func
predToExpr (H.PAnd ps) = F.PAnd (map predToExpr ps)

-- Result of processing a statement
data StmtResult = StmtResult
  { srPred :: H.Pred, -- Predicate describing state transformation / conditions established
    srRetExp :: Maybe F.Expr, -- Return expression, if statement is Ret
    srEnv :: VEnv -- Updated environment
  }

-- Process a single statement
processStmt :: VEnv -> Stmt -> StmtResult
processStmt env (Decl ty xVal mExp) =
  let xSym = F.symbol xVal
      newEnv = Map.insert xVal (xSym, oatTypeToFixSort ty) env
      pred' = case mExp of
        Just eInit -> H.Reft (F.PAtom F.Eq (F.EVar xSym) (astToExp env eInit))
        Nothing -> H.Reft F.PTrue -- Declaration without initialization
   in StmtResult pred' Nothing newEnv
processStmt env (Assn (Id lval) rhsExp) =
  -- Assuming lvalue is always Id for simplicity
  let lvSym = getVarSym lval env -- Should ideally exist in env
      pred' = H.Reft (F.PAtom F.Eq (F.EVar lvSym) (astToExp env rhsExp))
   in StmtResult pred' Nothing env -- Env structure unchanged, pred describes the new value equality
processStmt env (Ret mExp) =
  let retE = case mExp of
        Just e -> Just (astToExp env e)
        Nothing -> Just F.PTrue -- Void return represented as True
   in StmtResult (H.Reft F.PTrue) retE env
processStmt env (If condExp thenStmts elseStmts) =
  let condAstExpr = astToExp env condExp
      (thenBlockPred, thenBlockRetOpt, _) = processBlockStmts env thenStmts
      (elseBlockPred, elseBlockRetOpt, _) = processBlockStmts env elseStmts

      -- Predicate describing the if-stmt: (cond AND then_pred) OR (!cond AND else_pred)
      thenPathPred = F.PAnd [condAstExpr, predToExpr thenBlockPred]
      elsePathPred = F.PAnd [F.PNot condAstExpr, predToExpr elseBlockPred]
      combinedPred = H.Reft (F.POr [thenPathPred, elsePathPred])

      -- Return expression from If, using F.EIte if both branches return something
      mRetE = case (thenBlockRetOpt, elseBlockRetOpt) of
        (Just te, Just ee) -> Just (F.EIte condAstExpr te ee)
        _ -> Nothing -- If one path doesn't return expr, If doesn't return expr in this model
   in StmtResult combinedPred mRetE env
-- Default for unhandled statements (SCall, For, While)
processStmt env _ = StmtResult (H.Reft F.PTrue) Nothing env

-- Process a block of statements
processBlockStmts :: VEnv -> Block -> (H.Pred, Maybe F.Expr, VEnv)
processBlockStmts startEnv =
  foldl
    ( \(accPred, accRetExp, currentEnv) stmt ->
        -- If a return expression is already found, subsequent statements are effectively dead code for this path.
        if isJust accRetExp
          then (accPred, accRetExp, currentEnv)
          else
            let stmtRes = processStmt currentEnv stmt
                newPred = H.PAnd [accPred, srPred stmtRes] -- Accumulate predicates
             in (newPred, srRetExp stmtRes, srEnv stmtRes)
    )
    (H.Reft F.PTrue, Nothing, startEnv)

-- Generate predicate and return expression for a function body block
generatePredForBlock :: VEnv -> Block -> Id -> State CGState (H.Pred, F.Expr)
generatePredForBlock initialEnv block retValIdNameIfNoExplicitRet = do
  let (blockPred, mRetExp, _) = processBlockStmts initialEnv block
  case mRetExp of
    Just actualRetExp -> return (blockPred, actualRetExp)
    Nothing -> return (blockPred, F.EVar (F.symbol retValIdNameIfNoExplicitRet)) -- Or error if function must return

-- Build the Horn clause for a function
buildHornClause :: [H.Bind H.Tag] -> H.Pred -> H.Pred -> H.Cstr H.Tag
buildHornClause argBinders bodyPremisePred headConclusionPred =
  let implication = H.Reft (F.PImp (predToExpr bodyPremisePred) (predToExpr headConclusionPred))
   in foldr H.All (H.Head implication defaultHornTag) argBinders -- Use defaultHornTag

-- Process a function declaration
processFdecl :: Decl -> State CGState ()
processFdecl (Fdecl (Just rc) _retTy _fnName argsAst block) = do
  let initialEnv = Map.fromList $ map (\(ty, name) -> (name, (F.symbol name, oatTypeToFixSort ty))) (reArgs rc)

  -- Create H.Bind for each argument from RefineCond.
  -- Assume no explicit refinements on args in RefineCond for now (bPred = True).
  let argBinders =
        map
          ( \(oatT, idName) ->
              H.Bind (F.symbol idName) (oatTypeToFixSort oatT) (H.Reft F.PTrue) defaultHornTag -- Use defaultHornTag
          )
          (reArgs rc)

  (bodyPred, actualReturnExp) <- generatePredForBlock initialEnv block (reRetValId rc)

  -- Create the head predicate from RefineCond's reRetCond
  -- This predicate is P(arg1, ..., argN, reRetValId)
  -- We need P(arg1_sym, ..., argN_sym, actualReturnExp)
  let retValSpecSym = F.symbol (reRetValId rc)
  let headPredExpr = case reRetCond rc of
        Just condExp -> F.subst1 (astToExp initialEnv condExp) (retValSpecSym, actualReturnExp)
        Nothing -> F.PTrue -- No specific return condition means True
  let headConclusion = H.Reft headPredExpr

  let hornClause = buildHornClause argBinders bodyPred headConclusion
  modify (\s -> s {cgConstraints = hornClause : cgConstraints s})
processFdecl _ = return () -- Ignore Gdecl or Fdecl without refinement conditions

-- Main solve function
solve :: Prog -> IO String
solve prog = do
  let finalState = execState (mapM_ processFdecl prog) initialCGState

      constraints = cgConstraints finalState
      -- Correctly form a single H.Cstr for the query.
      -- If `constraints` is empty, use a trivial true constraint.
      -- If `constraints` has one element, use that element.
      -- If `constraints` has multiple elements, combine them with H.CAnd.
      actualTopCstr :: H.Cstr H.Tag
      actualTopCstr = case constraints of
                        [] -> H.Head (H.Reft F.PTrue) defaultHornTag
                        [c] -> c
                        cs  -> H.CAnd cs -- H.CAnd expects a list of constraints

      query =
        H.Query
          { H.qQuals = [], -- Consider populating based on sprite-lang example if needed
            H.qVars = cgKVars finalState, -- Now [H.Var H.Tag]
            H.qCstr = actualTopCstr,      -- Now H.Cstr H.Tag
            H.qCon = HMap.empty,         -- Constants like '+', '-' are usually handled by SMT built-ins
            H.qDis = HMap.empty,
            H.qEqns = [],
            H.qDefs = [],
            H.qMats = [],
            H.qData = [],
            H.qOpts = [],                -- Options like "--eliminate" go into FC.Config
            H.qNums = []
          }

  let queryDisplay = "Generated Horn Query:\n" ++ F.showpp query ++ "\n\n"

  -- Configure for Horn solving, typically using SMTCHC backend.
  -- FC.eliminate = FC.Some can be demanding. FC.Horn is also an option.
  let config = FC.defConfig {FC.eliminate = FC.Some}
  solutionResult <- HS.solve config query -- query is now H.Query H.Tag
  let resultOutput = case F.resStatus solutionResult of
        F.Safe _ -> "Solver Status: Safe"
        F.Unsafe _ _ -> "Solver Status: Unsafe" -- Payload is H.Tag, or whatever 'a' is in F.Result a
        F.Crash _ err -> "Solver Status: Crash - " ++ F.showpp err -- Use F.showpp for better error display
  return (queryDisplay ++ resultOutput)
