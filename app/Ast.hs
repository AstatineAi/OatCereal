module Ast where

type Id = String

data OatType
  = TBool
  | TInt
  | TRef RefType
  deriving (Show)

data RefType
  = RString
  | RArray OatType
  | RFun [OatType] OatType
  deriving (Show)

data RetType
  = RetVoid
  | RetVal OatType
  deriving (Show)

data UnOp
  = Neg
  | Lognot
  | Bitnot
  deriving (Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  deriving (Show)

data Exp
  = CNull RefType
  | CBool Bool
  | CInt Int
  | CStr String
  | Id Id
  | CArr OatType [Exp]
  | NewArr OatType Exp
  | NewArrInit OatType Exp Id Exp
  | Index Exp Exp
  | Length Exp
  | Call Exp [Exp]
  | Bop BinOp Exp Exp
  | Uop UnOp Exp
  deriving (Show)

data Stmt
  = Assn Exp Exp
  | Decl OatType Id (Maybe Exp)
  | Ret (Maybe Exp)
  | SCall Exp [Exp]
  | If Exp [Stmt] [Stmt]
  | For [OatType] (Maybe Exp) (Maybe Stmt) [Stmt]
  | While Exp [Stmt]
  deriving (Show)

type Block = [Stmt]

data RefineCond = RefinementCond
  { reArgs :: [(OatType, Id)],
    reRetType :: OatType,
    reRetValId :: Id,
    reRetCond :: Maybe Exp
  }
  deriving (Show)

data Decl
  = Gdecl Id (Maybe Exp)
  | Fdecl (Maybe RefineCond) RetType Id [(OatType, Id)] Block
  deriving (Show)

type Prog = [Decl]
