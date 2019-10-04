{-
はちゅける1 in Haskell 完成版 ＋α
-}

import Data.Maybe (fromMaybe)
import Text.Parsec
import Control.Monad (guard)

-- 抽象構文木

data AST = ASTLambda [String] AST    -- ラムダ抽象
         | ASTLet [ASTVdef] AST      -- 局所定義
         | ASTVarRef String          -- 変数参照
         | ASTInt Int                -- 整数リテラル
         | ASTApp [AST]              -- 関数適用
-- Exercise 1.4
         | ASTIf0 AST AST AST
  deriving Show

data ASTVdef = ASTVdef String AST
  deriving Show

-- 構文木の表示

showAST (ASTApp ts) = unwords $ map showASTk ts
showAST (ASTLambda xs t) =
  "\\" ++ unwords xs ++ " -> " ++ showAST t
showAST (ASTLet ds t) =
  "let " ++ concatMap showVdef ds ++ "in " ++ showAST t
showAST (ASTVarRef x) = x
showAST (ASTInt n) = show n

showASTk t = case t of
  ASTApp _      -> kakko $ showAST t
  ASTLambda _ _ -> kakko $ showAST t
  ASTLet _ _    -> kakko $ showAST t
  _             ->         showAST t

kakko s = '(' : s ++ ")"

showVdef (ASTVdef x t) = x ++ " = " ++ showAST t ++ "; "

-- 値と環境

type Assoc = [(String,Val)]

data Env = Env Env Assoc
         | EmptyEnv

data Val = ValInt Int                             -- 整数値
         | ValLamClosure AST Env Assoc [String]   -- ラムダ閉包
         | ValBinClosure ([Val] -> Val) [Val] Int -- 組込関数

instance Show Val where
  show (ValInt k) = show k
  show (ValLamClosure t _ _ xs) =
    unwords $ "\\" : xs ++ ["->", showAST t]
  show (ValBinClosure _ _ k) = "[Builtin " ++ show k ++ "]"

getEnv :: String -> Env -> Val
getEnv x EmptyEnv = error $ x ++ " is unbound variable"
getEnv x (Env p assoc) =
  fromMaybe (getEnv x p) (lookup x assoc)

-- 正格評価器

stEval :: Env -> AST -> Val
stEval _ (ASTInt n) = ValInt n
stEval env (ASTVarRef x) = getEnv x env
stEval env (ASTLambda xs t) = ValLamClosure t env [] xs
stEval env (ASTApp (t:ts)) = loop (stEval env t) ts
  where
    loop v (t:ts) = loop (apply v (stEval env t)) ts
    loop v []     = v
    apply (ValLamClosure t p as (x:xs)) v =
      let as1 = (x,v):as
      in if null xs then stEval (Env p as1) t
                    else ValLamClosure t p as1 xs
    apply (ValBinClosure f vs 1) v = f (v:vs)
    apply (ValBinClosure f vs n) v =
                 ValBinClosure f (v:vs) (n-1)
    apply f _ = error (show f ++ " cannot apply")
stEval env (ASTLet vds t) = stEval env1 t
  where assoc = [(x, stEval env1 t) | ASTVdef x t <- vds]
        env1  = Env env assoc
-- Exercise 1.4
stEval env (ASTIf0 b t s) =
  case stEval env b of
    ValInt 0 -> stEval env t
    ValInt _ -> stEval env s
    _        -> error "non number in if0 condition"

-- パーサ

type Parser a = Parsec String () a

pInt :: Parser Int
pInt = do ds <- many1 digit; spaces; return (read ds)

pVar :: Parser String
pVar = do
  c  <- lower
  cs <- many (alphaNum <|> char '_')
  let x = c:cs
  guard $ notElem x ["let", "in", "if0", "then", "else"]
  spaces
  return x

pExpr :: Parser AST
pExpr = do
  es <- many1 $ choice $
        map try [pLambda, pLet, pParen, pIf0, pExprVar, pExprInt]
  case es of
    [e] -> return e
    _   -> return (ASTApp es)

pExprVar = ASTVarRef <$> pVar

pExprInt = ASTInt <$> pInt

pToken :: Parser a -> Parser ()
pToken p = p >> spaces

pParen = between (pToken $ char '(')
                 (pToken $ char ')')
                 pExpr

pLambda = do
  pToken $ char '\\'
  xs <- many1 pVar
  pToken $ string "->"
  t <- pExpr
  return (ASTLambda xs t)

pLet = do
  pToken $ string "let"
  vds <- many1 (try pVdef)
  pToken $ string "in"
  t <- pExpr
  return (ASTLet vds t)

pVdef = do
  x <- pVar
  pToken $ char '='
  t <- pExpr
  pToken $ char ';'
  return (ASTVdef x t)

pApp = ASTApp <$> many1 pExpr

-- Exercise 1.4
pIf0 = do
  pToken $ string "if0"
  b <- pExpr
  pToken $ string "then"
  t <- pExpr
  pToken $ string "else"
  s <- pExpr
  return (ASTIf0 b t s)

testEnv = Env EmptyEnv
  [ ("x", ValInt 999)
  , ("add"
    ,ValBinClosure (\[ValInt y,ValInt x] -> ValInt (x+y))
                   [] 2)
-- Exercise 1.1
  , ("sub"
    ,ValBinClosure (\[ValInt y,ValInt x] -> ValInt (x-y))
                   [] 2)
-- Exercise 1.2
  , ("signum"
    ,ValBinClosure (\[ValInt x] -> ValInt (signum x))
                   [] 1)
  ]

pExprWhole = between spaces eof pExpr
parseExp s = parse pExprWhole "" s

repl = do
  li <- getLine
  if li == ":quit" then return () else
    case parseExp li of
      Right ast -> print (stEval testEnv ast) >> repl
      Left err -> print err >> repl

main = repl

{-
Exercise 1.5

乗算
let mult = \x y -> if0 y then 0 else add x (mult x (sub y 1)); in mult 9 9

除算
let div = \x y -> let p = sub x y; q = add 1 (signum p); in if0 q then 0 else add 1 (div p y); in div 82 9

竹内関数
let t = \x y z -> if0 add 1 (signum (sub y x)) then t (t (sub x 1) y z) (t (sub y 1) z x) (t (sub z 1) x y) else y; in t 14 13 0
-}
