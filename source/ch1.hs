---------- p.003 List 1.1

data AST = ASTLambda [String] AST    -- ラムダ抽象
         | ASTLet [ASTVdef] AST      -- 局所定義
         | ASTVarRef String          -- 変数参照
         | ASTInt Int                -- 整数リテラル
         | ASTApp [AST]              -- 関数適用
  deriving Show

data ASTVdef = ASTVdef String AST
  deriving Show

---------- p.005 List 1.2

-- 1. 123
example1 = ASTInt 123

-- 2. x
example2 = ASTVarRef "x"

-- 3. add x 1
example3 = ASTApp [ASTVarRef "add", ASTVarRef "x", ASTInt 1]

-- 4. let y = 1; in add x y
example4 = ASTLet [ASTVdef "y" (ASTInt 1)]
                  (ASTApp [ ASTVarRef "add"
                          , ASTVarRef "x"
                          , ASTVarRef "y"])

-- 5. let x = 456; in x
example5 = ASTLet [ASTVdef "x" (ASTInt 456)] (ASTVarRef "x")

-- 6. (\ x -> add x x) 3
example6 =
  ASTApp [ ASTLambda ["x"] (ASTApp [ ASTVarRef "add"
                                   , ASTVarRef "x"
                                   , ASTVarRef "x"])
         , ASTInt 3]

-- 7. let s = add 1; in add (s 2) (s 3)
example7 =
  ASTLet [ASTVdef "s" (ASTApp [ASTVarRef "add", ASTInt 1])]
         (ASTApp [ ASTVarRef "add"
                 , ASTApp [ASTVarRef "s", ASTInt 2]
                 , ASTApp [ASTVarRef "s", ASTInt 3] ])

---------- p.006 List 1.3

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

---------- p.007

stEval :: Env -> AST -> Val

-----

stEval _ (ASTInt n) = ValInt n

-----

stEval env (ASTVarRef x) = getEnv x env

---------- p.008 List 1.4

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

-----

getEnv :: String -> Env -> Val
getEnv x EmptyEnv = error $ x ++ " is unbound variable"
getEnv x (Env p assoc) = case (lookup x assoc) of
                           Just v -> v
                           Nothing -> getEnv x p

-----

import Data.Maybe (fromMaybe)

getEnv x (Env p assoc) =
  fromMaybe (getEnv x p) (lookup x assoc)

---------- p.009

stEval env (ASTLet vds t) = stEval env1 t
  where assoc = [(x, stEval env t) | ASTVdef x t <- vds]
        env1  = Env env assoc

-----

stEval env (ASTLambda xs t) = ValLamClosure t env [] xs

-----

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

-----

testEnv = Env EmptyEnv
  [ ("x", ValInt 999)
  , ("add"
    ,ValBinClosure (\[ValInt y,ValInt x] -> ValInt (x+y))
                   [] 2)
  ]

---------- p.011

stEval env (ASTLet vds t) = stEval env1 t
  where assoc = [(x, stEval env1 t) | ASTVdef x t <- vds]
        env1  = Env env assoc

---------- p.012

import Text.Parsec

type Parser a = Parsec String () a

---------- p.013

pInt :: Parser Int
pInt = do ds <- many1 digit; spaces; return (read ds)

-----

pVar :: Parser String
pVar = do
  c  <- lower
  cs <- many (alphaNum <|> char '_')
  let x = c:cs
  spaces
  return x

-----

pExpr :: Parser AST
pExpr = choice [pLambda,pLet,pParen,pExprVar,pExprInt,pApp]

---------- p.014

pExprVar = do x <- pVar; return (ASTVarRef x)

-----

pExprVar = ASTVarRef <$> pVar

pExprInt = ASTInt <$> pInt

-----

pToken :: Parser a -> Parser ()
pToken p = p >> spaces

pParen = between (pToken $ char '(')
                 (pToken $ char ')')
                 pExpr

-----

pLambda = do
  pToken $ char '\\'
  xs <- many1 pVar
  pToken $ string "->"
  t <- pExpr
  return (ASTLambda xs t)

pLet = do
  pToken $ string "let"
  vds <- many1 pVdef
  pToken $ string "in"
  t <- pExpr
  return (ASTLet vds t)

pVdef = do
  x <- pVar
  pToken $ char '='
  t <- pExpr
  pToken $ char ';'
  return (ASTVdef x t)

---------- p.015

pApp = ASTApp <$> many1 pExpr

-----

testexp s = parse pExpr "" s
example11 = testexp "123"
example12 = testexp "x"
example13 = testexp "add x 1"
example14 = testexp "let y = 1; in add x y"
example15 = testexp "let x = 456; in x"
example16 = testexp "(\\ x -> add x x) 3"
example17 = testexp "let s = add 1; in add (s 2) (s 3)"
example18 = testexp
  "let y = 1; in let f = \\x -> y; in let y = 3; in f 456"

---------- p.016

pExpr = choice [pApp,pLambda,pLet,pParen,pExprVar,pExprInt]

---------- p.017

pExpr = do
  es <- many1 $ choice $
        map try [pLambda, pLet, pParen, pExprVar, pExprInt]
  case es of
    [e] -> return e
    _   -> return (ASTApp es)

---------- p.018

pExprWhole = between spaces eof pExpr
parseExp s = parse pExprWhole "" s

repl = do
  li <- getLine
  if li == ":quit" then return () else
    case parseExp li of
      Right ast -> print (stEval testEnv ast) >> repl
      Left err -> print err >> repl
