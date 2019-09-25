---------- p.054 List 5.1

data AST = ...
         | ASTLet [DAST] AST          -- 局所定義（更新）
         | ...
         | ASTConst String            -- 値構成子
         | ASTCase AST [(PAST,AST)]   -- Case式

-- 型 (type)
data TAST = TASTName String           -- 型名
          | TASTVar String            -- 型変数
          | TASTApp [TAST]            -- 型関数の適用

-- パターン (pattern)
data PAST = PASTInt Int               -- 整数リテラル
          | PASTWildcard              -- ワイルドカード
          | PASTVar String            -- パターン変数
          | PASTConst String [PAST]   -- 値構成子

-- プログラム行 (declaration)
data DAST = DASTVdef PAST AST         -- 変数定義
          | DASTData String [String]  -- データ型宣言
                     [(String,[TAST])]

---------- p.055

data Val = ...
         | ValADTClosure String Int [Val]         -- 値構成子

instance Show Val where
  ...
  show (ValADTClosure k n vs) =
    unwords (k : map (kakko.show) vs)

kakko s = '(' : s ++ ")"

-----

-- stEval (ASTApp ...
apply (ValADTClosure k 0 _ ) _ =
  error ("too many arg to constructor " ++ k)
apply (ValADTClosure k n vs) v = ValADTClosure k (n-1) (v:vs)

---------- p.056

stEval env (ASTConst k) = getEnv k env

---------- p.057 List 5.2

type H2Prog = [DAST]

compile :: H2Prog -> Env -> Env
compile ds parent
  | isJust assoc = env1
  | otherwise = error "irrefutable pattern"
  where
    assoc = fmap concat $ sequence
      [match p (stEval env1 t) | DASTVdef p t <- ds]
    adtks =
      [ (kname, ValADTClosure kname (length as) [])
      | DASTData tyname tyargs cons <- ds
      , (kname, as) <- cons]
    env1 = Env parent $ fromJust assoc ++ adtks

----- List 5.3

stEval env (ASTLet pts t)
  | isJust assoc = stEval env1 t
  | otherwise    = error "irrefutable pattern"
  where
    assoc = fmap concat $ sequence
            [ match p (stEval env1 t) | DASTVdef p t <- pts ]
    env1 = Env env $ fromJust assoc

---------- p.058 List 5.4

stEval env (ASTCase e as) =
  case ms of
    (a,b):_ -> stEval (Env env a) b
    []      -> error "Nonexhaustive patterns"
  where
    v = stEval env e
    ms = [ (fromJust r,e)
         | (p,e) <- as, let r = match p v, isJust r]

-----

match :: PAST -> Val -> Maybe Assoc
match PASTWildcard _ = Just []
match (PASTInt n) (ValInt m) =
  if n == m then Just [] else Nothing
match (PASTVar x) v = Just [(x,v)]
match (PASTConst k ps) (ValADTClosure j 0 vs)
  | k == j && length ps == length vs =
    fmap concat $ sequence $ zipWith match ps $ reverse vs
match _ _ = Nothing

---------- p.059

pKakko :: Parser a -> Parser a
pKakko p = between (pToken $ char '(') (pToken $ char ')') p

pParen = pKakko pExpr

-----

oneWrap :: ([a] -> a) -> [a] -> a
oneWrap _ [x] = x
oneWrap f xs  = f xs

-----

pType :: Parser String
pType = do
  c  <- upper
  cs <- many (alphaNum <|> char '_')
  spaces
  return (c:cs)

-----

pCType = choice
  [ TASTVar <$> try pVar
  , TASTName <$> try pType
  , try $ pKakko pTYPE]

pTYPE = oneWrap TASTApp <$> many1 pCType

---------- p.060

pConstDecl = do
  k <- pType
  as <- many (try pCType)
  return (k, as)

-----

pDASTData = do
  pToken $ string "data"
  typename <- pType
  typeargs <- many pVar
  pToken $ char '='
  ks <- sepBy1 pConstDecl (pToken $ char '|')
  pToken $ char ';'
  return (DASTData typename typeargs ks)

-----

pConst = ASTConst <$> pType

-----

pPattern = do
  p1 <- pCPattern
  case p1 of
    PASTConst n as -> do
      ps <- many pPattern
      if length ps > 0 then return (PASTConst n ps)
                       else return p1
    _ -> return p1

pCPattern = choice
  [pPInt, pPWildcard, pPVar, pPConst, pKakko pPattern]

pPInt = PASTInt <$> pInt
pPWildcard = pToken (char '_') >> return PASTWildcard
pPVar = PASTVar <$> pVar
pPConst = pType >>= \n -> return (PASTConst n [])

---------- p.061

pDASTVdef = do
  p <- pPattern
  pToken $ char '='
  t <- pExpr
  pToken $ char ';'
  return (DASTVdef p t)

-----

pCase = do
  pToken $ string "case"
  e <- pExpr
  pToken $ string "of"
  pToken $ char '{'
  alts <- many1 (try pCaseAlt)
  pToken $ char '}'
  return (ASTCase e alts)

pCaseAlt = do
  p <- pPattern
  pToken $ string "->"
  e <- pExpr
  pToken $ char ';'
  return (p,e)

-----

pProgram :: Parser H2Prog
pProgram = between spaces eof
                   (many1 (try pDASTData <|> try pDASTVdef))

---------- p.062

main = do
  let fn = "Prelude.hc"
  cont <- readFile fn
  case parse pProgram fn cont of
    Left err -> print err
    Right prelu -> do
      let env1 = compile prelu rootEnv
      repl2 env1

repl2 env = do
  putStr "> "
  li <- getLine
  if li == ":quit" then return () else
    case testexp li of
      Right ast -> print (stEval env ast) >> repl2 env
      Left err -> print err >> repl2 env

rootEnv = Env EmptyEnv
  [ ("x", ValInt 999)
  , ("add", ValBinClosure
      (\[ValInt y,ValInt x] -> ValInt (x+y)) [] 2)
  , ("sub", ValBinClosure
      (\[ValInt y,ValInt x] -> ValInt (x-y)) [] 2)
  , ("signum", ValBinClosure
      (\[ValInt x] -> ValInt (signum x)) [] 1)
  , ("True" ,ValADTClosure "True"  0 [])
  , ("False",ValADTClosure "False" 0 []) ]
