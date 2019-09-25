---------- p.105

data R12N = ...
          | RCase [(PAST,AST)] LZVal

---------- p.106 List 7.1

data Matcher = Matcher Assoc [PAST] [LZVal]
             | MatchFail

matchStep :: Matcher -> IO Matcher
matchStep m@(Matcher as (p:ps) (lv:lvs)) = do
  r <- deref lv
  case (p,r) of
    (PASTWildcard,_) -> return (Matcher as ps lvs)
    (PASTVar x, _) -> return (Matcher ((x,lv):as) ps lvs)
    (PASTInt a, RInt b) -> return $
       if a == b then (Matcher as ps lvs) else MatchFail
    (PASTConst n ps1, RADTClosure m 0 vs) -> return $
       if n == m
         then (Matcher as (ps1 ++ ps) (reverse vs ++ lvs))
         else MatchFail
    _ -> do
       b <- lzEval lv
       if b then return m else error "cannot reduce condition"

matchDone (Matcher _ ps _) = null ps
matchDone MatchFail = False

matcherEnv env (Matcher as _ _) = Env env as

---------- p.107

data R12N = ...
          | RCase [(PAST,AST)] Env LZVal Matcher AST --case式

-----

reload :: R12N -> R12N
reload (RCase ((p,t):alts) env v  MatchFail           _) =
        RCase        alts  env v (Matcher [] [p] [v]) t
reload r@(RCase _ _ _ (Matcher _ _ _) _) = r
reload (RCase [] _ _ _ _) = error "non exhaustive"

-----

s2r12n env (ASTCase t alts) = do
  v <- newIORef (RSource env t)
  let c0 = RCase alts env v MatchFail undefined
  let c1 = reload c0
  return c1

-----

--case rep of
    RCase _ _ _ _ _ -> lzEvalCase lv rep

lzEvalCase lv r@(RCase alts env v m t)
  | matchDone m = do
    writeIORef lv $ RSource (matcherEnv env m) t
    return True
  | otherwise = do
    let RCase alts1 env v m1 t1 = reload r
    m2 <- matchStep m1
    writeIORef lv $ RCase alts1 env v m2 t1
    return True

---------- p.108

data R12N = ...
          | RResolving Env Matcher

-----

--case rep of
    RResolving _ _ -> lzEvalResolve lv rep

matchfail MatchFail = True
matchfail _ = False

lzEvalResolve lv (RResolving env m) = do
  if matchfail m then error "irrefutable pattern"
  else if matchDone m then do
    let Matcher assoc _ _ = m
    forM_ assoc (\(x,lv) -> do
      let xv = getEnv x env
      writeIORef xv (RRef lv)
      )
    return True
  else do
    m1 <- matchStep m
    writeIORef lv (RResolving env m1)
    return True

---------- p.111 List 7.2

import Data.List (unzip)

vlist :: PAST -> [String]
vlist (PASTVar x) = [x]
vlist (PASTConst _ ps) = concatMap vlist ps
vlist _ = []

s2r12n env (ASTLet vds u) = do
  let vs = concat [vlist p | DASTVdef p _ <- vds]
  cells <- alloc (length vs)
  let env1 = Env env $ zip vs cells
  lvs <- sequence
         [newIORef (RSource env1 s) | DASTVdef _ s <- vds]
  sequence_
    [ writeIORef xlv rr
    | (DASTVdef p _, lv) <- zip vds lvs
    , let mr = Matcher [] [p] [lv]
    , x <- vlist p
    , let xlv = getEnv x env1
    , let rr = RResolving env1 mr
    ]
  return (RSource env1 u)

alloc n = sequence $ replicate n (newIORef undefined)

---------- p.112 List 7.3

compile ds parent = do
  let vs = concat [vlist p | DASTVdef p _ <- ds]
  let adtks = [ (n, RADTClosure n (length as) [])
              | DASTData tyname tyargs cons <- ds
              , (n, as) <- cons]
  cells1 <- alloc (length vs)
  cells2 <- sequence $ map (newIORef.snd) adtks
  let env = Env parent $
            zip vs cells1 ++ zip (map fst adtks) cells2
  lvs <- sequence [newIORef (RSource env s)
                  | DASTVdef _ s <- ds]
  sequence_
    [ writeIORef xlv rr
    | (DASTVdef p _, lv) <- zip ds lvs
    , let mr = Matcher [] [p] [lv]
    , x <- vlist p
    , let xlv = getEnv x env
    , let rr = RResolving env mr
    ]
  return env

---------- p.113 List 7.5

main = do
  let fn = "Prelude2.hc"
  cont <- readFile fn
  rootEnv <- mkRootEnv
  case parse pProgram fn cont of
    Left err -> print err
    Right prelu -> do
      env1 <- compile prelu rootEnv
      repl3 env1

repl3 env = do
  putStr "> "
  li <- getLine
  guard (li /= ":quit")
  case (parse pExprWhole "" li) of
    Right ast -> lzRun env ast >>= showLZV >> repl3 env
    Left err -> print err >> repl3 env
