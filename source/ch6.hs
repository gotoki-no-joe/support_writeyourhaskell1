---------- p.080

data Tree = Node Tree Tree | Leaf Integer deriving Show

incr :: Tree -> Tree
incr (Node a b) = Node (incr a) b
incr (Leaf n) = Leaf (succ n)

t1 = Node (Leaf 1) (Leaf 2)
t2 = let x = Leaf 3 in Node x x

---------- p.081

data GNode = Gode Int Int | Geaf Integer deriving Show
type Graph = [GNode]

g :: Graph
g = [ Gode 1 2, Geaf 1, Geaf 2   -- 0 = t1
    , Gode 4 4, Geaf 3]          -- 3 = t2

incrg :: Int -> Graph -> Graph
incrg i g = case g !! i of
  Gode a _ -> incrg a g
  Geaf n   -> take i g ++ Geaf (succ n) : drop (i+1) g

-----

import Data.IORef

type RTree = IORef RNode

data RNode = RNode RTree RTree | RLeaf Integer

-----

makeR1 = do
  a <- newIORef (RLeaf 1)
  b <- newIORef (RLeaf 2)
  newIORef (RNode a b)

makeR2 = do
  a <- newIORef (RLeaf 3)
  newIORef (RNode a a)

---------- p.082

incrr :: RTree -> IO ()
incrr r = do
  x <- readIORef r
  case x of
    RNode a _ -> incrr a
    RLeaf n -> writeIORef r (RLeaf (succ n))

-----

test = do
  r1 <- makeR1
  display r1 >> putStrLn ""
  incrr r1
  display r1 >> putStrLn ""
  r2 <- makeR2
  display r2 >> putStrLn ""
  incrr r2
  display r2 >> putStrLn ""

display :: RTree -> IO ()
display r = do
  x <- readIORef r
  case x of
    RLeaf n -> putStr ("RLeaf " ++ show n)
    RNode a b -> putStr "RNode (" >> display a >>
                 putStr ") (" >> display b >> putStr ")"

---------- p.083

type LZVal = IORef R12N

type Assoc = [(String, LZVal)]

data Env = Env Env Assoc
         | EmptyEnv

getEnv :: String -> Env -> LZVal -- 実装は同一なので省略

data R12N = RInt Int                                 -- 整数値
          | RLamClosure AST Env Assoc [String]       -- ラムダ
          | RBinClosure ([R12N] -> R12N) [LZVal] Int -- 組込
          | RADTClosure String Int [LZVal]           -- 構成子
          | RVarRef Env String                   -- 変数参照
          | RApp [LZVal]                         -- 関数適用
          | RSource Env AST                      -- 未展開の式

---------- p.084

lzEval :: LZVal -> IO Bool
lzEval lv = do
  rep <- readIORef lv
  case rep of
    ...

-----

--case rep of
    RInt _ -> return False
    RLamClosure _ _ _ _ -> return False
    RBinClosure _ _ _   -> return False
    RADTClosure _ _ _   -> return False

---------- p.085

--case rep of
    RBinClosure _ _ 0 -> lzEvalBinClosed lv rep

lzEvalBinClosed lv (RBinClosure f lvs _) = do
    b <- loop lvs
    if b then return True else do
      rs <- sequence (map readIORef lvs)
      writeIORef lv (f rs)
      return True
  where
    loop [] = return False
    loop (lv:lvs) = do
      b <- lzEval lv
      if b then return True else loop lvs

-----

--case rep of
    RLamClosure t env assoc [] -> do
      writeIORef lv (RSource (Env env assoc) t)
      return True

---------- p.086

--case rep of
    RApp lvs -> lzEvalApp lv lvs

lzEvalApp lv (lv0:lv1:lvs) = do
  r0 <- readIORef lv0
  if not (isClosure r0) then do
    b <- lzEval lv0
    return $! b || error "can't reduce to closure"
  else if saturated r0 then lzEval lv0 else do
    lv0' <- newIORef (apply r0 lv1)
    writeIORef lv (RApp (lv0':lvs))
    return True

isClosure (RLamClosure _ _ _ _) = True
isClosure (RBinClosure _ _ _) = True
isClosure (RADTClosure _ _ _) = True
isClosure _ = False

saturated (RLamClosure _ _ _ xs) = null xs
saturated (RBinClosure _ _ k) = k == 0
saturated (RADTClosure _ k _) = k == 0

apply (RLamClosure t env         as (x:xs)) lv =
       RLamClosure t env ((x,lv):as)   xs
apply (RBinClosure f  lvs  k)  lv    =
       RBinClosure f (lv:lvs) (k-1)
apply (RADTClosure n  k        lvs) lv =
       RADTClosure n (k-1) (lv:lvs)

-----

--case rep of
    RSource env t -> do r <- s2r12n env t
                        writeIORef lv r
                        return True

s2r12n _   (ASTInt n)       = return (RInt n)
s2r12n env (ASTVarRef x)    = return (RVarRef env x)
s2r12n env (ASTConst x)     = return (RVarRef env x)
s2r12n env (ASTApp ts)      =
  RApp <$> (sequence $ map (newIORef.RSource env) ts)
s2r12n env (ASTLambda xs u) =
  return $ RLamClosure u env [] xs
s2r12n env (ASTLet vds u)   = do
  cells <- sequence $
           replicate (length vds) (newIORef undefined)
  let env1 = Env env $
       [(x,c) | (DASTVdef (PASTVar x) _, c) <- zip vds cells]
  sequence_ [ writeIORef c (RSource env1 s)
            | (c,DASTVdef _ s) <- zip cells vds]
  return (RSource env1 u)

---------- p.088

data R12N = ..
          | RRef LZVal

---------- p.089

deref :: LZVal -> IO R12N
deref lv = do
  rep <- readIORef lv
  case rep of
    RRef lv1 -> deref lv1
    _        -> return rep

-----

--case rep of
    RRef lv1 -> lzEval lv1

-----

-- 変数参照
--case rep of
    RVarRef env x -> do let lv1 = getEnv x env
                        writeIORef lv (RRef lv1)
                        return True
-- 関数適用
lzEvalApp lv (lv0:[]) = do
  writeIORef lv (RRef lv0)
  return True

-----

compile :: H2Prog -> Env -> IO Env
compile ds parent = do
  let assoc = [(x,t) | DASTVdef (PASTVar x) t <- ds]
  let adtks = [ (n, RADTClosure n (length as) [])
              | DASTData tyname tyargs cons <- ds
              , (n, as) <- cons]
  cells <- sequence $
           replicate (length assoc + length adtks)
                     (newIORef undefined)
  let (cells1,cells2) = splitAt (length assoc) cells
  let env = Env parent $
            zip (map fst assoc ++ map fst adtks) cells
  sequence_ [ writeIORef c (RSource env t)
            | (c,t) <- zip cells1 (map snd assoc)]
  sequence_ [ writeIORef c t
            | (c,t) <- zip cells2 (map snd adtks)]
  return env

---------- p.090

lzRun env t = do
  r <- newIORef (RSource env t)
  lzRunLoop r

lzRunLoop r = do
  b <- lzEval r
  if b then lzRunLoop r else return r

-----

showLZV :: LZVal -> IO ()
showLZV lv = showLZV' [] lv >> return ()

showLZV' lvd lv
  | elem lv lvd = putStrLn "(**)" >> return (lv:lvd)
  | otherwise = do
    r <- deref lv
    case r of
      RInt n -> print n >> return (lv:lvd)
      RBinClosure _ lvs k -> do
        putStr ("(RBinClosure " ++ show k ++ " ")
        showLZVs (lv:lvd) lvs
      RADTClosure n 0 lvs -> do
        putStr ("(" ++ n ++ " ")
        showLZVs (lv:lvd) (reverse lvs)
      RSource _ t -> do
        putStr ("{" ++ showAST t ++ "}")
        return (lv:lvd)
      _ -> putStrLn "**TODO**" >> return (lv:lvd)

showLZVs lvd lvs = do
  lvd1 <- foldM showLZV' lvd lvs
  putStrLn ")"
  return lvd1

---------- p.091

import Debug.Trace (trace)

mkRootEnv :: IO Env
mkRootEnv = do
  x <- newIORef $ RInt 999
  add <- newIORef $ RBinClosure addFunc [] 2
  sub <- newIORef $ RBinClosure subFunc [] 2
  true <- newIORef $ RADTClosure "True" 0 []
  fals <- newIORef $ RADTClosure "False" 0 []
  return $ Env EmptyEnv [ ("x",x),("add", add), ("sub", sub)
                        , ("True", true), ("False", fals)]

addFunc [RInt y, RInt x] =
  trace (unwords ["add", show x, show y])
        (RInt (x+y))

subFunc [RInt y, RInt x] =
  trace (unwords ["sub", show x, show y])
        (RInt (x-y))

---------- p.092

test4 exp = do
  e0 <- mkRootEnv
  let Right ds = parse pProgram "" theSource
  e1 <- compile ds e0
  let Right mein = parse pExprWhole "" exp
  v <- lzRun e1 mein
  showLZV v

theSource = "data List a = Nil | Cons a (List a);" ++
            "zero = \\x -> sub x x;"
