{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (print)

-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
-- http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html

-- | This represents a symbolic reference to a variable in our DSL. It is what allows threading
-- several monadic statements with each other. In general, a DSL may have more than one type, but
-- for our purposes one will do. For now we will assume that each variable is internally represented
-- by a single integer, but other representations are possible (e.g. strings, memory addresses,
-- etc.).
newtype Var = Var Int

-- | Define a function for displaying the symbolic name of a variable.
instance Show Var where
  show (Var n) = "v" ++ show n

newtype BoolV = BoolV Int
instance Show BoolV where
  show (BoolV n) = "b" ++ show n

-- | This is the core definition of the syntax of our DSL, also encoding their return types. The
-- last argument of each non-terminal instruction represents the next instruction in the monadic
-- sequence. Note how instructions that introduce new variable in the scope accept a parameter of
-- type `Var -> next`, while others simply accept a `next`.
data OpF next
  = Def Int (Var -> next)
  | Add Var Var (Var -> next)
  | Print Var next
  | Eq Var Var (BoolV -> next)
  | End

-- | We create various convenience combinator lifting each of the syntax elements into the "free"
-- monadic context. Note that this is nothing more than a mechanical transformation; no behaviour or
-- meaning is associated to the instructions at this point. Note also the last argument to each
-- statement is either `id` or `()`; why?
def :: Int -> Op Var
def n = liftF $ Def n id

add :: Var -> Var -> Op Var
add v1 v2 = liftF $ Add v1 v2 id

print :: Var -> Op ()
print v = liftF $ Print v ()

eq :: Var -> Var -> Op BoolV
eq v1 v2 = liftF $ Eq v1 v2 id

end :: Op a
end = liftF $ End

-- | We define the behaviour of OpF as a Functor. Note that it is possible for Haskell to derive
-- this automatically using an extension.
instance Functor OpF where
  fmap f (Def n k) = Def n (f . k)
  fmap f (Add v1 v2 k) = Add v1 v2 (f . k)
  fmap f (Print v next) = Print v (f next)
  fmap f (Eq v1 v2 k) = Eq v1 v2 (f . k)
  fmap f End = End

-- | Shorthand notation.
type Op = Free OpF

-- | A sample program written in our new DSL.
test :: Op ()
test = do
  a <- def 111
  print a
  b <- def 222
  print b
  c <- add a b
  print c
  d <- def 333
  e <- eq c d
  print d

assign :: String
assign =
    "<-"

-- | Manual continuation.
pp1 :: Op a -> Int -> String

pp1 (Pure r) i =
    "return"

pp1 (Free op) i =
    case op of
        Def n k ->
            (line [show v, assign, show n]) ++ (pp1 (k v) (succ i))
          where
            v = Var i
        Add v1 v2 k ->
            (line [show v, assign, show v1, "+", show v2]) ++ (pp1 (k v) (succ i ))
          where
            v = Var i
        Print v k ->
            (line ["print", show v]) ++ (pp1 k i)
        Eq v1 v2 k ->
            (line [show v, assign, show v1, "==", show v2]) ++ (pp1 (k v) (succ i))
          where
            v = BoolV i
        End ->
            "end"

-- | Applicative style (needs to work for both StateT and State).
newVar :: (Monad a, Functor a) => StateT Int a Var
newVar =
    Var <$> get <* modify succ

{- Monadic style:
newVar = do
  i <- get
  modify succ
  return $ Var i
-}

newBoolV :: (Monad a, Functor a) => StateT Int a BoolV
newBoolV =
    BoolV <$> get <* modify succ


-- | State Monad only.
pp2 :: Op a -> State Int String

pp2 (Pure r) =
    return $ "return"

pp2 (Free op) = do
  case op of
      Def n k -> do
          v <- newVar
          next <- pp2 $ k v
          return $ (line [show v, assign, show n]) ++ next
      Add v1 v2 k -> do
          v <- newVar
          next <- pp2 $ k v
          return $ (line [show v, assign, show v1, "+", show v2]) ++ next
      Print v k -> do
          next <- pp2 $ k
          return $ (line ["print", show v]) ++ next
      Eq v1 v2 k -> do
          v <- newBoolV
          next <- pp2 $ k v
          return $ (line [show v, assign, show v1, "==", show v2]) ++ next


-- | Writer Monad only.
pp3 :: Op a -> Int -> Writer String ()

pp3 (Pure r) i =
    tell "return"

pp3 (Free op) i =
  case op of
      Def n k -> do
          let v = Var i
          tell $ line [show v, assign, show n]
          pp3 (k v) (succ i)
      Add v1 v2 k -> do
          let v = Var i
          tell $ line [show v, assign, show v1, "+", show v2]
          pp3 (k v) (succ i)
      Print v k -> do
          tell $ line ["print", show v]
          pp3 k i
      Eq v1 v2 k -> do
          let v = BoolV i
          tell $ line [show v, assign, show v1, "==", show v2]

line :: [String] -> String
line vs =
    (unwords vs) ++ "\n"


-- | Monad transformers (StateT + Writer).
pp4 :: Op a -> StateT Int (Writer String) ()

pp4 (Pure r) =
    tell "return"

pp4 (Free op) =
  case op of
      Def n k -> do
          v <- newVar
          tell $ line [show v, assign, show n]
          pp4 $ k v
      Add v1 v2 k -> do
          v <- newVar
          tell $ line [show v, assign, show v1, "+", show v2]
          pp4 $ k v
      Print v k -> do
          tell $ line ["print", show v]
          pp4 $ k
      Eq v1 v2 k -> do
           v <- newBoolV
           tell $ line [show v, assign, show v1, "==", show v2]
           pp4 $ k v


data RunState = RunState
  { _intVar :: [Int]
  , _boolVar :: [Bool]
  }


setVal :: Monad a => Int -> StateT RunState a Var
setVal v = do
           modify $ \state -> state { _intVar = (_intVar state) ++ [v] }
           vv <- gets _intVar
           return $ Var $ pred (length vv)


getVal :: Monad a => Var -> StateT RunState a Int
getVal (Var n) = do
                 vv <- gets _intVar
                 return $ vv !! n


initialRunState :: RunState
initialRunState = RunState { _intVar = [] , _boolVar = [] }


runOp :: Op a -> StateT RunState IO a

runOp (Pure k) =
    return k

runOp (Free op) =
    case op of
        Def n k -> do
            v <- setVal n
            runOp (k v)
        Add v1 v2 k -> do
            n1 <- getVal v1
            n2 <- getVal v2
            v <- setVal (n1 + n2)
            runOp (k v)
        Print v k -> do
            val <- getVal v
            (liftIO . putStrLn) (show val)
            runOp k
        Eq (Var n1) (Var n2) k -> do
            runOp (k $ BoolV 1)
        -- End -> return ()


main :: IO ()
main = do

    let listing1 = pp1 test 0
    putStr $ "pp1: {\n" ++ listing1 ++ "}\n\n"

    let listing2 = evalState (pp2 test) 0
    putStr $ "pp2: {\n" ++ listing2 ++ "}\n\n"

    let listing3 = execWriter (pp3 test 0)
    putStr $ "pp3: {\n" ++ listing3 ++ "}\n\n"

    let listing4 = execWriter (evalStateT (pp4 test) 0)
    putStr $ "pp4: {\n" ++ listing4 ++ "}\n\n"

    putStr $ "run: {\n"
    runStateT (runOp test) initialRunState
    putStr $ "}\n\n"
