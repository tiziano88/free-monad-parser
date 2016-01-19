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

-- | This is the core definition of the syntax of our DSL, also encoding their return types. The
-- last argument of each non-terminal instruction represents the next instruction in the monadic
-- sequence. Note how instructions that introduce new variable in the scope accept a parameter of
-- type `Var -> next`, while others simply accept a `next`.
data OpF next
  = Def Int (Var -> next)
  | Add Var Var (Var -> next)
  | Print Var next
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

end :: Op a
end = liftF $ End

-- | We define the behaviour of OpF as a Functor. Note that it is possible for Haskell to derive
-- this automatically using an extension.
instance Functor OpF where
  fmap f (Def n k) = Def n (f . k)
  fmap f (Add v1 v2 k) = Add v1 v2 (f . k)
  fmap f (Print v next) = Print v (f next)
  fmap f End = End

-- | Shorthand notation.
type Op = Free OpF

-- | A sample program written in our new DSL.
test :: Op ()
test = do
  a <- def 123
  b <- def 234
  c <- add a b
  print c

-- | Manual continuation.
pp1 :: Int -> Op a -> String
pp1 i (Pure r) = "return\n"
pp1 i (Free (Def n k)) = unwords [show v, "=", show n, "\n", pp1 (succ i) (k v)]
  where v = Var i
pp1 i (Free (Add v1 v2 k)) = unwords [show v, "=", show v1, "+", show v2, "\n", pp1 (succ i) (k v)]
  where v = Var i
pp1 i (Free (Print v k))  = unwords ["print", show v, "\n", pp1 i k]
pp1 i (Free (End)) = "end"

-- | Applicative style.
newVar :: State Int Var
newVar = Var <$> get <* modify succ

{- Monadic style:
newVar = do
  i <- get
  modify succ
  return $ Var i
-}

-- | State Monad.
pp2 :: Op a -> State Int String
pp2 (Pure r) = return $ "return"
pp2 (Free op) = do
  case op of (Def n k) -> do
                          v <- newVar
                          next <- pp2 $ k v
                          return $ unwords [show v, "=", show n, "\n", next]
             (Add v1 v2 k) -> do
                              v <- newVar
                              next <- pp2 $ k v
                              return $ unwords [show v, "=", show v1, "+", show v2, "\n", next]
             (Print v k) -> do
                            next <- pp2 $ k
                            return $ unwords ["print", show v, "\n", next]

-- | TODO: Monad transformers (State + Writer).
{-
pp3 :: Op a -> WriterT String (State Int) ()
pp3 = do
  return ()
-}

runOp :: Op a -> IO a
runOp (Pure k) = return k
runOp (Free (Def n k)) = runOp (k v)
  where v = Var n
runOp (Free (Add (Var n1) (Var n2) k)) = runOp (k v)
  where v = Var (n1 + n2)
runOp (Free (Print (Var n) k)) = putStrLn (show n) >> runOp k
--runOp (Free (End)) = return ()

main :: IO ()
main = do

  let listing1 = pp1 0 test
  putStr $ "listing1: {\n" ++ listing1 ++ "}\n\n"

  let (listing2, _) = runState (pp2 test) 0
  putStr $ "listing2: {\n" ++ listing2 ++ "}\n\n"

  putStr $ "output: {\n"
  _ <- runOp test
  putStr $ "}\n\n"
