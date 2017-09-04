module Ch14 where
import System.Random
import Control.Monad
import GHC.Base

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
(v, s) `applyLog` f =
  let (r, s') = f v
  in (r, s ++ s')

newtype Writer w a = Writer
  { runWriter :: (a, w)
  }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w =>
         Applicative (Writer w) where
  pure f = Writer (f, mempty)
  Writer (f, fw) <*> Writer (v, vw) = Writer (f v, fw `mappend` vw)

instance Monoid w =>
         Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, w)) >>= f =
    let Writer (x', w') = f x
    in Writer (x', w `mappend` w')

writer
  :: Monoid w
  => a -> Writer w a
writer v = Writer (v, mempty)

tell
  :: Monoid w
  => w -> Writer w ()
tell w = Writer ((), w)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList dxs = getDiffList dxs []

instance Monoid (DiffList a) where
  mempty = toDiffList []
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

multWithLog' :: Writer [String] Int
multWithLog' =
  logNumber 3 >>=
  \a ->
     logNumber 5 >>=
     \b -> tell ["Gonna multiply these two"] >>= \_ -> return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
  | b == 0 = do
    tell $ toDiffList ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd'' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
  | b == 0 = do
    tell $ toDiffList ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse' b (a `mod` b)
    tell $ toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

--class Joinable a where
--  join :: Joinable (Joinable a) -> Joinable a
--flatMap f m =
-- Monad operation: join = Monad<Monad<T>> -> Monad<T>
-- flatMap can be defined using join: flatMap f m = join (fmap f m)

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

addStuff :: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  c <- (4-)
  return (a+b+c)

addStuff2 :: Int -> Int
addStuff2 =
  (*2) >>= (\a -> (+10) >>= (\b -> (4-) >>= (\c -> return (a + b + c))))

--threeCoins :: StdGen -> (Bool, Bool, Bool)
--threeCoins gen =
--  let (firstCoin, newGen) = random gen
--      (secondCoin, newGen') = random newGen
--      (thirdCoin, newGen'') = random newGen'
--  in  (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State h) = State $ \s ->
    let (x, s') = h s
    in (f x, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State af) <*> (State av) = State $ \s ->
    let (f, s') = af s
        (v, s'') = av s'
    in (f v, s'')

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f = State $ \s ->
    let (a, newState) = h s
        (State g) = f a
    in g newState

pop' :: State Stack Int
pop' = State $ \(x:xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = State $ \xs -> ((), a:xs)

get :: State Stack Stack
get = State $ \s -> (s, s)

put :: Stack -> State Stack ()
put s = State $ const ((), s)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  a <- pop'
  pop'

stackManip'' :: State Stack Int
stackManip'' = do
  push' 3
  pop'
  pop'

stackStuff :: State Stack ()
stackStuff = do
  a <- pop'
  if a == 5
    then push' 5
    else do
      push' 3
      push' 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip''
  when (a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)
