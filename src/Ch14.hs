module Ch14 where

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
(v, s) `applyLog` f =
  let (r, s') = f v
  in (r, s ++ s')

newtype Writer β α = Writer
  { runWriter :: (α, β)
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
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])
