module Main where

--sequencA :: (Applicative f) => [f a] -> f [a]
--sequencA [] = pure []
--sequencA (x:xs) = (:) <$> x <*> sequencA xs

--instance Applicative ((->) r) where
--  pure x _ = x
--  f <*> g = \x -> f x (g x)

--import Data.String.Strip

--instance Functor IO where
--fmap f action = do
--  x <- action
--  Functor $ f x

--instance Functor ((->) r) where
--    fmap f g = (\x -> f (g x))

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

newtype Any = Any { getAny :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
  mempty = Any True
  Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

newtype MyOrdering = MyOrdering { getOrdering :: Ordering }

instance Monoid MyOrdering where
  mempty = MyOrdering EQ
  MyOrdering LT `mappend` _ = MyOrdering LT
  MyOrdering EQ `mappend` y = y
  MyOrdering GT `mappend` _ = MyOrdering GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`
                     (x `compare` y)

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")

newtype MyMaybe a = MyMaybe { getMaybe :: Maybe a }

instance Monoid a => Monoid (MyMaybe a) where
  mempty = MyMaybe Nothing
  MyMaybe Nothing `mappend` x = x
  x `mappend` MyMaybe Nothing = x
  MyMaybe (Just x) `mappend` MyMaybe (Just y) = MyMaybe . Just $ x `mappend` y

newtype First a = First { getFirst :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First (Just x)
  First Nothing `mappend` x = x

newtype Last a = Last { getLast :: Maybe a }

instance Monoid (Last a) where
  mempty = Last Nothing
  _ `mappend` Last (Just x) = Last (Just x)
  x `mappend` Last Nothing = x

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Foldable Tree where
  foldMap _ EmptyTree = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x         `mappend`
                           foldMap f r

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

instance Functor MyMaybe where
  fmap _ (MyMaybe Nothing) = MyMaybe Nothing
  fmap f (MyMaybe (Just x)) = MyMaybe (Just (f x))

instance Applicative MyMaybe where
  pure x = MyMaybe (Just x)
  MyMaybe Nothing <*> _ = MyMaybe Nothing
  _ <*> MyMaybe Nothing = MyMaybe Nothing
  MyMaybe (Just f) <*> MyMaybe (Just x) = MyMaybe (Just (f x))

instance Monad MyMaybe where
  MyMaybe Nothing >>= _ = MyMaybe Nothing
  MyMaybe (Just x) >>= f = f x

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (left, right) = testPole (left + b, right)

landRight :: Birds -> Pole -> Maybe Pole
landRight b (left, right) = testPole (left, right + b)

testPole :: Pole -> Maybe Pole
testPole (left, right)
  | left >= 0 && right >= 0 && abs (left - right) < 4 = Just (left, right)
  | otherwise = Nothing

(-:) :: a -> (a -> b) -> b
x -: f = f x

main :: IO ()
main = fmap reverse getLine >>=
  \line -> putStrLn ("You said " ++ line ++ " backwards!") >>=
  \_ -> putStrLn ("Yes, you really said " ++ line ++ " backwards!")

--main = do
--  line <- fmap reverse getLine
--  putStrLn $ "You said " ++ line ++ " backwards!"
--  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

--main = do
--  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
--  putStrLn line

--interact strip

--main = do
--  return $ putStrLn <*> "hello"
--  return b
--  a <- pure (++) <*> getLine <*> getLine

--  return ()
--  putStrLn $ "The two lines concatenate turn out to be: " ++ a
