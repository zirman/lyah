module MyList where

infixr 5 :-:

data List a
  = Empty
  | (:-:) a
          (List a)
  deriving (Show, Read, Eq, Ord)

infixr 5 ^++

(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: xs ^++ ys

lmap :: (a -> b) -> List a -> List b
lmap _ Empty = Empty
lmap f (x :-: xs) = f x :-: lmap f xs

instance Functor List where
  fmap = lmap
