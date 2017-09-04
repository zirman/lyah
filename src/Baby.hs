module Baby where

import Data.List (group, nub, sort, isPrefixOf, tails, find)
import Data.Char (ord, chr, digitToInt, isDigit)
--import qualified Data.Map as Map

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' :: (Num a, Ord a) => a -> a
doubleSmallNumber' x =
  (if x > 100
     then x
     else x * 2) +
  1

conanO'Brien = "It's a-me, Conan O'Brien!"

lucky
  :: (Integral a)
  => a -> String
lucky 7 = "LUCKY NUMBER SEVER!"
lucky x = "Sorry, you're out of luck, pal!"

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

addVectors
  :: (Num a)
  => (a, a) -> (a, a) -> (a, a)
addVectors x y = (fst x + fst y, snd x + snd y)

addVectors'
  :: (Num a)
  => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

head' :: [a] -> a
head' (x:_) = x

-- type List = Nil + Cons (x, List)
--
-- head'' :: Cons (a, List) -> a
-- head'' (Cons (x, _)) = x
tell
  :: (Show a)
  => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) =
  "This list is long. The first two elements are: " ++
  show x ++ " and " ++ show y

length'
  :: (Num b)
  => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise = "You're a whale, congratulations!"
-- bmiTell2 :: (RealFloat a) => a -> a -> String
-- bmiTell2 weight height
--     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--     | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise = "You're a whale, congratulations!"
bmiTell
  :: (RealFloat a)
  => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

max'
  :: (Ord a)
  => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare
  :: (Ord a)
  => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

asdf [x]
  | x > 0 = 1
asdf [x, y]
  | y > 0 = 2

calcBmis
  :: (RealFloat a)
  => [(a, a)] -> [a]
calcBmis xs =
  [ bmi w h
  | (w, h) <- xs ]
  where
    bmi weight height = weight / height ^ 2

cylinder
  :: (RealFloat a)
  => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "maximum of empty list"
-- maximum' [x] = x
-- maximum' (x:xs)
--     | x > maxTail = x
--     | otherwise = maxTail
--     where maxTail = maximum' xs
replicate'
  :: (Num i, Ord i)
  => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take'
  :: (Num i, Ord i)
  => i -> [a] -> [a]
take' n x
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]
--
-- reverse'' :: [a] -> [a]
-- reverse'' = foldl (flip (:)) []
repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- elem' :: (Eq a) => a -> [a] -> Bool
-- elem' _ [] = False
-- elem' x (y:ys)
--     | x == y = True
--     | otherwise = elem' x ys
quicksort
  :: (Ord a)
  => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
  quicksort
    [ x
    | x <- xs 
    , x < pivot ] ++
  pivot :
  quicksort
    [ x
    | x <- xs 
    , x >= pivot ]

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | otherwise = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter p (map chain [1 .. 100]))
  where
    p c = length (take 16 c) == 16

sum'
  :: (Num a)
  => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- works on infinite lists
elem'
  :: (Eq a)
  => a -> [a] -> Bool
elem' y = foldr' (\ x acc -> (x == y) || acc) False

-- does not work on infinite lists
elem''
  :: (Eq a)
  => a -> [a] -> Bool
elem'' y = foldl (\ acc x -> (x == y) || acc) False

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x $ foldr' f acc xs

maximum'
  :: (Ord a)
  => [a] -> a
maximum' = foldr1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product'
  :: (Num a)
  => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
  foldr
    (\x acc ->
        if f x
          then x : acc
          else acc)
    []

last' :: [a] -> a
last' = foldr1 (\x acc -> acc)

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = (+ 1) . length . takeWhile (< 1000) . scanl1 (+) $ map sqrt [1 ..]

fn x = ceiling (negate (tan (cos (max 50 x))))

fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (< 10000) . filter odd $ map (^ 2) [1 ..]

numUniqes
  :: (Eq a)
  => [a] -> Int
numUniqes = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\x -> (head x, length x)) . group . sort . words

isIn
  :: (Eq a)
  => [a] -> [a] -> Bool
needle `isIn` haystack = any (isPrefixOf needle) $ tails haystack

encode :: Int -> String -> String
encode n = map (chr . (+ n) . ord)

decode :: Int -> String -> String
decode n = encode $ negate n

firstTo :: Int -> Maybe Int
firstTo x = find ((== x) . sum . map digitToInt . show) [1 ..]

--findKey
--  :: (Eq a)
--  => a -> [(a, b)] -> Maybe b
-- findKey _ [] = Nothing
-- findKey key ((k, v) : xs)
--     key == k = Just v
--   | otherwise = findKey key xs
--findKey key = foldl f Nothing

--phoneBook :: Map.Map String String
--phoneBook =
--  Map.fromList
--  [("betty", "555-2938"), ("bonnie", "452-2928"),
--   ("patsy", "493-2928"), ("lucille", "205-2928"),
--   ("wendy", "939-8282"), ("penny", "853-2492")]

stringToDigits :: String -> [Int]
stringToDigits = map digitToInt . filter isDigit
