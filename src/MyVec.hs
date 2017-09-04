module MyVec where

data Vector a = Vector a a a deriving Show

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector x y z) = i*x + j*y + k*z

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) s = Vector (i*s) (j*s) (k*s)
