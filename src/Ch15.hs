module Ch15 where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Direction = L | R deriving (Show)
type Directions = [Direction]

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

(-:) :: a -> (a -> b) -> b
x -: f = f x

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, bs) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, bs) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (l, LeftCrumb x r : bs) = Just (Node x l r, bs)
goUp (r, RightCrumb x l : bs) = Just (Node x l r, bs)
goUp (Empty, bs) = Nothing

--insert :: a -> Zipper a -> Zipper a
--insert x (Node _ l r, bs) = (Node x l r, bs)
--insert x (Empty, bs) = (Node x Empty Empty, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Maybe (Zipper a)
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x:xs, bs) = Just (xs, x:bs)
goForward ([], bs) = Nothing

goBackward :: ListZipper a -> Maybe (ListZipper a)
goBackward (xs, x:bs) = Just (x:xs, bs)
goBackward (xs, []) = Nothing

type Name = String
type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "goat_yelling_like_a_man.wmv" "baaaaaa"
    , File "pope_time.avi" "god bless"
    , Folder "pics"
      [ File "ape_throwing_up.jpg" "bleargh"
      , File "watermelon_smash.gif" "smash!!"
      , File "skull_man(scary).bmp" "Yikes!" ]
    , File "dijion_poupon.doc" "best mustard"
    , Folder "programs"
      [ File "fartwizard.exe" "10gotofart"
      , File "owl_bandit.dmg" "mov eax, h00t"
      , File "not_a_virus.exe" "really not a virus"
      , Folder "source code"
        [ File "best_hs_prog.hs" "main = print(fix error)"
        , File "random.hs" "main = print 4" ] ] ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (item, FSCrumb name ls rs:bs) = Just (Folder name (ls ++ [item] ++ rs), bs)
fsUp (item, []) = Nothing

split :: (a -> Bool) -> [a] -> ([a], [a])
split p = rec []
  where
    rec ls (y:rs) | p y = (ls, y:rs)
    rec ls (y:rs) = rec (ls ++ [y]) rs
    rec ls [] = (ls, [])

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, rs) = split (matchName name) items
  in case rs of
    item:rs -> Just (item, FSCrumb folderName ls rs : bs)
    [] -> Nothing
  where
    matchName :: Name -> FSItem -> Bool
    matchName name (File fileName _) = name == fileName
    matchName name (Folder folderName _) = name == folderName

fsRename :: Name -> FSZipper -> FSZipper
fsRename name (File _ d, bs) = (File name d, bs)
fsRename name (Folder _ items, bs) = (Folder name items, bs)

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile item (Folder name items, bs) = Just (Folder name (item:items), bs)
fsNewFile item (File _ _, bs) = Nothing
