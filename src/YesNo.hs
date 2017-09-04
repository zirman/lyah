module YesNo where
import MyTree
import Traffic

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo a => YesNo (Maybe a) where
  yesno (Just a) = yesno a
  yesno _ = False

instance YesNo a => YesNo (Tree a) where
  yesno EmptyTree = False
  yesno (Node left x right) =  yesno left || yesno x || yesno right

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf x thenExp elseExp = if yesno x then thenExp else elseExp
