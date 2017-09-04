module MyDay where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Ord, Show, Read, Eq, Bounded, Enum)
