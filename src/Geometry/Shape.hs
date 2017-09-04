module Geometry.Shape
  ( Point(..)
  , Shape(..)
  , area
  , nudge
  , baseCircle
  , baseRect
  ) where

data Shape =
  Circle Point Float |
  Rectangle Point Point
  deriving (Show)

data Point = Point Float Float
  deriving (Show)

area :: Shape -> Float
area (Circle _ radius) = pi * radius ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) radius) dx dy = Circle (Point (x + dx) (y + dy)) radius
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy = Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
