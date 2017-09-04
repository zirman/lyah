module Locker where

--import qualified Data.Map as Map
--import qualified MyEither
--
--data LockerState = Taken | Free deriving (Show, Eq)
--
--type Code = String
--
--type LockerMap = Map.Map Int (LockerState, Code)
--
--lockerLookup :: Int -> LockerMap -> MyEither.Either String Code
--lockerLookup number lockerMap = case Map.lookup number lockerMap of
--  Nothing -> MyEither.Left $ "Locker " ++ show number ++ " doesn't exist!"
--  Just (Taken, _) -> MyEither.Left $ "Locker " ++ show number ++ " already taken!"
--  Just (Free, code) -> MyEither.Right $ code
