module Todo where

import System.Environment
import System.Directory
import System.IO
import Control.Exception
import System.Random

data FailableDouble = Failure
                    | OK Double
  deriving Show

main :: IO ()
main = getArgs >>= matchArguments
--main = do
--  arguments <- getArgs
--  matchArguments arguments

matchArguments :: [String] -> IO ()
matchArguments [] = putStrLn "usage: [add|view|remove|bump] [args]"
matchArguments (command:arguments) = dispatch command arguments

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch _ = usage

usage :: [String] -> IO ()
usage _ = putStrLn "usage: [add|view|remove|bump]"

add :: [String] -> IO ()
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")
add _ = putStrLn "usage: add [filename] [todo item]"

view :: [String] -> IO ()
view [filename] =
  readFile filename >>=
  (\contents ->
    putStrLn "Here are your todo items:" >>=
    (\_ ->
      let numberedItems = unlines .
                          zipWith (\n line -> show (n :: Integer) ++ " - " ++ line) [0 ..] $
                          lines contents
      in putStr numberedItems))
view _ = putStrLn "usage: [add|view|remove|bump] [args]"

--view [filename] = do
--  contents <- readFile filename
--  let numberedItems =
--        unlines .
--        zipWith (\n line -> show (n :: Integer) ++ " - " ++ line) [0 ..] $
--        lines contents
--  putStrLn "Here are your todo items:"
--  putStr numberedItems
--view _ = putStrLn "usage: [add|view|remove|bump] [args]"

remove :: [String] -> IO ()
remove = modifyTodo workFun "remove"
  where
    workFun :: Int -> [String] -> [String]
    workFun todoId todoItems = take todoId todoItems ++ drop (todoId + 1) todoItems

bump :: [String] -> IO ()
bump = modifyTodo workFun "bump"
  where
    workFun :: Int -> [String] -> [String]
    workFun todoId todoItems = (todoItems !! todoId) : take todoId todoItems ++ drop (todoId + 1) todoItems

modifyTodo :: (Int -> [String] -> [String]) -> String -> [String] -> IO ()
modifyTodo modifyFun _ [filename, idString] =
  readFile filename >>= (\contents ->
    let todoId = read idString
        newTodoItems = unlines . modifyFun todoId $ lines contents
    in bracketOnError
      (openTempFile "." "temp")
      (\(tempName, tempHandle) -> hClose tempHandle >>=
                                  (\_ -> removeFile tempName))
      (\(tempName, tempHandle) -> do
         hPutStr tempHandle newTodoItems
         hClose tempHandle
         removeFile filename
         renameFile tempName filename))
--modifyTodo modifyFun _ [filename, idString] = do
--  contents <- readFile filename
--  let todoId = read idString
--      newTodoItems = unlines . modifyFun todoId $ lines contents
--  bracketOnError
--    (openTempFile "." "temp")
--    (\(tempName, tempHandle) -> do
--       hClose tempHandle
--       removeFile tempName)
--    (\(tempName, tempHandle) -> do
--       hPutStr tempHandle newTodoItems
--       hClose tempHandle
--       removeFile filename
--       renameFile tempName filename)
modifyTodo _ command _ = putStrLn ("usage: " ++ command ++ " [filename] [id]")
