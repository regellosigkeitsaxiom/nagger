module Main where

import Types
import Actions
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "add" ] -> addNag
    [ "+" ] -> addNag
    [ "completed" ] -> showNags [ Complete ]
    [ "active" ] -> showNags [ Active ]
    [ "cancelled" ] -> showNags [ Cancelled ]
    [ "queued" ] -> showNags [ Queued ]
    [ "q" ] -> showNags [ Queued ]
    [ "all" ] -> showNags [ Active, Queued, Complete, Cancelled ]
    [ "a" ] -> showNags [ Active, Queued, Complete, Cancelled ]
    [ "tags" ] -> showTags
    ( "tag":rest ) -> showNagsByTags rest
    ( "t":rest ) -> showNagsByTags rest
    [ ] -> showNags [ Active, Queued ]
    _ -> do
      putStrLn "nagger add | +"
      putStrLn "nagger completed"
      putStrLn "nagger active"
      putStrLn "nagger cancelled"
      putStrLn "nagger queued | q"
      putStrLn "nagger all | a"
      putStrLn "nagger tags"
      putStrLn "nagger tag <tags> | t <tags>"
      putStrLn "nagger (active+queued)"
