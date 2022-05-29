module Main where

import System.Environment (getArgs)
import TRS
import Parser
import MyLib

main :: IO ()
main = do
    filepath : _ <- getArgs
    parsed <- readTRSFile filepath
    case parsed of
        Left e -> print e
        Right trs -> do
            putStr $ showTRS trs
            putStrLn "-----------"
            print (nf3 trs $ Con "main")

