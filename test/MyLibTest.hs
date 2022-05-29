module Main where

import Test.Hspec
import Parser
import TRS
import GHC.Base (returnIO)

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = do
    describe "standard" $ do
        -- passed
        it "fact5" $ do
            res <- exec "./input/fact5.trs"
            res `shouldBe` "120"
        it "mul" $ do
            res <- exec "./input/mul.trs"
            res `shouldBe` "14"
        it "qsort" $ do
            res <- exec "./input/qsort.trs"
            res `shouldBe` "[0,1,2,3,4,5,6,7,8,9]"
        it "array" $ do
            res <- exec "./input/array.trs"
            res `shouldBe` "[1,2,3,4,5]"
        it "array2" $ do
            res <- exec "./input/array2.trs"
            res `shouldBe` "[1,2,3,4,5]"

exec :: String -> IO String
exec filepath = do
    parsed <- readTRSFile filepath
    case parsed of
        Left e -> returnIO $ show e
        Right trs -> do
            returnIO $ show $ nf3 trs $ Con "main"

