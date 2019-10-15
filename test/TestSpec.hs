module Main where
import qualified TestPredicate
import qualified TestJson
import qualified TestRefined
import Data.Functor

main :: IO ()
main = do
  TestPredicate.doit
  TestJson.suite
  TestRefined.suite
