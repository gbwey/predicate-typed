module Main where
import EasyTest
import qualified TestPredicate
import qualified TestJson
import qualified TestRefined
import Data.Functor

main :: IO ()
main = do
  TestPredicate.doit
  run TestJson.suite
  run TestRefined.suite
