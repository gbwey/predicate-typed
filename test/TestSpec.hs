module Main where
import qualified TestPredicate
import qualified TestJson
import qualified TestRefined
import qualified TestRefined2
import qualified TestRefined3
import Data.Functor
import Test.Tasty
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  defaultMain $ testGroup "alltests"
    [ TestJson.suite
    , TestPredicate.suite
    , TestRefined.suite
    , TestRefined2.suite
    , TestRefined3.suite
    ]
