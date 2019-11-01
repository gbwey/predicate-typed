import Test.DocTest
-- main = doctest ["src", "--verbose"]
main :: IO ()
main = doctest ["src"]

-- stack exec doctest -- "src/Predicate.hs"
-- stack exec doctest -- src
