import Test.DocTest
-- main = doctest ["src", "--verbose"]
main :: IO ()

main = doctest ["src","-XNoStarIsType"]


-- stack exec doctest -- "src/Predicate/Prelude.hs"
-- stack exec doctest -- src
