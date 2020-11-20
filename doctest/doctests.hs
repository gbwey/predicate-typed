import Test.DocTest
main :: IO ()

main = doctest ["src","-XNoStarIsType"]
--main = doctest ["src","--verbose","-XNoStarIsType"]


-- stack exec doctest -- "src/Predicate.hs"
-- stack exec doctest -- src
