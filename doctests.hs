import Test.DocTest
-- main = doctest ["src", "--verbose"]
main :: IO ()
main = doctest ["-isrc", "src", "-XDataKinds", "-XTypeApplications", "-XTypeOperators", "-XNoStarIsType"]

{-
C:\haskell\predicate-typed>stack exec doctest -- "src/Predicate.hs"

-- just specify the directory
C:\haskell\predicate-typed>stack exec doctest -- src
-}