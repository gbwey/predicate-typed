{-# LANGUAGE CPP #-}
import Test.DocTest
-- main = doctest ["src", "--verbose"]
main :: IO ()

#if __GLASGOW_HASKELL__ >= 806
main = doctest ["src","-XNoStarIsType"]
#else
main = doctest ["src"]
#endif


-- stack exec doctest -- "src/Predicate.hs"
-- stack exec doctest -- src
