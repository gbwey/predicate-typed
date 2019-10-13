{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wno-type-defaults #-}
{-# OPTIONS -Wno-redundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE NoOverloadedLists #-} -- overloaded lists breaks the tests
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module TestExtras where
import EasyTest
import System.IO
import Predicate
import Extras
import UtilP
import Data.Ratio
import qualified Data.Text as T
import Data.Typeable
--import Control.Lens
import qualified Data.Semigroup as SG
import Text.Show.Functions ()
import TestRefined hiding (suite)
--import Data.Either
import Data.These

doit :: IO ()
doit = do
  hSetBuffering stdout LineBuffering
  run suite

suite :: Test ()
suite = orderTests allTests

orderTests :: [Test ()] -> Test ()
orderTests = tests . zipWith (\i t -> scope (T.pack (show i)) t) [1::Int ..]

allTests :: [Test ()]
allTests =
  [
    expectPE (PresentT [3,3,3,3]) $ pl @(Proxy (RepeatT 4 3) >> Proxyeval) 123
  , expectPE (PresentT ["ab","ab","ab","ab"]) $ pl @(Proxy (RepeatT 4 "ab") >> Proxyeval) 'x'
  , expectPE (PresentT [99,99,99]) $ pl @(Proxy (RepeatT 3 99) >> Proxyeval) ()
  , expectPE (PresentT [-1 % 5,-1 % 5,-1 % 5]) $ pl @(Proxy (RepeatT 3 (NegR 1 5)) >> Proxyeval) ()
  , expectPE (PresentT ["xy","xy","xy"]) $ pl @(Proxy (RepeatT 3 "xy") >> Proxyeval) ()
  , expectPE (PresentT ("xx",(("xx",20),("xx",20)))) $ pl @(Extend (Fdup _) Id) ("xx",20)
  , expectPE (PresentT "AbcAbc") $ pl @((Fdup _ $ "Abc") >> (Fst <> Snd)) ()
  , expectPE (PresentT 123) $ pl @((Fdup _ >> (F_1 _ _ <$> Id)) $ 123) ()
  , expectPE (PresentT (("xy","aa"),("xy","aa"))) $ pl @(((Fdup _ >> (F_App _ <$> Id)) $ '("xy","aa")) $ MemptyT _) ()
  , expectPE (PresentT "aaa") $ pl @(Fid String >> Fdupx >> F1X $ "aaa") ()
  , expectPE (PresentT "xxxx") $ pl @(Fid _ >> Fdupx >> Mappendx $ "xx") ()
  , expectPE (PresentT 14) $ pl @(Fid _ >> Fdupx >> Mappendx $ (7 >> Wrap (SG.Sum _) Id)) ()
  , expectPE (PresentT (13,15)) $ pl @((FidpP >> Fdupx $ 14) >> PredU *** SuccU) (2::Int)

  , expectPE (PresentT (Just 80)) $ pl @(Fst <$> Snd) ((*20), Just 4)
  , expectPE (PresentT (Just 80)) $ pl @(Fst <*> Snd) (Just (*20), Just 4)
  , expectPE (PresentT (Just 4)) $ pl @(K _ Fst <$> Snd) (4,Just 'x')
  , expectPE (PresentT (Just (4,"asfds"))) $ pl @(F_Comma _ _ <$> (Pure Maybe 4) <*> (Pure _ "asfds")) ()
  , expectPE (PresentT (Just [10,20])) $ pl @(Pure2 [] *** Pure2 [] >> F_App _ <$> Fst <*> Snd) (Just 10, Just 20)
  , expectPE (PresentT (Just [1,2,3,4,10,11,12])) $ pl @(F_Alt _ <$> Fst <*> Snd) (Just [1..4], Just [10..12])
  , expectPE (PresentT [10,12]) $ pl @(Fst <$> Snd) (fst, [(10,'a'),(12,'c')])
  , expectPE (PresentT [10,12]) $ pl @(F_1 _ _ <$> Id) [(10,'a'),(12,'c')]
  , expectPE (PresentT [(10,'a'),(12,'b'),(9,'c')]) $ pl @(F_Swapl _ _ _ <$> Id) [('a',10),('b',12),('c',9)]
  , expectPE (PresentT [Right 'a', Left 12, Right 'c']) $ pl @(F_Swapl _ _ _ <$> Id) [Left 'a',Right 12,Left 'c']
  , expectPE (PresentT (96,"12")) $ pl @(F_Comma _ _ <$> Fst <*> Snd $ 12) ((*8),show)
  , expectPE (PresentT "1212") $ pl @(F_App _ <$> Fst <*> Snd $ 12) (show,show)
  , expectPE (PresentT 96) $ pl @(F_K _ _ <$> Fst <*> Snd $ 12) ((*8),show)
  , expectPE (PresentT "12") $ pl @(F_Flipk _ _ <$> Fst <*> Snd $ 12) ((*8),show)

  , expectPE (PresentT (Just "5")) $ pl @(Fst >>= Snd) (Just 4, Just . show . succ)
  , expectPE (PresentT ("xx","xx")) $ pl @(Extend (F_1 _ _) Id) ("xx",20)
  , expectPE (PresentT ("xx",20)) $ pl @(Extend (F_2 _ _) Id) ("xx",20)
  , expectPE (PresentT (143,"abc")) $ pl @(F_Comma Int String $ 143 $ "abc") 14
  , expectPE (PresentT (143,"abc")) $ pl @(F_Comma _ _ $ 143 $ "abc") 14
  , expectPE (PresentT "aabb") $ pl @(F_Comma String String >> Uncurry >> Mappendx $ '("aa","bb")) 14
  , expectPE (PresentT "aabb") $ pl @(F_Comma _ _ >> Uncurry >> Mappendx $ '("aa","bb")) 14
  , expectPE (PresentT 11) $ pl @((F_Comma _ Int $ "xx") >> F2X $ 11) ()
  , expectPE (PresentT 11) $ pl @((F_Comma _ _ $ "xx") >> F2X $ 11) ()
  , expectPE (PresentT (-11 % 4)) $ pl @(FidpP >> (F_Comma _ _ $ "xx") >> F2X $ NegR 11 4) ()
  , expectPE (PresentT (SG.Sum 25)) $ pl @(F_Comma _ _ >> Uncurry >> Mappendx $ Id) (SG.Sum 12, SG.Sum 13)
  , expectPE (PresentT "aabb") $ pl @(F_Comma _ _ >> Uncurry >> Mappendx $ '("aa","bb")) ()
  , expectPE (PresentT ('x',"13")) $ pl @(Bimap (F_1 _ _) (F_Show _)) (('x',10),13) -- works cos uses 'a' as input -- not flexible

  , expectPE (PresentT "xx") $ pl @(FidpP $ "xx") "1"
  , expectPE (PresentT 5) $ pl @(FidpP $ 4 >> Id >> SuccU) 2
  , expectPE (PresentT [10,12]) $ pl @(Fmapx F1X Id) [(10,'a'),(12,'c')]
  , expectPE (PresentT "ac") $ pl @(Fmapx F2X Id) [(10,'a'),(12,'c')]
  , expectPE (PresentT [10,12]) $ pl @(Fmap Fst Snd) (fst,[(10,'a'),(12,'c')])
  , expectPE (PresentT [('a',10),('c',12)]) $ pl @(Fmap (F_Swap _ _) Snd) (fst, [(10,'a'),(12,'c')])
  , expectPE (PresentT [((10,'a'),(10,'a')),((12,'c'),(12,'c'))]) $ pl @(Fmap (F_Dup _) Snd) (fst, [(10,'a'),(12,'c')])
   -- pl @(Fmap (Fcurry _ _ _ >> F_App _) Id) [("a","b")] -- Fcurry / Funcurry are a noop! why is that?
  , expectPE (PresentT ["ab"]) $ pl @(Fmap (F_Appa _) Id) [("a","b")]
  , expectPE (PresentT ["1","2","3"]) $ pl @(Fmap (F_Show _) Id) [1,2,3]
  -- bothx doesnt compose well
  , expectPE (PresentT [5,99]) $ pl @(Bothx SuccU '[Id] (:+)) (4,99)

  , expectPE (PresentT ("xx","xx")) $ pl @(Extend Fst Snd) (fst,("xx",20))
  , expectPE (PresentT ("xx",20)) $ pl @(Extend Fst Snd) (snd,("xx",20))
  , expectPE (PresentT ("xx",(("xx",20),("xx",20)))) $ pl @(Extend Fst Snd) (\x -> (x,x),("xx",20))
  , expectPE (PresentT "abcdefabcdefabcdefabcdef") $ pl @(Extend Fst Snd $ "abcd" $ "ef") (\x -> x <> x,\x -> x <> x)

  , expectPE (PresentT [3]) $ pl @(GDef_PA Id (Proxylist >> MemptyProxy) (Pure [] Snd) Id) (Just 3)
  , expectPE (PresentT []) $ pl @(GDef_PA Id (Proxylist >> MemptyProxy) (Pure [] Snd) Id) (Nothing @())
  , expectPE (PresentT [[23],[33],[]]) $ pl @(MapF (GDef_PA ThisToMaybe (Proxylist >> MemptyProxy) (Pure [] Snd) Id) ) [This 23,This 33,That 'x']

  , expectPE (PresentT (Right [99,1,2,3,4,5])) $ pl @Fmap_CONS (Right @() (99,[1..5]))
  , expectPE (PresentT (Right [1,2,3,4,5,99])) $ pl @(Fmap_SWAP >> Fmap_SNOC) (Right @() (99,[1..5]))
  , expectPE (PresentT ["abcx"]) $ pl @Fmap_SNOC [("abc"::T.Text,'x')]

  , expectPE (PresentT [Right 'x',Left 10,Left 12,Right 'y',Right 'a']) $ pl @Fmap_SWAP [Left 'x',Right 10,Right 12,Left 'y',Left 'a']
  , expectPE (PresentT [("someval",Left 'x'),("someval",Right 10),("someval",Right 12),("someval",Left 'y'),("someval",Left 'a')]) $ pl @(Fmap_INS "someval" Id) [Left 'x',Right 10,Right 12,Left 'y',Left 'a']

  ]


