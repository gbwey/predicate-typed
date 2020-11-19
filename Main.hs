{-
:set -package th-lift
:set -package th-lift-instances
:l main TH_Orphans.hs
-}
{-# OPTIONS -Wno-missing-export-lists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where
import Predicate.Examples.Refined3
import Predicate.Refined3
import Predicate
import Data.Typeable
import qualified Type.Reflection as TR
import qualified GHC.TypeLits as GL
import Data.Time
import Data.Kind (Type)
import Data.These
import TH_Orphans ()
import Instances.TH.Lift ()
--import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString (ByteString)

main :: IO ()
main = putStrLn "ok"

test0a :: Refined 'OA (Between 0 0xff Id) Int
test0a = $$(refinedTH 0xfe)

test3a :: MakeR3 (BaseN 'OU 16)
test3a = $$(refined3TH "0000fe")

test3b :: Refined3 'OU
   (Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> L2 Head >> Map (ReadP Int Id))
   (All (0 <..> 0xff))
   (PrintL 4 "%03d.%03d.%03d.%03d" Id)
   String
test3b = $$(refined3TH "123.211.122.1")

test3c :: Refined3 'OU
   (Resplit "\\." >> Map (ReadP Int Id))
   (All (0 <..> 0xff) && Len == 4)
   (PrintL 4 "%03d.%03d.%03d.%03d" Id)
   String
test3c = $$(refined3TH "200.2.3.4")

test3d :: Refined3 'OU
   (Rescan "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" >> L2 Head >> Map (ReadP Int Id))
   (BoolsN (PrintT "value %d at index %d is outside 0..255" Swap) 4 (0 <..> 0xff))
   (PrintL 4 "%03d.%03d.%03d.%03d" Id)
   String
test3d = $$(refined3TH "123.211.89.1")

-- failed
test4a :: Refined OU
   (Pop2' (Proxy ('(,) :: Type -> Bool -> (Type,Bool))) (Proxy (W "bbb")) Fst Snd >> Snd)
   (Proxy 'True, Int)
test4a = $$(refinedTH @OU (Proxy @'True,1234))


test4c :: Refined OU
   (Pop0 (Proxy "abc") () >> 'True)
   ()
test4c = $$(refinedTH @OU ())

-- fails
test4d :: Refined OU
   (Pop1' (Proxy (Lift "abc" :: GL.Nat -> Type)) (Proxy 4) () >> 'True)
   ()
test4d = $$(refinedTH @OU ())

-- fails
test4e :: Refined OU
   (PApp (Proxy (Lift "abc" :: Type -> Type)) (Proxy ()) >> 'True)
   ()
test4e = $$(refinedTH @OU ())

-- fails
test4f :: Refined OU
   (PApp (Proxy (Lift "abc" :: Type -> Type)) (Proxy ()) >> (4 > 3))
   ()
test4f = $$(refinedTH @OU ())

test4g :: Either
    Msg0
    (Refined OU (PApp (Proxy (Lift "abc" :: Type -> Type)) (Proxy ()) >> 'True) ())
test4g = newRefined @OU @(PApp (Proxy (Lift "abc")) (Proxy ()) >> 'True) ()

test4h :: Either Msg0 (Refined OU ("abc" >> 'True) ())
test4h = newRefined @OU @("abc" >> 'True) ()

test4b :: Refined OU
   ("abc" >> 'True)
   ()
test4b = $$(refinedTH @OU ())

-- fails
test4d0 :: Refined OU
   (Pop1' (Proxy (Lift "abc" :: GL.Nat -> Type)) (Proxy 4) () >> 'True)
   ()
test4d0 = $$(refinedTH @OU @(Pop1' (Proxy (Lift "abc")) (Proxy 4) () >> 'True) ())

test4d1 :: Either Msg0 (Refined OU
   (Pop1' (Proxy (Lift "abc" :: GL.Nat -> Type)) (Proxy 4) () >> 'True)
   ())
test4d1 = newRefined @OU @(Pop1' (Proxy (Lift "abc")) (Proxy 4) () >> 'True) ()

-- fails
test4k :: Refined OU
   (PApp (Proxy ((<>) "abc" :: GL.Symbol -> Type)) (Proxy "def") >> 'True)
   ()
test4k = $$(refinedTH @OU ())

-- fails
test4k1 :: Either Msg0 (Refined 'OU
   (PApp (Proxy ((<>) "abc" :: GL.Symbol -> Type)) (Proxy "def") >> 'True)
   ())
test4k1 = newRefined @OU @(PApp (Proxy ((<>) "abc")) (Proxy "def") >> 'True) ()

-- ok
test4m :: Either Msg0 (Refined OU
   (("abc" <> "def") >> 'True)
   ())
test4m = newRefined @OU ()

-- ok
test4m1 :: Refined OU
   (("abc" <> "def") >> 'True)
   ()
test4m1 = $$(refinedTH @OU ())

-- fails: cannot write signatures for this stuff if using tricky kind application
-- will work without the signature tho!
test4n :: Refined OU
   (PApp (Proxy (L1 :: Type -> Type)) (Proxy Id) >> 'True)
   ()
test4n = $$(refinedTH @OU ())

-- only works without the signature!!! PApp Pop0 etc all use k application
test4n1 :: Refined OU (PApp (Proxy (L1 :: Type -> Type)) (Proxy Id) >> 'True) ()
test4n1 = $$(refinedTH @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ())

test4n2 :: Either Msg0 (Refined OU (PApp (Proxy (L1 :: Type -> Type)) (Proxy Id) >> 'True) ())
test4n2 = newRefined @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ()

-- fails
test4d2 :: Refined OU (Pop1 (Proxy (Lift "abc" :: GL.Nat -> Type)) 4 () >> 'True) ()
test4d2 = $$(refinedTH @OU @(Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ())

{-
>$$(refinedTH @OU @(Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ()) :: Refined OU (Pop1 (Proxy)

<interactive>:493:4: error:
    * Couldn't match kind 'k' with 'GN.Nat'
      When matching types
        z0 :: GN.Nat -> *
        Lift "abc" :: k -> *
    * In the expression:
        refinedTH @OU @(Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ()
      In the Template Haskell splice
        $$(refinedTH @OU @(Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ())
      In the expression:
          $$(refinedTH @OU @(Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ()) ::
            Refined OU (Pop1 (Proxy (Lift "abc")) 4 () >> 'True) ()

main.hs:140:14: error:
    * Couldn't match kind 'k' with '*'
      When matching types
        w0 :: k
        Id :: *
    * In the expression:
        refinedTH @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ()
      In the Template Haskell splice
        $$(refinedTH @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ())
      In the expression:
        $$(refinedTH @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ())
    * Relevant bindings include
        test4n1 :: Refined OU (PApp (Proxy L1) (Proxy Id) >> 'True) ()
          (bound at main.hs:140:1)
    |
140 | test4n1 = $$(refinedTH @OU @(PApp (Proxy L1) (Proxy Id) >> 'True) ())

main.hs:133:13: error:
    * Couldn't match kind 'k' with '*'
      When matching types
        w0 :: k
        Id :: *
    * In the expression: refinedTH @OU ()
      In the Template Haskell splice $$(refinedTH @OU ())
      In the expression: $$(refinedTH @OU ())
    * Relevant bindings include
        test4n :: Refined OU (PApp (Proxy L1) (Proxy Id) >> 'True) ()
          (bound at main.hs:133:1)
    |
133 | test4n = $$(refinedTH @OU ())
-}

ttt :: IO ()
ttt =
  let t = typeOf (Proxy @Id)
  in case t of
       TR.SomeTypeRep (_ :: TR.TypeRep p) ->
         --let y = runIdentity $ eval (Proxy @p) defOpts ()
         print (Proxy @p)

test5a :: Refined OU (Id < TimeUtc) UTCTime
test5a = $$(refinedTHIO @OU (read "2020-01-01 12:12:12Z"))

test5b :: Refined OU (EBoth' >> Fst > 3 || Snd) (Elr Int Bool)
test5b = $$(refinedTHIO @OU (EBoth 4 False))

test5c :: Refined OU (These' >> Fst > 3 || Snd) (These Int Bool)
test5c = $$(refinedTHIO @OU (These 1 True))

test5d :: Refined OU (ToString >> Len >= 4) Text
test5d = $$(refinedTH @OU ("Asdf" :: Text))

test5e :: Refined3 OU ToString (Len >= 4) (FromString ByteString Id) ByteString
test5e = $$(refined3TH @OU ("Asdf" :: ByteString))

test5f :: Refined OU (Succ > 'EQ) Ordering
test5f = $$(refinedTH @OU EQ)

