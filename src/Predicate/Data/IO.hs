{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | promoted io functions
module Predicate.Data.IO (
   -- ** file handling
    ReadFile
  , ReadFileBinary
  , FileExists
  , ReadDir
  , DirExists
  , AppendFile
  , WriteFile
  , WriteFile'

  -- ** screen
  , Stdout
  , Stderr
  , Stdin
  , ReadIO
  , ReadIO'

  -- ** environment
  , ReadEnv
  , ReadEnvAll

  -- ** date time
  , TimeUtc
  , TimeZt

  -- ** random
  , GenIO
  , GenPure
  , GenNext
  , GenSplit
  , GenRange
  , RandomNext
  , RandomList
  , RandomRNext
  , RandomRList

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Enum (type (...))
import Predicate.Data.Maybe (IsJust)
import Predicate.Data.Iterator (Foldl)
import Predicate.Data.List (type (:+))
import Predicate.Data.Monoid (type (<>), MEmptyT)
import Predicate.Data.ReadShow (ReadP)
import Predicate.Data.Tuple (Second)
import GHC.TypeLits (Symbol,KnownSymbol)
import Data.Proxy (Proxy(Proxy))
import qualified Control.Exception as E
import Data.Kind (Type)
import Control.Arrow (ArrowChoice(left))
import Data.Time (UTCTime, ZonedTime, getCurrentTime, getZonedTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO (hPutStr, withFile, IOMode(WriteMode, AppendMode))
import System.Environment (getEnvironment, lookupEnv)
import qualified Data.ByteString.Char8 as BS8
import System.Random
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import Predicate.Prelude

-- | similar to 'System.IO.readFile'
--
-- >>> pz @(ReadFile "LICENSE" >> 'Just Id >> Len > 0) ()
-- Val True
--
-- >>> pz @(FileExists "xyzzy") ()
-- Val False
--
data ReadFile p deriving Show

instance ( PP p x ~ String
         , P p x
         ) => P (ReadFile p) x where
  type PP (ReadFile p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadFile"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (Fail msg1) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> litL opts b) [hh pp]

-- | similar to 'Data.ByteString.readFile'
data ReadFileBinary p deriving Show

instance ( PP p x ~ String
         , P p x
         ) => P (ReadFileBinary p) x where
  type PP (ReadFileBinary p) x = Maybe BS8.ByteString
  eval _ opts x = do
    let msg0 = "ReadFileBinary"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> BS8.readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (Fail msg1) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (BS8.length b) <> " Just " <> litBS opts b) [hh pp]

-- | similar to 'System.Directory.doesFileExist'
data FileExists p deriving Show
type FileExistsT p = ReadFile p >> IsJust

instance P (FileExistsT p) x => P (FileExists p) x where
  type PP (FileExists p) x = PP (FileExistsT p) x
  eval _ = evalBool (Proxy @(FileExistsT p))

-- | similar to 'System.Directory.doesDirectoryExist'
--
-- >>> pz @(DirExists ".") ()
-- Val True
--
-- >>> pz @(DirExists "xxy") ()
-- Val False
--
data DirExists p deriving Show
type DirExistsT p = ReadDir p >> IsJust

instance P (DirExistsT p) x => P (DirExists p) x where
  type PP (DirExists p) x = PP (DirExistsT p) x
  eval _ = evalBool (Proxy @(DirExistsT p))

-- | similar to 'System.Directory.listDirectory'
data ReadDir p deriving Show
instance ( PP p x ~ String
         , P p x
         ) => P (ReadDir p) x where
  type PP (ReadDir p) x = Maybe [FilePath]
  eval _ opts x = do
    let msg0 = "ReadDir"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesDirectoryExist p
                if b then Just <$> listDirectory p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (Fail msg1) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> showL opts b) [hh pp]

-- | read an environment variable: similar to 'System.Environment.getEnv'
--
-- >>> pz @(ReadEnv "PATH" >> 'Just Id >> 'True) ()
-- Val True
--
data ReadEnv p deriving Show

instance ( PP p x ~ String
         , P p x
         ) => P (ReadEnv p) x where
  type PP (ReadEnv p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadEnv"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ lookupEnv p
        pure $ case mb of
          Nothing -> mkNode opts (Fail msg1) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just v) -> mkNode opts (Val (Just v)) (msg1 <> " " <> litL opts v) [hh pp]

-- | read all the environment variables as key value pairs: similar to 'System.Environment.getEnvironment'
data ReadEnvAll deriving Show

instance P ReadEnvAll a where
  type PP ReadEnvAll a = [(String,String)]
  eval _ opts _ = do
    let msg0 = "ReadEnvAll"
    mb <- runIO getEnvironment
    pure $ case mb of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (Val v) (msg0 <> " count=" <> show (length v)) []

-- | get the current time using 'UTCTime'
data TimeUtc deriving Show

instance P TimeUtc a where
  type PP TimeUtc a = UTCTime
  eval _ opts _ = do
    let msg0 = "TimeUtc"
    mb <- runIO getCurrentTime
    pure $ case mb of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (Val v) (msg0 <> " " <> showL opts v) []

-- | get the current time using 'ZonedTime'
data TimeZt deriving Show

instance P TimeZt a where
  type PP TimeZt a = ZonedTime
  eval _ opts _ = do
    let msg0 = "TimeZt"
    mb <- runIO getZonedTime
    pure $ case mb of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (Val v) (msg0 <> " " <> showL opts v) []

data FHandle s = FStdout | FStderr | FOther !s !WFMode
  deriving stock (Read, Show, Eq)

class GetFHandle (x :: FHandle Symbol) where getFHandle :: FHandle String
instance GetFHandle 'FStdout where getFHandle = FStdout
instance GetFHandle 'FStderr where getFHandle = FStderr
instance ( GetMode w
         , KnownSymbol s
         ) => GetFHandle ('FOther s w) where getFHandle = FOther (symb @s) (getMode @w)

data WFMode = WFAppend | WFWrite | WFWriteForce
  deriving stock (Read, Show, Eq)

class GetMode (x :: WFMode) where getMode :: WFMode
instance GetMode 'WFAppend where getMode = WFAppend
instance GetMode 'WFWriteForce where getMode = WFWriteForce
instance GetMode 'WFWrite where getMode = WFWrite

data WriteFileImpl (hh :: FHandle Symbol) p deriving Show

-- | append to a file
data AppendFile (s :: Symbol) p deriving Show
type AppendFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFAppend) p

instance P (AppendFileT s p) x => P (AppendFile s p) x where
  type PP (AppendFile s p) x = PP (AppendFileT s p) x
  eval _ = eval (Proxy @(AppendFileT s p))


-- | write to file, overwriting if needed
data WriteFile' (s :: Symbol) p deriving Show
type WriteFileT' (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWriteForce) p

instance P (WriteFileT' s p) x => P (WriteFile' s p) x where
  type PP (WriteFile' s p) x = PP (WriteFileT' s p) x
  eval _ = eval (Proxy @(WriteFileT' s p))

-- | write to file, without overwriting
data WriteFile (s :: Symbol) p deriving Show
type WriteFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWrite) p

instance P (WriteFileT s p) x => P (WriteFile s p) x where
  type PP (WriteFile s p) x = PP (WriteFileT s p) x
  eval _ = eval (Proxy @(WriteFileT s p))

-- | write a string value to stdout
data Stdout p deriving Show
type StdoutT p = WriteFileImpl 'FStdout p

instance P (StdoutT p) x => P (Stdout p) x where
  type PP (Stdout p) x = PP (StdoutT p) x
  eval _ = eval (Proxy @(StdoutT p))

-- | write a string value to stderr
data Stderr p deriving Show
type StderrT p = WriteFileImpl 'FStderr p

instance P (StderrT p) x => P (Stderr p) x where
  type PP (Stderr p) x = PP (StderrT p) x
  eval _ = eval (Proxy @(StderrT p))

instance ( GetFHandle fh
         , P p a
         , PP p a ~ String
         ) => P (WriteFileImpl fh p) a where
  type PP (WriteFileImpl fh p) a = ()
  eval _ opts a = do
    let fh = getFHandle @fh
        msg0 = case fh of
                      FStdout -> "Stdout"
                      FStderr -> "Stderr"
                      FOther s w -> (<>("[" <> s <> "]")) $ case w of
                         WFAppend -> "AppendFile"
                         WFWrite -> "WriteFile"
                         WFWriteForce -> "WriteFile'"
    pp <- eval (Proxy @p) opts a
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right ss -> do
          mb <- runIO $ case fh of
                  FStdout -> fmap (left show) $ E.try @E.SomeException $ putStr ss
                  FStderr -> fmap (left show) $ E.try @E.SomeException $ putStr ss
                  FOther s w -> do
                     b <- doesFileExist s
                     if b && w == WFWrite then pure $ Left $ "file [" <> s <> "] already exists"
                     else do
                            let md = case w of
                                   WFAppend -> AppendMode
                                   _ -> WriteMode
                            fmap (left show) $ E.try @E.SomeException $ withFile s md (`hPutStr` ss)
          pure $ case mb of
            Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" [hh pp]
            Just (Left e) -> mkNode opts (Fail $ msg0 <> ":" <> e) "" [hh pp]
            Just (Right ()) -> mkNode opts (Val ()) msg0 [hh pp]

-- | read in a value of a given type from stdin with a prompt: similar to 'System.IO.readIO'
type ReadIO (t :: Type) = ReadIO' t "Enter value"
type ReadIO' (t :: Type) s = Stdout (s <> ":") >> Stdin >> ReadP t Id
-- eg pa @(ReadIO Int + ReadIO Int) ()

-- | read a value from stdin
data Stdin deriving Show

instance P Stdin x where
  type PP Stdin x = String
  eval _ opts _ = do
    let msg0 = "Stdin"
    mb <- runIO $ do
                      lr <- E.try getLine
                      pure $ case lr of
                        Left (e :: E.SomeException) -> Left $ show e
                        Right ss -> Right ss
    pure $ case mb of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just (Left e) -> mkNode opts (Fail $ msg0 <> ":" <> e) "" []
      Just (Right ss) -> mkNode opts (Val ss) (msg0 <> "[" <> litVerbose opts "" ss <> "]") []

-- | generate a random number: see 'System.Random.newStdGen'
data GenIO deriving Show

instance P GenIO x where
  type PP GenIO x = StdGen
  eval _ opts _ = do
    let msg0 = "GenIO"
    mg <- runIO newStdGen
    pure $ case mg of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just g -> mkNode opts (Val g) (msg0 <> "[" <> showVerbose opts "" g <> "]") []

-- | similar to 'System.Random.mkStdGen'
--
-- >>> pz @(GenPure Id) 1234
-- Val 1235 1
--
data GenPure p deriving Show

instance (PP p x ~ Int, P p x) => P (GenPure p) x where
  type PP (GenPure p) x = StdGen
  eval _ opts x = do
    let msg0 = "GenPure"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let g = mkStdGen p
        in mkNode opts (Val g) msg0 [hh pp]

-- | get the next random number of type @t@ using generator @r@ : similar to 'System.Random.random'
--
-- >>> pz @(UnfoldN 5 (RandomNext Bool Id) Id) (mkStdGen 3)
-- Val [True,True,False,True,True]
--
data RandomNext (t :: Type) p deriving Show

instance ( Random t
         , P p x
         , Show (PP p x)
         , RandomGen (PP p x)
         ) => P (RandomNext t p) x where
  type PP (RandomNext t p) x = (t, PP p x)
  eval _ opts x = do
    let msg0 = "RandomNext"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (a,g) = random p
        in mkNode opts (Val (a,g)) (msg0 <> "[" <> showVerbose opts "" g <> "]") [hh pp]

-- | get a list of @n@ random numbers of type @t@ using generator @p@: similar to 'System.Random.randoms'
--
-- >>> pz @(RandomList 10 Bool Id) (mkStdGen 4)
-- Val ([True,True,False,True,True,True,True,False,False,True],2036574526 1336516156)
--
data RandomList n (t :: Type) p deriving Show
type RandomListT n t p = Foldl (Fst >> Second (RandomNext t Id) >> '(L21 :+ Fst, L22)) '(MEmptyT [t],p) (1...n)

instance P (RandomListT n t p) x => P (RandomList n t p) x where
  type PP (RandomList n t p) x = PP (RandomListT n t p) x
  eval _ = eval (Proxy @(RandomListT n t p))


-- | get the next random number of type @t@ in range between @p@ and @q@ using generator @r@ : similar to 'System.Random.randomR'
--
-- >>> pz @(Foldl (Fst >> Second (RandomRNext Int 1 100 Id) >> '(L21 :+ Fst, L22)) '( MEmptyT [Int] ,Id) (1...5)) (mkStdGen 3)
-- Val ([12,26,33,94,64],781515869 652912057)
--
-- >>> pz @(UnfoldN 10 (RandomRNext _ (C "A") (C "H") Id) Id) (mkStdGen 3)
-- Val "DBABDDEEEA"
--
data RandomRNext (t :: Type) p q r deriving Show

instance ( Random t
         , P r x
         , RandomGen (PP r x)
         , Show (PP r x)
         , PP p x ~ t
         , PP q x ~ t
         , P p x
         , P q x
         ) => P (RandomRNext t p q r) x where
  type PP (RandomRNext t p q r) x = (t, PP r x)
  eval _ opts x = do
    let msg0 = "RandomRNext"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        rr <- eval (Proxy @r) opts x
        pure $ case getValueLR NoInline opts msg0 rr [hh pp,hh qq] of
          Left e -> e
          Right r ->
            let (a,g) = randomR (p,q) r
            in mkNode opts (Val (a,g)) (msg0 <> "[" <> showVerbose opts "" g <> "]") [hh pp, hh qq, hh rr]

-- | list @n@ random numbers of type @t@ in range between @p@ and @q@ using generator @r@ : similar to 'System.Random.randomRs'
--
-- >>> pz @(RandomRList 10 Int 0 6 Id) (mkStdGen 1)
-- Val ([6,6,5,1,3,0,3,6,5,2],1244126523 1336516156)
--
-- >>> pz @(RandomRList 10 _ (C "A") (C "F") Id) (mkStdGen 1)
-- Val ("EEBCBEFBEF",1244126523 1336516156)
--
data RandomRList n (t :: Type) p q r deriving Show
type RandomRListT n t p q r = Foldl (Fst >> Second (RandomRNext t p q Id) >> '(L21 :+ Fst, L22)) '(MEmptyT [t],r) (1...n)

instance P (RandomRListT n t p q r) x => P (RandomRList n t p q r) x where
  type PP (RandomRList n t p q r) x = PP (RandomRListT n t p q r) x
  eval _ = eval (Proxy @(RandomRListT n t p q r))

-- | similar to 'System.Random.split'
data GenSplit p deriving Show

instance (RandomGen (PP p x), P p x) => P (GenSplit p) x where
  type PP (GenSplit p) x = (PP p x, PP p x)
  eval _ opts x = do
    let msg0 = "GenSplit"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let g = split p
        in mkNode opts (Val g) msg0 [hh pp]


-- | similar to 'System.Random.next'
data GenNext p deriving Show

instance (RandomGen (PP p x), P p x) => P (GenNext p) x where
  type PP (GenNext p) x = (Int, PP p x)
  eval _ opts x = do
    let msg0 = "GenNext"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let g = next p
        in mkNode opts (Val g) msg0 [hh pp]

-- | similar to 'System.Random.genRange'
data GenRange p deriving Show

instance (RandomGen (PP p x), P p x) => P (GenRange p) x where
  type PP (GenRange p) x = (Int, Int)
  eval _ opts x = do
    let msg0 = "GenRange"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let g = genRange p
        in mkNode opts (Val g) msg0 [hh pp]

