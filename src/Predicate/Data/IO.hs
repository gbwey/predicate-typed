{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
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
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
   promoted io functions
-}
module Predicate.Data.IO (

    ReadFile
  , ReadFileBinary
  , FileExists
  , ReadDir
  , DirExists
  , ReadEnv
  , ReadEnvAll
  , TimeUtc
  , TimeZt
  , AppendFile
  , WriteFile
  , WriteFile'
  , Stdout
  , Stderr
  , Stdin
  , ReadIO
  , ReadIO'

 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.Maybe (IsJust)
import Predicate.Data.Monoid (type (<>))
import Predicate.Data.ReadShow (ReadP)
import GHC.TypeLits (Symbol,KnownSymbol)
import Data.Proxy
import qualified Control.Exception as E
import Data.Kind (Type)
import Control.Arrow
import Data.Time
import System.Directory
import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS8

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Predicate.Prelude

-- | similar to 'readFile'
--
-- >>> pz @(ReadFile "LICENSE" >> 'Just Id >> Len > 0) ()
-- TrueT
--
-- >>> pz @(FileExists "xyzzy") ()
-- FalseT
--
-- >>> pl @(FileExists "xxy") ()
-- False (IsJust)
-- FalseT
--
data ReadFile p

instance ( PP p x ~ String
         , P p x
         ) => P (ReadFile p) x where
  type PP (ReadFile p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadFile"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT msg1) "" [hh pp]
          Just Nothing -> mkNode opts (PresentT Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (PresentT (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> litL opts b) [hh pp]

data ReadFileBinary p

instance ( PP p x ~ String
         , P p x
         ) => P (ReadFileBinary p) x where
  type PP (ReadFileBinary p) x = Maybe BS8.ByteString
  eval _ opts x = do
    let msg0 = "ReadFileBinary"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> BS8.readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT msg1) "" [hh pp]
          Just Nothing -> mkNode opts (PresentT Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (PresentT (Just b)) (msg1 <> " len=" <> show (BS8.length b) <> " Just " <> litBS opts b) [hh pp]

instance P (FileExistsT p) x => P (FileExists p) x where
  type PP (FileExists p) x = PP (FileExistsT p) x
  eval _ = evalBool (Proxy @(FileExistsT p))

-- | similar to 'doesFileExist'
data FileExists p
type FileExistsT p = IsJust (ReadFile p)

-- | similar to 'doesDirectoryExist'
--
-- >>> pz @(DirExists ".") ()
-- TrueT
--
-- >>> pl @(DirExists ".") ()
-- True (IsJust)
-- TrueT
--
-- >>> pl @(DirExists "xxy") ()
-- False (IsJust)
-- FalseT
--
data DirExists p
type DirExistsT p = IsJust (ReadDir p)

instance P (DirExistsT p) x => P (DirExists p) x where
  type PP (DirExists p) x = PP (DirExistsT p) x
  eval _ = evalBool (Proxy @(DirExistsT p))

-- | similar to 'listDirectory'
data ReadDir p
instance ( PP p x ~ String
         , P p x
         ) => P (ReadDir p) x where
  type PP (ReadDir p) x = Maybe [FilePath]
  eval _ opts x = do
    let msg0 = "ReadDir"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesDirectoryExist p
                if b then Just <$> listDirectory p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT msg1) "" [hh pp]
          Just Nothing -> mkNode opts (PresentT Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (PresentT (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> showL opts b) [hh pp]

-- | read an environment variable: similar to 'getEnv'
--
-- >>> pz @(ReadEnv "PATH" >> 'Just Id >> 'True) ()
-- TrueT
--
data ReadEnv p

instance ( PP p x ~ String
         , P p x
         ) => P (ReadEnv p) x where
  type PP (ReadEnv p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadEnv"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ lookupEnv p
        pure $ case mb of
          Nothing -> mkNode opts (FailT msg1) "" [hh pp]
          Just Nothing -> mkNode opts (PresentT Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just v) -> mkNode opts (PresentT (Just v)) (msg1 <> " " <> litL opts v) [hh pp]

-- | read all the environment variables as key value pairs: similar to 'getEnvironment'
data ReadEnvAll

instance P ReadEnvAll a where
  type PP ReadEnvAll a = [(String,String)]
  eval _ opts _ = do
    let msg0 = "ReadEnvAll"
    mb <- runIO getEnvironment
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (PresentT v) (msg0 <> " count=" <> show (length v)) []

-- | get the current time using 'UTCTime'
data TimeUtc

instance P TimeUtc a where
  type PP TimeUtc a = UTCTime
  eval _ opts _a = do
    let msg0 = "TimeUtc"
    mb <- runIO getCurrentTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (PresentT v) (msg0 <> " " <> showL opts v) []

-- | get the current time using 'ZonedTime'
data TimeZt

instance P TimeZt a where
  type PP TimeZt a = ZonedTime
  eval _ opts _a = do
    let msg0 = "TimeZt"
    mb <- runIO getZonedTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" []
      Just v -> mkNode opts (PresentT v) (msg0 <> " " <> showL opts v) []

data FHandle s = FStdout | FStderr | FOther !s !WFMode deriving (Read, Show, Eq)

class GetFHandle (x :: FHandle Symbol) where getFHandle :: FHandle String
instance GetFHandle 'FStdout where getFHandle = FStdout
instance GetFHandle 'FStderr where getFHandle = FStderr
instance (GetMode w, KnownSymbol s) => GetFHandle ('FOther s w) where getFHandle = FOther (symb @s) (getMode @w)

data WFMode = WFAppend | WFWrite | WFWriteForce deriving (Read, Show, Eq)

class GetMode (x :: WFMode) where getMode :: WFMode
instance GetMode 'WFAppend where getMode = WFAppend
instance GetMode 'WFWriteForce where getMode = WFWriteForce
instance GetMode 'WFWrite where getMode = WFWrite

data WriteFileImpl (hh :: FHandle Symbol) p

-- | append to a file
data AppendFile (s :: Symbol) p
type AppendFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFAppend) p

instance P (AppendFileT s p) x => P (AppendFile s p) x where
  type PP (AppendFile s p) x = PP (AppendFileT s p) x
  eval _ = eval (Proxy @(AppendFileT s p))


-- | write to file, overwriting if needed
data WriteFile' (s :: Symbol) p
type WriteFileT' (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWriteForce) p

instance P (WriteFileT' s p) x => P (WriteFile' s p) x where
  type PP (WriteFile' s p) x = PP (WriteFileT' s p) x
  eval _ = eval (Proxy @(WriteFileT' s p))

-- | write to file, without overwriting
data WriteFile (s :: Symbol) p
type WriteFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWrite) p

instance P (WriteFileT s p) x => P (WriteFile s p) x where
  type PP (WriteFile s p) x = PP (WriteFileT s p) x
  eval _ = eval (Proxy @(WriteFileT s p))

-- | write a string value to stdout
data Stdout p
type StdoutT p = WriteFileImpl 'FStdout p

instance P (StdoutT p) x => P (Stdout p) x where
  type PP (Stdout p) x = PP (StdoutT p) x
  eval _ = eval (Proxy @(StdoutT p))

-- | write a string value to stderr
data Stderr p
type StderrT p = WriteFileImpl 'FStderr p

instance P (StderrT p) x => P (Stderr p) x where
  type PP (Stderr p) x = PP (StderrT p) x
  eval _ = eval (Proxy @(StderrT p))

instance (GetFHandle fh
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
    case getValueLR opts msg0 pp [] of
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
            Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" [hh pp]
            Just (Left e) -> mkNode opts (FailT e) (msg0 <> " " <> e) [hh pp]
            Just (Right ()) -> mkNode opts (PresentT ()) msg0 [hh pp]

-- | read in a value of a given type from stdin with a prompt: similar to 'System.IO.readIO'
type ReadIO (t :: Type) = ReadIO' t "Enter value"
type ReadIO' (t :: Type) s = Stdout (s <> ":") >> Stdin >> ReadP t Id
-- eg pa @(ReadIO Int + ReadIO Int) ()

-- | read a value from stdin
data Stdin

instance P Stdin x where
  type PP Stdin x = String
  eval _ opts _x = do
    let msg0 = "Stdin"
    mb <- runIO $ do
                      lr <- E.try getLine
                      pure $ case lr of
                        Left (e :: E.SomeException) -> Left $ show e
                        Right ss -> Right ss
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) "" []
      Just (Left e) -> mkNode opts (FailT e) (msg0 <> " " <> e) []
      Just (Right ss) -> mkNode opts (PresentT ss) (msg0 <> "[" <> litVerbose opts "" ss <> "]") []

