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
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Predicate.Data.Maybe (IsJust)
import Predicate.Data.Monoid (type (<>))
import Predicate.Data.ReadShow (ReadP)
import GHC.TypeLits (Symbol,KnownSymbol)
import Data.Proxy (Proxy(Proxy))
import qualified Control.Exception as E
import Data.Kind (Type)
import Control.Arrow (ArrowChoice(left))
import Data.Time (UTCTime, ZonedTime, getCurrentTime, getZonedTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO (hPutStr, withFile, IOMode(WriteMode, AppendMode), stderr)
import System.Environment (getEnvironment, lookupEnv)
import qualified Data.ByteString.Char8 as B8
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import Predicate

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
        mb <- runIO $ ifM (doesFileExist p)
                          (Just <$> readFile p)
                          mempty
        pure $ case mb of
          Nothing -> mkNode opts (Fail (msg1 <> " must run in IO")) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> litL opts b) [hh pp]

-- | similar to 'Data.ByteString.readFile'
data ReadFileBinary p deriving Show

instance ( PP p x ~ String
         , P p x
         ) => P (ReadFileBinary p) x where
  type PP (ReadFileBinary p) x = Maybe B8.ByteString
  eval _ opts x = do
    let msg0 = "ReadFileBinary"
    pp <- eval (Proxy @p) opts x
    case getValueLR NoInline opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ ifM (doesFileExist p)
                          (Just <$> B8.readFile p)
                          mempty
        pure $ case mb of
          Nothing -> mkNode opts (Fail (msg1 <> " must run in IO")) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (B8.length b) <> " Just " <> litBS opts b) [hh pp]

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
        mb <- runIO $ ifM (doesDirectoryExist p)
                          (Just <$> listDirectory p)
                          mempty
        pure $ case mb of
          Nothing -> mkNode opts (Fail (msg1 <> " must run in IO")) "" [hh pp]
          Just Nothing -> mkNode opts (Val Nothing) (msg1 <> " does not exist") [hh pp]
          Just (Just b) -> mkNode opts (Val (Just b)) (msg1 <> " len=" <> show (length b) <> " Just " <> showL opts b) [hh pp]

-- | read an environment variable: similar to 'System.Environment.getEnv'
--
-- >>> pz @(ReadEnv "PATH" >> Just' >> Not Null) ()
-- Val True
--
-- >>> pl @(ReadEnv "xyzzy") ()
-- Present Nothing (ReadEnv[xyzzy] does not exist)
-- Val Nothing
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
          Nothing -> mkNode opts (Fail (msg1 <> " must run in IO")) "" [hh pp]
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
                  FStdout -> left E.displayException <$> E.try @E.SomeException (putStr ss)
                  FStderr -> left E.displayException <$> E.try @E.SomeException (hPutStr stderr ss)
                  FOther s w -> do
                     b <- doesFileExist s
                     if b && w == WFWrite then pure $ Left $ "file [" <> s <> "] already exists"
                     else do
                            let md = case w of
                                       WFAppend -> AppendMode
                                       WFWrite -> WriteMode
                                       WFWriteForce -> WriteMode
                            left E.displayException <$> E.try @E.SomeException (withFile s md (`hPutStr` ss))
          pure $ case mb of
            Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" [hh pp]
            Just (Left e) -> mkNode opts (Fail $ msg0 <> ":" <> e) "" [hh pp]
            Just (Right ()) -> mkNode opts (Val ()) msg0 [hh pp]

-- | read in a value of a given type from stdin with a prompt: similar to 'System.IO.readIO'
type ReadIO (t :: Type) = ReadIO' t "Enter value"
-- | similar to 'ReadIO' but allow the user to specify the prompt string @s@
type ReadIO' (t :: Type) s = Stdout (s <> ":") >> Stdin >> ReadP t Id
-- eg pa @(ReadIO Int + ReadIO Int) ()

-- | read a value from stdin
data Stdin deriving Show

instance P Stdin x where
  type PP Stdin x = String
  eval _ opts _ = do
    let msg0 = "Stdin"
    mb <- runIO $ E.try @E.SomeException getLine
    pure $ case mb of
      Nothing -> mkNode opts (Fail (msg0 <> " must run in IO")) "" []
      Just (Left e) -> mkNode opts (Fail $ msg0 <> ":" <> E.displayException e) "" []
      Just (Right ss) -> mkNode opts (Val ss) (msg0 <> "[" <> litVerbose opts "" ss <> "]") []

