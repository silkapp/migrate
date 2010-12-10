{-# LANGUAGE
    DeriveDataTypeable
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , TemplateHaskell
  #-}
module Migrate where

import Control.Exception
import Data.Record.Label
import Data.Typeable

-- Maybe lifted to the type level.
data Nothing
data Just a

-- Type proxy, used to pass a type to a function.
data Proxy a = Proxy

-- A mapping from a type to the previous version of it.
type family PrevVersion a :: * -- Maybe *

version :: forall a. VersionNumber (PrevVersion a) => Proxy a -> Int
version _ = version' (Proxy :: Proxy (Just a))

-- Migrations between types.
class Migrate a b where
  migrate :: a -> b

-- We can migrate every type to itself.
instance Migrate a a where
  migrate = id

-------------------------------------------------------------------------------

data NoPreviousVersion = NoPreviousVersion
  deriving (Show, Typeable)

instance Exception NoPreviousVersion

data VersionMismatch =
  VersionMismatch
    { expectedVersion :: Int
    , actualVersion   :: Int
    } deriving (Show, Typeable)

instance Exception VersionMismatch

class VersionNumber a where
  version' :: Proxy a -> Int

instance VersionNumber Nothing where
  version' _ = -1

instance VersionNumber (PrevVersion a) => VersionNumber (Just a) where
  version' _ = 1 + version' (Proxy :: Proxy (PrevVersion a))

newtype Versioned a = Versioned { _versioned :: a }
  deriving (Eq, Ord, Read, Show)

$(mkLabels [''Versioned])

