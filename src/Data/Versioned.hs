{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , TemplateHaskell
  #-}
module Data.Versioned
  ( Versioned (..)
  , versioned
  ) where

import Data.Foldable
import Data.Generics (Data, Typeable)
import Data.Traversable

import qualified Data.Label as L

newtype Versioned a = Versioned { _versioned :: a }
  deriving ( Eq
           , Ord
           , Read
           , Show
           , Functor
           , Foldable
           , Traversable
           , Data
           , Typeable
           )

$(L.mkLabels [''Versioned])

