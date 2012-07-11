{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , Rank2Types
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  #-}
module Text.JSON.Versioned
  ( module Data.Versioned
  , showJSONVersioned
  , readJSONVersioned
  )
  where

import Control.Applicative
import Control.Exception
import Migrate
import Text.JSON

import qualified Data.Label as L

import Data.Versioned

class JSONVersioned a r where
  readJSONVersioned' :: Proxy a -> JSValue -> Result r

-- If there is no previous version, we can't do anything.

instance JSONVersioned Nothing r where
  readJSONVersioned' _ = throw NoPreviousVersion

-- We try to get the 'a', which is PrevVersion r. If that doesn't work, we
-- recurse trying to get older versions of 'a'. Finally, we migrate the 'a' to
-- an 'r'.

instance ( a ~ Versioned o
         , JSON o
         , Migrate a r
         , VersionNumber (PrevVersion a)
         , JSONVersioned (PrevVersion a) a
         ) => JSONVersioned (Just (Versioned o)) r where
  readJSONVersioned' _ j =
    do let v = version (Proxy :: Proxy a)
       o <- readJSON j
       w <- valFromObj "version" o
       migrate <$>
         case compare w v of
           LT -> readJSONVersioned' (Proxy :: Proxy (PrevVersion a)) j :: Result a
           EQ -> do d <- valFromObj "data" o
                    Versioned <$> readJSON d
           GT -> throw (VersionMismatch v w)

instance JSONVersioned (Just (FixedVersion n)) r where
  readJSONVersioned' _ = error "JSONVersioned: FixedVersion found instead of previous version."

showJSONVersioned :: forall a o. (a ~ Versioned o, JSON o, VersionNumber (PrevVersion a)) => a -> JSValue
showJSONVersioned a =
  let v = version (Proxy :: Proxy a)
      x = L.get versioned a
  in  showJSON (toJSObject [("version", showJSON v), ("data", showJSON x)])

readJSONVersioned :: forall o a. (a ~ Versioned o, JSON o, VersionNumber (PrevVersion a), JSONVersioned (PrevVersion a) a) => JSValue -> Result a
readJSONVersioned = readJSONVersioned' (Proxy :: Proxy (Just a))

instance (JSON a, VersionNumber (PrevVersion (Versioned a)), JSONVersioned (PrevVersion (Versioned a)) (Versioned a)) => JSON (Versioned a) where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned
