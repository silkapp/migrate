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
module Data.Binary.Versioned
( module Data.Versioned
, putVersioned
, getVersioned
)
where

import Control.Applicative
import Control.Exception
import Data.Binary
import Data.Binary.Get (lookAhead)
import Migrate

import qualified Data.Label as L

import Data.Versioned

class GetVersioned a r where
  getVersioned' :: Proxy a -> Get r

-- If there is no previous version, we can't do anything.

instance GetVersioned Nothing r where
  getVersioned' _ = throw NoPreviousVersion

-- We try to get the 'a', which is PrevVersion r. If that doesn't work, we
-- recurse trying to get older versions of 'a'. Finally, we migrate the 'a' to
-- an 'r'.

instance ( a ~ Versioned o
         , Binary o
         , Migrate a r
         , VersionNumber (PrevVersion a)
         , GetVersioned (PrevVersion a) a
         ) => GetVersioned (Just (Versioned o)) r where
  getVersioned' _ =
    do let v = version (Proxy :: Proxy a)
       w <- lookAhead get
       migrate <$>
         case compare w v of
           LT -> getVersioned' (Proxy :: Proxy (PrevVersion a)) :: Get a
           EQ -> do (_ :: Int) <- get
                    Versioned <$> get
           GT -> throw (VersionMismatch v w)

instance GetVersioned (Just (FixedVersion n)) r where
  getVersioned' _ = error "GetVersioned: FixedVersion found instead of previous version."

putVersioned :: forall a o. (a ~ Versioned o, Binary o, VersionNumber (PrevVersion a)) => a -> Put
putVersioned a =
  do let v = version (Proxy :: Proxy a)
     put v
     put (L.get versioned a)

getVersioned :: forall o a. (a ~ Versioned o, Binary o, VersionNumber (PrevVersion a), GetVersioned (PrevVersion a) a) => Get a
getVersioned = getVersioned' (Proxy :: Proxy (Just a))

