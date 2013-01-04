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
  , CPP
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
#if !MIN_VERSION_binary(0,6,0)
import Data.Binary.Get (lookAhead)
#endif
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
#if MIN_VERSION_binary(0,6,0)
    let v = version (Proxy :: Proxy a)
        getCurrent :: Get a
        getCurrent =
          do w <- get
             case compare w v of
               LT -> empty
               EQ -> Versioned <$> get
               GT -> throw (VersionMismatch v w)
        getOlder :: Get a
        getOlder = getVersioned' (Proxy :: Proxy (PrevVersion a)) :: Get a
    in migrate <$> (getCurrent <|> getOlder)
#else
    do let v = version (Proxy :: Proxy a)
       w <- lookAhead get
       migrate <$>
         case compare w v of
           LT -> getVersioned' (Proxy :: Proxy (PrevVersion a)) :: Get a
           EQ -> do (_ :: Int) <- get
                    Versioned <$> get
           GT -> throw (VersionMismatch v w)
#endif

instance GetVersioned (Just (FixedVersion n)) r where
  getVersioned' _ = error "GetVersioned: FixedVersion found instead of previous version."

putVersioned :: forall a o. (a ~ Versioned o, Binary o, VersionNumber (PrevVersion a)) => a -> Put
putVersioned a =
  do let v = version (Proxy :: Proxy a)
     put v
     put (L.get versioned a)

getVersioned :: forall o a. (a ~ Versioned o, Binary o, VersionNumber (PrevVersion a), GetVersioned (PrevVersion a) a) => Get a
getVersioned = getVersioned' (Proxy :: Proxy (Just a))

