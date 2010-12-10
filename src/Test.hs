{-# LANGUAGE
    TemplateHaskell
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , EmptyDataDecls
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , TypeFamilies
  , StandaloneDeriving
  #-}
module Test where

import Data.Binary
import Generics.Regular
import qualified Generics.Regular.Functions.Binary as G

import Migrate
import Data.Binary.Versioned

-------------------------------------------------------------------------------

data User0' = User0
  { name0     :: String
  , password0 :: String
  } deriving (Eq, Show)

$(deriveAll ''User0' "PFUser0'")
type instance PF User0' = PFUser0'

instance Binary User0' where
  get = G.gget
  put = G.gput

type User0 = Versioned User0'

instance Binary User0 where
  get = getVersioned
  put = putVersioned

type instance PrevVersion User0 = Nothing

-------------------------------------------------------------------------------

data User1' = User1
  { name1     :: String
  , password1 :: String
  , admin1    :: Bool
  } deriving (Show, Eq)

$(deriveAll ''User1' "PFUser1'")
type instance PF User1' = PFUser1'

instance Binary User1' where
  get = G.gget
  put = G.gput

type User1 = Versioned User1'

instance Binary User1 where
  get = getVersioned
  put = putVersioned

type instance PrevVersion User1 = Just User0
instance Migrate User0 User1 where
  migrate (Versioned u0) = Versioned $ User1
    { name1     = name0 u0
    , password1 = password0 u0
    , admin1    = False
    }

-------------------------------------------------------------------------------

newtype Name = Name String deriving (Eq, Show, Binary)
newtype Password = Password String deriving (Eq, Show, Binary)
data Role = NormalUser | PowerUser | Admin deriving (Eq, Show)

$(deriveAll ''Role "PFRole")
type instance PF Role = PFRole

instance Binary Role where
  get = G.gget
  put = G.gput

data User2' = User2
  { name2     :: Name
  , password2 :: Password
  , role2     :: Role
  } deriving (Eq, Show)

$(deriveAll ''User2' "PFUser2'")
type instance PF User2' = PFUser2'

instance Binary User2' where
  get = G.gget
  put = G.gput

type User2 = Versioned User2'

instance Binary User2 where
  get = getVersioned
  put = putVersioned

type instance PrevVersion User2 = Just User1
instance Migrate User1 User2 where
  migrate (Versioned u1) = Versioned $ User2
    { name2     = Name (name1 u1)
    , password2 = Password (password1 u1)
    , role2     = if admin1 u1 then Admin else NormalUser
    }

-------------------------------------------------------------------------------

user0 :: Versioned User0'
user0  = Versioned $ User0 { name0 = "Erik Hesselink", password0 = "password" }

user1 :: Versioned User1'
user1  = Versioned $ User1 { name1 = "Erik Hesselink", password1 = "password", admin1 = False }

user1_ :: Versioned User1'
user1_ = Versioned $ User1 { name1 = "Erik Hesselink", password1 = "password", admin1 = True }

user2 :: Versioned User2'
user2  = Versioned $ User2 { name2 = Name "Erik Hesselink", password2 = Password "password", role2 = NormalUser }

user2_ :: Versioned User2'
user2_ = Versioned $ User2 { name2 = Name "Erik Hesselink", password2 = Password "password", role2 = Admin }

tests :: (Bool, Bool, Bool, Bool)
tests = (test0, test1, test2, test3)
  where
    test0 = decode (encode user0) == user1
    test1 = decode (encode user0) == user2
    test2 = decode (encode user1) == user2
    test3 = decode (encode user1_) == user2_
