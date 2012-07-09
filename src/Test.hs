{-# LANGUAGE
    TemplateHaskell
  , GeneralizedNewtypeDeriving
  , EmptyDataDecls
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , TypeFamilies
  #-}
module Test where

import Data.Binary (Binary (..))
import Generics.Regular
import Generics.Regular.JSON
import Text.JSON (JSON (..))
import qualified Text.JSON   as JSON
import qualified Data.Binary as Binary
import qualified Generics.Regular.Functions.Binary as G

import Data.Binary.Versioned
import Migrate
import Text.JSON.Versioned

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

instance JSON User0' where
  showJSON = gshowJSON
  readJSON = greadJSON

type User0 = Versioned User0'

instance Binary User0 where
  get = getVersioned
  put = putVersioned

instance JSON User0 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

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

instance JSON User1' where
  showJSON = gshowJSON
  readJSON = greadJSON

type User1 = Versioned User1'

instance Binary User1 where
  get = getVersioned
  put = putVersioned

instance JSON User1 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion User1 = Just User0
instance Migrate User0 User1 where
  migrate (Versioned u0) = Versioned $ User1
    { name1     = name0 u0
    , password1 = password0 u0
    , admin1    = False
    }

-------------------------------------------------------------------------------

newtype Name = Name String deriving (Eq, Show, Binary, JSON)
newtype Password = Password String deriving (Eq, Show, Binary, JSON)
data Role = NormalUser | PowerUser | Admin deriving (Eq, Show)

$(deriveAll ''Role "PFRole")
type instance PF Role = PFRole

instance Binary Role where
  get = G.gget
  put = G.gput

instance JSON Role where
  showJSON = gshowJSON
  readJSON = greadJSON

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

instance JSON User2' where
  showJSON = gshowJSON
  readJSON = greadJSON

type User2 = Versioned User2'

instance Binary User2 where
  get = getVersioned
  put = putVersioned

instance JSON User2 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion User2 = Just User1
instance Migrate User1 User2 where
  migrate (Versioned u1) = Versioned $ User2
    { name2     = Name (name1 u1)
    , password2 = Password (password1 u1)
    , role2     = if admin1 u1 then Admin else NormalUser
    }

-------------------------------------------------------------------------------

data Address0' = Address0
  { street0 :: String
  , city0   :: String
  } deriving (Eq, Show)

$(deriveAll ''Address0' "PFAddress0'")
type instance PF Address0' = PFAddress0'

instance Binary Address0' where
  get = G.gget
  put = G.gput

instance JSON Address0' where
  showJSON = gshowJSON
  readJSON = greadJSON

type Address0 = Versioned Address0'

instance Binary Address0 where
  get = getVersioned
  put = putVersioned

instance JSON Address0 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion Address0 = Nothing

data User3' = User3
  { name3     :: Name
  , password3 :: Password
  , role3     :: Role
  , address3  :: Address0
  } deriving (Eq, Show)

$(deriveAll ''User3' "PFUser3'")
type instance PF User3' = PFUser3'

instance Binary User3' where
  get = G.gget
  put = G.gput

instance JSON User3' where
  showJSON = gshowJSON
  readJSON = greadJSON

type User3 = Versioned User3'

instance Binary User3 where
  get = getVersioned
  put = putVersioned

instance JSON User3 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion User3 = Just User2
instance Migrate User2 User3 where
  migrate (Versioned u2) = Versioned $ User3
    { name3     = name2     u2
    , password3 = password2 u2
    , role3     = role2     u2
    , address3  = Versioned (Address0 "" "")
    }

-------------------------------------------------------------------------------

data Address1' = Address1
  { street1  :: String
  , city1    :: String
  , country1 :: String
  } deriving (Eq, Show)

$(deriveAll ''Address1' "PFAddress1'")
type instance PF Address1' = PFAddress1'

instance Binary Address1' where
  get = G.gget
  put = G.gput

instance JSON Address1' where
  showJSON = gshowJSON
  readJSON = greadJSON

type Address1 = Versioned Address1'

instance Binary Address1 where
  get = getVersioned
  put = putVersioned

instance JSON Address1 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion Address1 = Just Address0
instance Migrate Address0 Address1 where
  migrate (Versioned a0) = Versioned $ Address1
    { street1   = street0 a0
    , city1     = city0   a0
    , country1  = ""
    }

data User4' = User4
  { name4     :: Name
  , password4 :: Password
  , role4     :: Role
  , address4  :: Address1
  } deriving (Eq, Show)

$(deriveAll ''User4' "PFUser4'")
type instance PF User4' = PFUser4'

instance Binary User4' where
  get = G.gget
  put = G.gput

instance JSON User4' where
  showJSON = gshowJSON
  readJSON = greadJSON

type User4 = Versioned User4'

instance Binary User4 where
  get = getVersioned
  put = putVersioned

instance JSON User4 where
  showJSON = showJSONVersioned
  readJSON = readJSONVersioned

type instance PrevVersion User4 = Just User3
instance Migrate User3 User4 where
  migrate (Versioned u3) = Versioned $ User4
    { name4     = name3     u3
    , password4 = password3 u3
    , role4     = role3     u3
    , address4  = migrate (address3 u3)
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

user3 :: Versioned User3'
user3  = Versioned $ User3 { name3 = Name "Erik Hesselink", password3 = Password "password", role3 = NormalUser, address3 = Versioned (Address0 "" "") }

user3_ :: Versioned User3'
user3_  = Versioned $ User3 { name3 = Name "Erik Hesselink", password3 = Password "password", role3 = NormalUser, address3 = Versioned (Address0 "Spuistraat" "Amsterdam") }

user4 :: Versioned User4'
user4  = Versioned $ User4 { name4 = Name "Erik Hesselink", password4 = Password "password", role4 = NormalUser, address4 = Versioned (Address1 "" "" "") }

user4_ :: Versioned User4'
user4_  = Versioned $ User4 { name4 = Name "Erik Hesselink", password4 = Password "password", role4 = NormalUser, address4 = Versioned (Address1 "Spuistraat" "Amsterdam" "") }

tests :: [Bool]
tests =
  [ Binary.decode (Binary.encode user0)  == user1
  , Binary.decode (Binary.encode user0)  == user2
  , Binary.decode (Binary.encode user1)  == user2
  , Binary.decode (Binary.encode user1_) == user2_
  , Binary.decode (Binary.encode user0)  == user3
  , Binary.decode (Binary.encode user1)  == user3
  , Binary.decode (Binary.encode user2)  == user3
  , Binary.decode (Binary.encode user0)  == user4
  , Binary.decode (Binary.encode user1)  == user4
  , Binary.decode (Binary.encode user2)  == user4
  , Binary.decode (Binary.encode user3)  == user4
  , Binary.decode (Binary.encode user3_) == user4_
  , JSON.decode (JSON.encode user0)  == JSON.Ok user1
  , JSON.decode (JSON.encode user0)  == JSON.Ok user2
  , JSON.decode (JSON.encode user1)  == JSON.Ok user2
  , JSON.decode (JSON.encode user1_) == JSON.Ok user2_
  , JSON.decode (JSON.encode user0)  == JSON.Ok user3
  , JSON.decode (JSON.encode user1)  == JSON.Ok user3
  , JSON.decode (JSON.encode user2)  == JSON.Ok user3
  , JSON.decode (JSON.encode user0)  == JSON.Ok user4
  , JSON.decode (JSON.encode user1)  == JSON.Ok user4
  , JSON.decode (JSON.encode user2)  == JSON.Ok user4
  , JSON.decode (JSON.encode user3)  == JSON.Ok user4
  , JSON.decode (JSON.encode user3_) == JSON.Ok user4_
  ]
