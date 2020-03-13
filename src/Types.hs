{-# LANGUAGE TemplateHaskell #-}

-- |

module Types (Condition(..)) where

import           Control.Monad.Catch (SomeException)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Pool
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Database.Persist.Postgresql
import           Lucid
import           Text.Lucius
import           Yesod hiding (Html)
import           Yesod.Lucid


data Condition
  = Asymptomatic
  | Isolating
  | Recovered
  deriving (Eq, Show, Read)
$(derivePersistField "Condition")
