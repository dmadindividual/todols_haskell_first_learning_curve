{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Models where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (ToJSON, FromJSON)

-- | Represents a task item with metadata
data TaskItem = TaskItem
    { taskId      :: Int       -- ^ Unique task identifier
    , taskName    :: Text      -- ^ Name or description of the task
    , isDone      :: Bool      -- ^ Whether the task is completed
    , createdAt   :: UTCTime   -- ^ Time of creation or last update
    } deriving (Show, Eq, Generic)

instance ToJSON TaskItem
instance FromJSON TaskItem

-- | Used for submitting a new task entry
newtype TaskInput = TaskInput
    { taskNameInput :: Text  -- ^ Text for the new task
    } deriving (Generic)

instance ToJSON TaskInput
instance FromJSON TaskInput
