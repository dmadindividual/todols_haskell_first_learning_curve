{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Data type representing a Todo item
data Todo = Todo
    { todoId        :: Int        -- ^ Unique identifier for the Todo item
    , todoTitle     :: Text       -- ^ Title of the Todo item
    , todoCompleted :: Bool       -- ^ Completion status of the Todo item
    , timestamp     :: UTCTime    -- ^ Timestamp when the Todo was created or last updated
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

-- | Data type for creating a new Todo item
newtype NewTodoDto = NewTodoDto
    { newTodoTitle :: Text  -- ^ Title for the new Todo item
    } deriving (Generic)

instance ToJSON NewTodoDto
instance FromJSON NewTodoDto


