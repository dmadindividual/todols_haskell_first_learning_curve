{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Router where

import GHC.Generics (Generic)
import Servant
import Servant.HTML.Blaze ()
import Data.Text (Text)
import Network.HTTP.Media ((//), (/:))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html)
import Web.FormUrlEncoded (FromForm)

-- Custom HTML response type
data HtmlPage

instance Accept HtmlPage where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HtmlPage Html where
    mimeRender _ = renderHtml

-- Incoming form data types
newtype NewTaskForm = NewTaskForm
    { taskTitle :: Text
    } deriving (Show, Generic)

instance FromForm NewTaskForm

newtype ToggleStatusForm = ToggleStatusForm
    { taskDone :: Maybe Bool
    } deriving (Show, Generic)

instance FromForm ToggleStatusForm

-- Route definitions
type HomePage =
    Get '[HtmlPage] Html

type AllTasks =
    Get '[HtmlPage] Html

type TaskDetail =
    Capture "taskId" Int :> Get '[HtmlPage] Html

type AddTask =
    ReqBody '[FormUrlEncoded] NewTaskForm
        :> Post '[HtmlPage] Html

type ModifyTask =
    Capture "taskId" Int
        :> ReqBody '[FormUrlEncoded] ToggleStatusForm
        :> Put '[HtmlPage] Html

type RemoveTask =
    Capture "taskId" Int
        :> Delete '[HtmlPage] Html

-- Grouped API
type TaskRoutes =
    "tasks" :> (
        AllTasks
        :<|> AddTask
        :<|> TaskDetail
        :<|> ModifyTask
        :<|> RemoveTask
    )

type AppRoutes = HomePage :<|> TaskRoutes
