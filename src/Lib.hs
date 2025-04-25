{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
    startApp,
    app,
    generateTodos,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time (UTCTime, getCurrentTime)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Text.Blaze.Html (Html)
import Todos.Api
import Todos.Types (Todo (..))
import Todos.Views (renderPage, renderTodoItem, renderTodoList)

generateTodos :: Maybe UTCTime -> IO [Todo]
generateTodos mNow = do
    now <- maybe getCurrentTime pure mNow
    let titles = ["Read Boook: Learn you a haskell great good", "Learn Basic fundamentals of haskell", "Text Yink about the haskell Project"]
        todos = [Todo i (pack title) False now | (i, title) <- zip [1 ..] titles]
    return todos

server :: IORef [Todo] -> Server API
server todoRef = handleRoot :<|> handleTodoApi
  where
    handleRoot :: Handler Html
    handleRoot = do
        todos <- liftIO $ readIORef todoRef
        return $ renderPage todos

    handleTodoApi :: Server TodoAPI
    handleTodoApi = handleGetTodos :<|> handlePostTodo :<|> handleGetTodo :<|> handlePutTodo :<|> handleDeleteTodo

    handleGetTodos :: Handler Html
    handleGetTodos = do
        todos <- liftIO $ readIORef todoRef
        return $ renderTodoList todos

    handleGetTodo :: Int -> Handler Html
    handleGetTodo tid = do
        todos <- liftIO $ readIORef todoRef
        case find (\t -> todoId t == tid) todos of
            Just todo -> return $ renderTodoItem todo
            Nothing -> throwError err404{errBody = "Todo not found"}

    handlePostTodo :: CreateTodoForm -> Handler Html
    handlePostTodo form = do
        now <- liftIO getCurrentTime
        _ <- liftIO $ atomicModifyIORef' todoRef $ \todos -> 
            let newId = if null todos then 1 else maximum (map todoId todos) + 1
                theNewTodo = Todo newId (createTitle form) False now
                newList = todos ++ [theNewTodo]
             in (newList, theNewTodo)
        allTodos <- liftIO $ readIORef todoRef
        return $ renderTodoList allTodos

    handlePutTodo :: Int -> TodoUpdateForm -> Handler Html
    handlePutTodo tid form = do
        now <- liftIO getCurrentTime
        let mNewCompletedStatus = completedStatus form  -- Referencing 'completedStatus' here

        mUpdatedTodo <- liftIO $ atomicModifyIORef' todoRef $ \todos -> 
            let (found, updatedTodos) = updateTodoInList tid form mNewCompletedStatus now todos
                maybeUpdatedTodo = if found then find (\t -> todoId t == tid) updatedTodos else Nothing
             in (updatedTodos, maybeUpdatedTodo) -- Return (newState, result)
        case mUpdatedTodo of
            Just updatedTodo -> return $ renderTodoItem updatedTodo
            Nothing -> throwError err404{errBody = "Todo not found for update"}

    handleDeleteTodo :: Int -> Handler Html
    handleDeleteTodo tid = liftIO $ do
        atomicModifyIORef' todoRef (\todos -> (filter (\t -> todoId t /= tid) todos, ()))
        updatedTodos <- readIORef todoRef
        return $ renderTodoList updatedTodos

-- Helper function for updating list (using mapAccumL)
updateTodoInList :: Int -> TodoUpdateForm -> Maybe Bool -> UTCTime -> [Todo] -> (Bool, [Todo])
updateTodoInList tid _ mNewCompletedStatus now =
    mapAccumL processTodo False
  where
    processTodo :: Bool -> Todo -> (Bool, Todo)
    processTodo alreadyFound t
        | alreadyFound = (True, t)
        | todoId t == tid =
            let updatedTodo =
                    t
                        { todoCompleted = fromMaybe (todoCompleted t) mNewCompletedStatus
                        , timestamp = now
                        }
             in (True, updatedTodo)
        | otherwise = (False, t)

-- API Proxy
api :: Proxy API
api = Proxy

-- CORS Middleware
customCors :: Middleware
customCors = cors (const $ Just policy)
  where
    policy =
        simpleCorsResourcePolicy
            { corsOrigins = Nothing
            , corsRequestHeaders = ["Content-Type", "Accept"]
            , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
            }

app :: IORef [Todo] -> Application
app ref = customCors $ serve api (server ref)

port :: Int
port = 8081

startApp :: IO ()
startApp = do
    putStrLn $ "Starting server on http://localhost:" ++ show port
    initialTodos <- generateTodos Nothing
    ref <- newIORef initialTodos
    run port (logStdoutDev $ app ref)
