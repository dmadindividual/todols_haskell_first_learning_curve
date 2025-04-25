{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EntryPoint (
    bootServer,
    webApp,
    loadMockTasks,
) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (find, mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time (UTCTime, getCurrentTime)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Todos.Router
import Todos.Models (TaskItem(..))
import Todos.Render (pageView, taskView, taskListView)
import Text.Blaze.Html (Html)


loadMockTasks :: Maybe UTCTime -> IO [TaskItem]
loadMockTasks mNow = do
    now <- maybe getCurrentTime pure mNow
    let taskNames =
            [ "Read Book: Learn You a Haskell"
            , "Study Haskell fundamentals"
            , "Ping Yinka about the Haskell side quest"
            ]
    return [TaskItem i (pack name) False now | (i, name) <- zip [1..] taskNames]

handleServer :: IORef [TaskItem] -> Server AppRoutes
handleServer store = rootPage :<|> taskHandlers
  where
    rootPage :: Handler Html
    rootPage = do
        tasks <- liftIO $ readIORef store
        return $ pageView tasks

    taskHandlers :: Server TaskRoutes
    taskHandlers =
        getAllTasks
        :<|> addTask
        :<|> getSingleTask
        :<|> modifyTask
        :<|> removeTask

    getAllTasks :: Handler Html
    getAllTasks = do
        tasks <- liftIO $ readIORef store
        return $ taskListView tasks

    getSingleTask :: Int -> Handler Html
    getSingleTask tid = do
        tasks <- liftIO $ readIORef store
        case find (\t -> taskId t == tid) tasks of
            Just task -> return $ taskView task
            Nothing   -> throwError err404{errBody = "Task not found"}

    addTask :: NewTaskForm -> Handler Html
    addTask form = do
        now <- liftIO getCurrentTime
        _ <- liftIO $ atomicModifyIORef' store $ \tasks ->
            let newId = if null tasks then 1 else maximum (map taskId tasks) + 1
                newEntry = TaskItem newId (taskTitle form) False now
             in (tasks ++ [newEntry], newEntry)
        updated <- liftIO $ readIORef store
        return $ taskListView updated

    modifyTask :: Int -> ToggleStatusForm -> Handler Html
    modifyTask tid form = do
        now <- liftIO getCurrentTime
        let newStatus = taskDone form
        mUpdated <- liftIO $ atomicModifyIORef' store $ \tasks ->
            let (found, updatedList) = applyUpdate tid form newStatus now tasks
                updatedOne = if found then find (\t -> taskId t == tid) updatedList else Nothing
             in (updatedList, updatedOne)
        case mUpdated of
            Just u -> return $ taskView u
            Nothing -> throwError err404{errBody = "No task found to update"}

    removeTask :: Int -> Handler Html
    removeTask tid = liftIO $ do
        atomicModifyIORef' store $ \tasks -> (filter (\t -> taskId t /= tid) tasks, ())
        refreshed <- readIORef store
        return $ taskListView refreshed

-- Functional update logic
applyUpdate :: Int -> ToggleStatusForm -> Maybe Bool -> UTCTime -> [TaskItem] -> (Bool, [TaskItem])
applyUpdate tid _ mStatus now =
    mapAccumL update False
  where
    update :: Bool -> TaskItem -> (Bool, TaskItem)
    update done task
        | done = (True, task)
        | taskId task == tid =
            let updated = task
                    { isDone = fromMaybe (isDone task) mStatus
                    , createdAt = now
                    }
             in (True, updated)
        | otherwise = (False, task)

-- Routing
apiProxy :: Proxy AppRoutes
apiProxy = Proxy

-- CORS
setupCors :: Middleware
setupCors = cors (const $ Just config)
  where
    config = simpleCorsResourcePolicy
        { corsOrigins = Nothing
        , corsRequestHeaders = ["Content-Type", "Accept"]
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        }

webApp :: IORef [TaskItem] -> Application
webApp dbRef = setupCors $ serve apiProxy (handleServer dbRef)

-- Launch
port :: Int
port = 8081

bootServer :: IO ()
bootServer = do
    putStrLn $ "App running at http://localhost:" ++ show port
    startingTasks <- loadMockTasks Nothing
    ref <- newIORef startingTasks
    run port (logStdoutDev $ webApp ref)
