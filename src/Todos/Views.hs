{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Todos.Views where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Todos.Types (Todo (..))
import Prelude hiding (div, span)

-- Render the full HTML page
renderPage :: [Todo] -> Html
renderPage todos = docTypeHtml $ do
    H.head $ do
        meta ! charset "UTF-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        H.title "Todo App"
        script ! src "https://unpkg.com/htmx.org@2.0.4" ! customAttribute "integrity" "sha384-HGfztofotfshcF7+8n44JQL2oJmowVChPTg48S+jvZoztPfvwD79OC/LTtG6dMp+" ! customAttribute "crossorigin" "anonymous" $ ""
        script ! src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4" $ ""
    body ! class_ "bg-gray-100 text-gray-900 min-h-screen p-6 flex flex-col items-center justify-center" $ do
        H.div ! class_ "min-w-[600px] max-w-2xl mx-auto bg-white p-8 rounded-lg shadow-2xl" $ do
            h1 ! class_ "text-4xl font-extrabold mb-6 text-teal-600" $ "Todo App"
            -- New Todo Form
            H.form
                ! A.id "new-todo-form"
                ! class_ "flex gap-4 mb-6"
                ! H.customAttribute "hx-post" "/todos"
                ! H.customAttribute "hx-target" "#todo-list"
                ! H.customAttribute "hx-swap" "innerHTML"
                ! H.customAttribute "hx-on::after-request" "this.reset()"
                $ do
                    input
                        ! A.type_ "text"
                        ! A.name "createTitle"
                        ! A.id "newTodoTitle"
                        ! placeholder "Add a new todo..."
                        ! required ""
                        ! class_ "flex-1 px-4 py-3 border-2 border-teal-400 rounded-md focus:outline-none focus:ring-2 focus:ring-teal-500"
                    button
                        ! A.type_ "submit"
                        ! class_ "bg-teal-600 text-white px-6 py-3 rounded-md hover:bg-teal-700 transition-colors"
                        $ "Add"
            -- Todo List Container
            H.div ! A.id "todo-list" ! class_ "max-h-[500px] overflow-y-auto" $ do
                renderTodoList todos

-- Render the list of todos (used for initial load and updates)
renderTodoList :: [Todo] -> Html
renderTodoList [] = H.div ! class_ "text-center text-gray-500" $ "There are no todos, add one now."
renderTodoList todos = mconcat $ Prelude.map renderTodoItem todos

-- Render a single todo item
renderTodoItem :: Todo -> Html
renderTodoItem todo =
    H.div
        ! A.id (stringValue $ "todo-" ++ show (todoId todo))
        ! class_ "group relative py-5 flex items-center justify-between p-4 rounded-lg border-2 border-teal-400 my-4 shadow-lg hover:shadow-2xl transition duration-300 ease-in-out"
        $ do
            -- Left side (checkbox and title)
            H.div ! class_ "flex items-center gap-4" $ do
                input
                    ! A.type_ "checkbox"
                    ! A.name "updateCompleted"
                    ! class_ "mr-3"
                    ! H.customAttribute "hx-put" (stringValue $ "/todos/" ++ show (todoId todo))
                    ! H.customAttribute "hx-target" (stringValue $ "#todo-" ++ show (todoId todo))
                    ! H.customAttribute "hx-swap" "outerHTML"
                    ! H.customAttribute "hx-trigger" "change"
                    ! H.customAttribute "hx-include" "this"
                    ! (if todoCompleted todo then checked "" else mempty)
                    ! H.customAttribute "name" "updateCompletedPresent"
                    ! H.customAttribute "value" "true"
                    ! H.customAttribute "type" "hidden"

                -- Apply strike-through with different color for completed tasks
                H.span
                    ! class_ (if todoCompleted todo then "line-through text-teal-500 font-semibold" else "text-lg")
                    $ toHtml (todoTitle todo)

            -- Right side (delete button)
            H.div ! class_ "flex items-center gap-4" $ do
                button
                    ! class_ "bg-red-600 text-white px-4 py-2 rounded-md hover:bg-red-700 hidden group-hover:block"
                    ! H.customAttribute "hx-delete" (stringValue $ "/todos/" ++ show (todoId todo))
                    ! H.customAttribute "hx-target" (stringValue $ "#todo-" ++ show (todoId todo))
                    ! H.customAttribute "hx-swap" "delete"
                    $ "Delete"
