

```markdown
#  Haskell Todo List Application

A lightweight, functional Todo List web application built entirely in Haskell. This project showcases full-stack development using idiomatic Haskell practices, serving as a learning milestone in the journey of mastering Haskell for web development.

It combines server-side rendering, type-safe API design, and minimal JavaScript interactivity via HTMX.

---

## Features

- Create, update, and delete todo items
- Fully server-rendered UI with [Blaze](https://hackage.haskell.org/package/blaze-html)
- Clean, responsive design using [Tailwind CSS](https://tailwindcss.com/) (via CDN)
- Dynamic interactions using [HTMX](https://htmx.org/)
- In-memory state management using `IORef`
- Type-safe API structure using [Servant](https://hackage.haskell.org/package/servant)
- Built-in CORS support and request logging


## Technologies Used

| Layer        | Tool/Library |
|--------------|--------------|
| Language     | Haskell |
| API Routing  | Servant |
| HTML Rendering | Blaze |
| State Management | `IORef` |
| Frontend Interactivity | HTMX |
| Styling | Tailwind CSS |
| Middleware | `wai-cors`, request logging |

---


Make sure you have:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/) **or** [Cabal](https://www.haskell.org/cabal/)

### 🔧 Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/dmadindividual/todols_haskell_first_learning_curve.git
   cd todols_haskell_first_learning_curve
   ```

2. Build and run the project:

   With **Stack**:

   ```bash
   stack build
   stack exec todols-haskell-first-learning-curve-exe
   ```

   With **Cabal**:

   ```bash
   cabal build
   cabal run
   ```

3. Open your browser and navigate to:

   ```
   http://localhost:8081
   ```

---

##  Project Structure

```
src/
├── EntryPoint.hs              -- Application entry and server setup
├── Todos/
│   ├── Models.hs          -- API routes and type definitions
│   ├── Router.hs        -- Data types for todos and forms
│   ├── Render.hs        -- Blaze-based HTML rendering
```

---

## 🔍 How It Works

- **Todos.Router**: Defines API routes using Servant.
- **Todos.Models**: Houses data types like `Todo` and `CreateTodoForm`.
- **Todos.Render**: Generates dynamic and partial HTML using Blaze.
- **EntryPoint.hs**: Bootstraps the server, sets up CORS, and initializes the `IORef` state.

HTMX enables asynchronous UI updates through HTML attributes—no JavaScript framework is required.

---

## 🧪 Development Notes

- Todos are stored in-memory (`IORef`) and will reset on server restart.
- Tailwind CSS is loaded via CDN for quick styling.
- HTMX handles interactivity declaratively—perfect for quick iterations.

---

##  Future Improvements

- Persistent storage (e.g., SQLite or PostgreSQL)
- Edit functionality for todos
- Filtering (All / Active / Completed)
- User authentication and sessions
- Add unit/integration tests with HSpec or Servant Test

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## 👤 Author

Created by [@dmadindividual](https://github.com/dmadindividual) — a first milestone in full-stack Haskell exploration.
```
