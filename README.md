**https://github.com/dmadindividual/todols_haskell_first_learning_curve**

---

```markdown
# Haskell Todo List Application

This project is a simple and functional Todo List web application built using Haskell. It leverages the Servant framework for defining APIs, Blaze for HTML rendering, and HTMX for interactive frontend behavior without relying on JavaScript frameworks.

The application was created as part of an early learning curve in full-stack Haskell development, demonstrating how to build a complete web system using idiomatic Haskell principles.

---

## Features

- Create, update, and delete Todo items
- Server-side HTML rendering using Blaze
- Responsive UI styled with Tailwind CSS via CDN
- Interactive actions using HTMX for AJAX-style behavior
- In-memory storage using `IORef`
- Type-safe APIs defined using Servant

---

## Technologies Used

- **Language**: Haskell
- **Routing/API**: [Servant](https://hackage.haskell.org/package/servant)
- **HTML Rendering**: [Blaze](https://hackage.haskell.org/package/blaze-html)
- **State Management**: `IORef`
- **UI Interactivity**: [HTMX](https://htmx.org/)
- **Styling**: [Tailwind CSS](https://tailwindcss.com/) (via CDN)
- **Middleware**: CORS support, request logging via `wai` middlewares

---

## Getting Started

### Prerequisites

Ensure you have the following installed:

- [GHC (The Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Stack](https://docs.haskellstack.org/en/stable/README/) or [Cabal](https://www.haskell.org/cabal/)

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/dmadindividual/todols_haskell_first_learning_curve.git
   cd todols_haskell_first_learning_curve
   ```

2. Build and run the project using Stack:

   ```bash
   stack build
   stack exec todols-haskell-first-learning-curve-exe
   ```

   Or using Cabal:

   ```bash
   cabal build
   cabal run
   ```

3. Open your browser and visit:

   ```
   http://localhost:8081
   ```

---

## Project Structure

```
src/
├── Lib.hs               -- Application entry point and server logic
├── Todos/
│   ├── Api.hs           -- API type definitions using Servant
│   ├── Types.hs         -- Data types for Todo and form input
│   ├── Views.hs         -- Blaze HTML rendering functions
```

---

## How It Works

- **Todos.Api** defines the RESTful API endpoints.
- **Todos.Types** contains data structures used in the app (`Todo`, `CreateTodoForm`, etc.).
- **Todos.Views** renders full HTML pages and partial components using Blaze.
- **Lib.hs** initializes the app, sets up CORS, routes, and an in-memory `IORef` to manage todo state.

The frontend behavior (adding, updating, deleting todos) is handled using HTMX attributes embedded in the HTML. No additional JavaScript or frontend framework is required.

---

## Development Notes

- Todos are stored in memory using `IORef`. Data will be reset on server restart.
- Tailwind CSS is included via CDN for rapid prototyping and clean styling.
- HTMX is used to make the app interactive while keeping the frontend simple and declarative.

---

## Future Improvements

- Implement persistent storage (e.g., SQLite, PostgreSQL)
- Add todo editing functionality
- Introduce filtering (All / Active / Completed)
- Add authentication and user sessions
- Unit and integration testing using HSpec or Servant Test

---

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## Author

[dmadindividual](https://github.com/dmadindividual)
```

---
