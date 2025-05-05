# MicroML AST Visualizer

A simple F#/.NET 9 web app using Giraffe that parses MicroML code and renders its Abstract Syntax Tree (AST) on a single page.

---

## Features

- **MicroML grammar** supporting:
  - Integer literals
  - Variables
  - Lambdas (`fun x -> …`)
  - Let-bindings (`let x = … in …`)
  - Nested and higher-order functions
  - Application of functions
- **Live AST rendering** as a styled HTML `<ul>` tree
- **Single-page interface**: input and output on the same screen  
- **Minimal dependencies**: FParsec for parsing, Giraffe for web

---

## Prerequisites

- [.NET 9.0 SDK](https://dotnet.microsoft.com/download)  
- Git (for version control)

---

## Getting Started

```bash
# Clone the repo
git clone https://github.com/yourusername/microml-ast-visualizer.git
cd microml-ast-visualizer

# Restore dependencies
dotnet restore

# Run the web app
dotnet run --project WebApp
