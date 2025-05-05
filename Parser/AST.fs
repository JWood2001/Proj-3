// Parser/AST.fs

module Parser.AST

open System

/// A MicroML expression
type Expr =
  | IntLit of int
  | Var    of string
  | Lambda of string * Expr
  | App    of Expr * Expr
  | Let    of string * Expr * Expr

/// Pretty-print an Expr as an indented tree
let toString (expr: Expr) : string =
  let rec go indent expr =
    let pad = String.replicate (indent * 2) " "
    match expr with
    | IntLit n ->
        pad + sprintf "IntLit %d\n" n
    | Var v ->
        pad + sprintf "Var %s\n" v
    | Lambda(arg, body) ->
        pad + sprintf "Lambda %s\n" arg
        + go (indent + 1) body
    | App(f, x) ->
        pad + "App\n"
        + go (indent + 1) f
        + go (indent + 1) x
    | Let(name, value, body) ->
        pad + sprintf "Let %s\n" name
        + go (indent + 1) value
        + go (indent + 1) body
  go 0 expr


