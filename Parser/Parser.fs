// Parser/Parser.fs

module Parser.Parsing

open FParsec
open Parser.AST

/// Consume trailing whitespace
let ws     = spaces
let str_ws s = pstring s .>> ws

/// Parse an identifier that is **not** a reserved keyword
let identifier : Parser<string,unit> =
  let isStart c = isLetter c
  let isChar  c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isStart isChar "identifier"
  .>> ws
  >>= fun id ->
    // Exclude reserved words
    match id with
    | "let" | "in" | "fun" -> fail $"keyword '{id}' cannot be used as an identifier"
    | _                   -> preturn id

/// Parse a 32-bit integer literal
let pint_ws = pint32 .>> ws |>> Expr.IntLit

/// Forward declaration of `expr` so we can be recursive
let expr, exprRef = createParserForwardedToRef<Expr,unit>()

/// Parenthesized sub-expression: `( expr )`
let parenExpr = between (str_ws "(") (str_ws ")") expr

/// A simple atomic term (no application)
let atom =
  choice [
    attempt pint_ws
    attempt (identifier |>> Expr.Var)
    parenExpr
  ]

/// Function application: one or more atoms, left-associative
let application =
  many1 atom
  |>> List.reduce (fun f x -> Expr.App(f, x))

/// Lambda: `fun x -> expr`
let lambdaParser =
  str_ws "fun" >>. identifier .>> str_ws "->"
  .>>. expr
  |>> fun (arg, body) -> Expr.Lambda(arg, body)

/// Let-binding: `let x = expr in expr`
let letParser =
  str_ws "let" >>. identifier .>> str_ws "=" .>>. expr
  .>> str_ws "in" .>>. expr
  |>> fun ((name, value), body) -> Expr.Let(name, value, body)

/// Define `expr` to first try let, then lambda, then application
do exprRef.Value <- ws >>. choice [
  attempt letParser
  attempt lambdaParser
  application
]

/// Top-level parse: require EOF after the expression
let parse (input:string) : Result<Expr,string> =
  match run (expr .>> eof) input with
  | Success(res, _, _) -> Result.Ok    res
  | Failure(err, _, _) -> Result.Error err
