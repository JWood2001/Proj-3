// Parser/Parser.fs

module Parser.Parsing

open FParsec
open Parser.AST

/// Whitespace
let ws     = spaces
let str_ws s = pstring s .>> ws

/// Identifier that cannot be a keyword
let identifier : Parser<string,unit> =
  let isStart c = isLetter c
  let isChar  c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isStart isChar "identifier" .>> ws
  >>= fun id ->
    match id with
    | "let" | "in" | "fun" 
      -> fail $"keyword '{id}' cannot be used as an identifier"
    | _ -> preturn id

/// Integer literal
let pint_ws = pint32 .>> ws |>> Expr.IntLit

/// Forward recursion
let expr, exprRef = createParserForwardedToRef<Expr,unit>()

/// Parenthesized expression
let parenExpr = between (str_ws "(") (str_ws ")") expr

/// Atomic term (no application)
let atom =
  choice [
    attempt pint_ws
    attempt (identifier |>> Expr.Var)
    parenExpr
  ]

/// Application: one or more atoms
let application =
  many1 atom
  |>> List.reduce (fun f x -> Expr.App(f, x))

/// Lambda: fun x -> expr
let lambdaParser =
  str_ws "fun" >>. identifier .>> str_ws "->"
  .>>. expr
  |>> fun (arg, body) -> Expr.Lambda(arg, body)

/// Let-binding: let x = expr in expr
let letParser =
  str_ws "let" >>. identifier .>> str_ws "=" .>>. expr
  .>> str_ws "in" .>>. expr
  |>> fun ((name, value), body) -> Expr.Let(name, value, body)

/// Top-level expr: prefer let → lambda → application
do exprRef.Value <- ws >>. choice [
  attempt letParser
  attempt lambdaParser
  application
]

/// Public API: require EOF after expr
let parse (input:string) : Result<Expr,string> =
  match run (expr .>> eof) input with
  | Success(res, _, _) -> Result.Ok res
  | Failure(err, _, _) -> Result.Error err

