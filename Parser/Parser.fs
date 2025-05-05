// Parser/Parser.fs

module Parser.Parsing

open FParsec
open Parser.AST

/// Whitespace parser
let ws     = spaces
let str_ws s = pstring s .>> ws

/// Identifier: letter (letter|digit|_)*
let identifier =
  let isStart c = isLetter c
  let isChar  c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isStart isChar "identifier" .>> ws

/// Integer literal parser
let pint_ws = pint32 .>> ws |>> Expr.IntLit

/// Forward declaration for recursive expressions
let expr, exprRef = createParserForwardedToRef<Expr,unit>()

/// Parenthesized parser helper
let paren p = between (str_ws "(") (str_ws ")") p

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

/// Simple term: literal, var, lambda, let, or parenthesized
let simple =
  choice [
    attempt pint_ws
    attempt lambdaParser
    attempt letParser
    identifier |>> Expr.Var
    paren expr
  ]

/// Function application: chain of simple terms
let app =
  many1 simple
  |>> (List.reduce (fun f x -> Expr.App(f, x)))

/// Hook up the recursive reference (use .Value <- per F# refcell)
do exprRef.Value <- ws >>. app .>> eof

/// Public parse function returning a Result<Expr,string>
let parse (input:string) : Result<Expr,string> =
  match run expr input with
  | Success(res, _, _) -> Result.Ok    res
  | Failure(err, _, _) -> Result.Error err




