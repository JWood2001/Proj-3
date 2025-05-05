module WebApp.Program

open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting

open Giraffe
open Giraffe.ViewEngine

open Parser.AST
open Parser.Parsing

/// Recursively render an Expr as a nested <ul>…</ul>
let rec renderList (expr: Expr) =
    let children =
        match expr with
        | IntLit _ | Var _    -> []
        | Lambda(_, body)     -> [ renderList body ]
        | App(f, x)           -> [ renderList f; renderList x ]
        | Let(_, v, b)        -> [ renderList v; renderList b ]

    ul [ _class "ast" ] [
      li [] [
        span [] [ str (sprintf "%A" expr) ]
        for c in children do yield c
      ]
    ]

let webApp : HttpHandler =
    choose [
      GET  >=> htmlFile "Views/index.html"
      POST >=> fun next ctx -> task {
        let! form = ctx.BindFormAsync()
        let code = form.["code"]
        match parse code with
        | Result.Ok ast ->
            let view = renderList ast
            // wrap in a full HTML document:
            let htmlDoc =
              html [] [
                head [] [
                  title [] [ str "MicroML AST" ]
                  link [ _rel "stylesheet"; _href "/styles.css" ]
                ]
                body [] [
                  view
                ]
              ]
            return! htmlString (RenderView.AsString.htmlDocument htmlDoc) next ctx

        | Result.Error err ->
            return! htmlString $"<p class='error'>Parse error: {err}</p>" next ctx
      }
    ]

[<EntryPoint>]
let main args =
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun web ->
            web
              .UseUrls("http://localhost:5000")
              .Configure(fun app ->
                app.UseStaticFiles() |> ignore
                app.UseGiraffe webApp
              )
        )
        .Build()
        .Run()

    0


