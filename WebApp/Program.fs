// WebApp/Program.fs

module WebApp.Program

open System
open Giraffe
open Giraffe.ViewEngine
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.AspNetCore.Http

open Parser.AST
open Parser.Parsing

/// Recursively render an Expr as a nested <ul class="ast">
let rec renderList (expr: Expr) : XmlNode =
    let children =
        match expr with
        | IntLit _ | Var _    -> []
        | Lambda(_, b)        -> [ renderList b ]
        | App(f, x)           -> [ renderList f; renderList x ]
        | Let(_, v, b)        -> [ renderList v; renderList b ]

    ul [ _class "ast" ] [
      li [] (
        [ span [] [ str (sprintf "%O" expr) ] ] @ children
      )
    ]

/// The main Giraffe HTTP handler
let webApp : HttpHandler =
    choose [
      GET  >=> htmlFile "Views/index.html"
      POST >=> fun next ctx ->
        task {
           // Read the form collection (known IFormCollection type)
          let! form = ctx.Request.ReadFormAsync()
          // Now form has type IFormCollection, so indexing works
          let code = form.["code"].ToString()
          match parse code with
          | Ok ast ->
              let view = renderList ast
              // Wrap in full HTML
              let doc =
                html [] [
                  head [] [
                    title [] [ str "MicroML AST Visualizer" ]
                    link [ _rel "stylesheet"; _href "/styles.css" ]
                  ]
                  body [] [ view ]
                ]
              return! htmlString (RenderView.AsString.htmlDocument doc) next ctx
          | Error e ->
              return! htmlString $"<p class='error'>Parse error: {e}</p>" next ctx
        }
    ]

[<EntryPoint>]
let main args =
    // Create the WebApplicationBuilder
    let builder = WebApplication.CreateBuilder(args)
    // Add Giraffe services
    builder.Services.AddGiraffe() |> ignore

    let app = builder.Build()

    // Serve wwwroot static files and use Giraffe
    app.UseStaticFiles() |> ignore
    app.UseGiraffe webApp

    // Run on localhost:5000
    app.Run("http://localhost:5000")

    0
