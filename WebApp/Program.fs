// WebApp/Program.fs

module WebApp.Program

open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting

open Giraffe
open Giraffe.ViewEngine

open Parser.AST
open Parser.Parsing

/// Recursively render an Expr as a nested <ul class="ast">
let rec renderList (expr: Expr) : XmlNode =
    // Build any child subtrees
    let children =
        match expr with
        | IntLit _ | Var _    -> []
        | Lambda(_, body)     -> [ renderList body ]
        | App(f, x)           -> [ renderList f; renderList x ]
        | Let(_, v, b)        -> [ renderList v; renderList b ]

    // Create a UL node whose first item is this expr, then its children
    ul [ _class "ast" ] (
        [ span [] [ str (sprintf "%O" expr) ] ] @ children
    )

/// The main HTTP handler
let webApp : HttpHandler =
    choose [
        GET  >=> htmlFile "Views/index.html"
        POST >=> fun next ctx -> task {
            // Bind form data
            let! form = ctx.BindFormAsync()
            let code = form.["code"].ToString()

            match parse code with
            | Ok ast ->
                // Render and wrap in an HTML document
                let view = renderList ast
                let htmlDoc =
                    html [] [
                        head [] [
                            title [] [ str "MicroML AST Visualizer" ]
                            link [ _rel "stylesheet"; _href "/styles.css" ]
                        ]
                        body [] [ view ]
                    ]
                let htmlStr = RenderView.AsString.htmlDocument htmlDoc
                return! htmlString htmlStr next ctx

            | Error err ->
                return! htmlString (sprintf "<p class='error'>Parse error: %s</p>" err) next ctx
        }
    ]

[<EntryPoint>]
let main args =
    Host.CreateDefaultBuilder(args)
      .ConfigureWebHostDefaults(fun webBuilder ->
          webBuilder.UseUrls("http://localhost:5000") |> ignore
          webBuilder.Configure(fun appBuilder ->
              appBuilder.UseStaticFiles() |> ignore
              appBuilder.UseGiraffe webApp   |> ignore
          ) |> ignore
      )
      .Build()
      .Run()
    0



