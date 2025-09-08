#r "nuget: Fornax.Core, 0.16.0"
#r "nuget: Markdig, 0.41.3"
#r "nuget: FSharp.Formatting, 20.0.1"

#if !FORNAX
#load "../loaders/apirefloader.fsx"
#endif

#load "partials/layout.fsx"

open System
open System.Text.Json
open FSharp.Formatting.ApiDocs
open Html
open Apirefloader
open Markdig

let markdownPipeline =
    MarkdownPipelineBuilder()
        .UsePipeTables()
        .UseGridTables()
        .Build()

let getComment (c: ApiDocComment) =
  let t =
    c.RawData
    |> List.map (fun n -> n.Value)
    |> String.concat "\n\n"
  Markdown.ToHtml(t, markdownPipeline)


let formatMember (m: ApiDocMember) =
    let attributes =
      m.Attributes
      |> List.filter (fun a -> a.FullName <> "Microsoft.FSharp.Core.CustomOperationAttribute")

    let hasCustomOp =
      m.Attributes
      |> List.exists (fun a -> a.FullName = "Microsoft.FSharp.Core.CustomOperationAttribute")

    let customOp =
      if hasCustomOp then
        m.Attributes
        |> List.tryFind (fun a -> a.FullName = "Microsoft.FSharp.Core.CustomOperationAttribute")
        |> Option.bind (fun a ->
          a.ConstructorArguments
          |> Seq.tryFind (fun x -> x :? string)
          |> Option.map (fun x -> x.ToString())
        )
        |> Option.defaultValue ""
      else
        ""

    tr [] [
        td [] [
            code [] [!! m.Name]
            br []

            if hasCustomOp then
              b [] [!! "CE Custom Operation: "]
              code [] [!!customOp]
              br []
            br []
            b [] [!! "Signature: "]
            match m.Details with
            | ApiDocMemberDetails(usageHtml, _, _, _, _, _, _, _) -> !!usageHtml.HtmlText
            br []
            if not (attributes.IsEmpty) then
                b [] [!! "Attributes:"]
                for a in attributes do
                    code [] [!! (a.Name)]
        ]
        td [] [!! (getComment m.Comment)]
    ]

let generateType ctx (page: ApiPageInfo<ApiDocEntity>) =
    let t = page.Info
    let body =
        div [Class "api-page"] [
            h2 [] [!! t.Name]
            b [] [!! "Namespace: "]
            a [Href ($"../%s{page.NamespaceUrlName}.html")] [!! page.NamespaceName]
            br []
            if page.ParentName <> page.NamespaceName then
                b [] [!! "Parent Module: "]
                a [Href ($"../%s{page.ParentUrlName}.html")] [!! page.ParentName]
                br []
            b [] [!! "Assembly: "]
            !! t.Assembly.Name
            br []
            if not (String.IsNullOrWhiteSpace t.Category) then
                b [] [!! "Category: "]
                !!t.Category
                br []
            if not (t.Attributes.IsEmpty) then
                b [] [!! "Attributes: "]
                for a in t.Attributes do
                    code [] [!! (a.Name)]
                    br []
            br []

            table [] [
                tr [] [
                    th [ Width "35%" ] [!!"Name"]
                    th [ Width "65%"] [!!"Description"]
                ]
                if not (t.Constructors : ApiDocMember list).IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Constructors"]]]
                yield! t.Constructors |> List.map formatMember

                if not (t.InstanceMembers : ApiDocMember list).IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Instance Members"]]]
                yield! t.InstanceMembers |> List.map formatMember

                // Record Fields from AllMembers
                let recordFields = t.AllMembers |> List.filter (fun m -> m.Kind = ApiDocMemberKind.RecordField)
                if not recordFields.IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Record Fields"]]]
                yield! recordFields |> List.map formatMember

                if not (t.StaticMembers : ApiDocMember list).IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Static Members"]]]
                yield! t.StaticMembers |> List.map formatMember

                // Static Parameters from AllMembers
                let staticParams = t.AllMembers |> List.filter (fun m -> m.Kind = ApiDocMemberKind.StaticParameter)
                if not staticParams.IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Static Parameters"]]]
                yield! staticParams |> List.map formatMember

                // Union Cases from AllMembers
                let unionCases = t.AllMembers |> List.filter (fun m -> m.Kind = ApiDocMemberKind.UnionCase)
                if not unionCases.IsEmpty then tr [] [ td [ColSpan 2. ] [ b [] [!! "Union Cases"]]]
                yield! unionCases |> List.map formatMember
            ]
        ]
    t.UrlBaseName, Layout.layout ctx [body] t.Name

let generateModule ctx (page: ApiPageInfo<ApiDocEntity>) =
    let m = page.Info
    let body =
        div [Class "api-page"] [
            h2 [] [!! m.Name]
            b [] [!! "Namespace: "]
            a [Href ($"../%s{page.NamespaceUrlName}.html")] [!! page.NamespaceName]
            br []
            if page.ParentName <> page.NamespaceName then
                b [] [!! "Parent Module: "]
                a [Href ($"../%s{page.ParentUrlName}.html")] [!! page.ParentName]
                br []
            if not (String.IsNullOrWhiteSpace m.Category) then
                b [] [!! "Category: "]
                !!m.Category
                br []
            br []

            // Split NestedEntities into types and modules
            let nestedTypes = m.NestedEntities |> List.filter (fun e -> e.IsTypeDefinition)
            let nestedModules = m.NestedEntities |> List.filter (fun e -> not e.IsTypeDefinition)

            if not nestedTypes.IsEmpty then
                b [] [!! "Declared Types"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Type"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    for t in nestedTypes do
                        tr [] [
                            td [] [a [Href ($"%s{t.UrlBaseName}.html")] [!! t.Name ]]
                            td [] [!! (getComment t.Comment)]
                        ]
                ]
                br []

            if not nestedModules.IsEmpty then
                b [] [!! "Declared Modules"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Module"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    for t in nestedModules do
                        tr [] [
                            td [] [a [Href ($"%s{t.UrlBaseName}.html")] [!! t.Name ]]
                            td [] [!! (getComment t.Comment)]
                        ]
                ]
                br []

            if not (m.ValuesAndFuncs : ApiDocMember list).IsEmpty then
                b [] [!! "Values and Functions"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Name"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    yield! m.ValuesAndFuncs |> List.map formatMember
                ]
                br []

            if not (m.TypeExtensions : ApiDocMember list).IsEmpty then
                b [] [!! "Type Extensions"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Name"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    yield! m.TypeExtensions |> List.map formatMember
                ]
        ]
    m.UrlBaseName, Layout.layout ctx [body] m.Name

let generateNamespace ctx (allTypes: ApiPageInfo<ApiDocEntity> list) (ns: ApiDocNamespace) =
    let namespaceTypes = allTypes |> List.filter (fun t -> t.NamespaceName = ns.Name && t.ParentName = ns.Name)

    let body =
        div [Class "api-page"] [
            h2 [] [!!ns.Name]

            if not namespaceTypes.IsEmpty then
                b [] [!! "Declared Types"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Type"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    for t in namespaceTypes do
                        tr [] [
                            td [] [a [Href ($"%s{t.Info.UrlBaseName}.html")] [!! t.Info.Name ]]
                            td [] [!! (getComment t.Info.Comment)]
                        ]
                ]
                br []

            if not (ns.Entities).IsEmpty then
                b [] [!! "Declared Modules"]
                table [] [
                    tr [] [
                        th [ Width "35%" ] [!!"Module"]
                        th [ Width "65%"] [!!"Description"]
                    ]
                    for t in ns.Entities do
                        tr [] [
                            td [] [a [Href ($"%s{t.UrlBaseName}.html")] [!! t.Name ]]
                            td [] [!! (getComment t.Comment)]
                        ]
                ]
        ]
    ns.Name, Layout.layout ctx [body] (ns.Name)


let generate' (ctx : SiteContents)  =
    let all = ctx.TryGetValues<AssemblyEntities>()
    match all with
    | None -> []
    | Some all ->
      all
      |> Seq.toList
      |> List.collect (fun n ->
        let name = n.GeneratorOutput.Collection.CollectionName
        let namespaces =
          n.GeneratorOutput.Collection.Namespaces
          |> List.map (generateNamespace ctx n.Types)

        let modules =
          n.Modules
          |> Seq.map (generateModule ctx)

        let types =
          n.Types
          |> Seq.map (generateType ctx)

        let ref =
          Layout.layout ctx [
            h1 [] [!! name ]
            b [] [!! "Declared namespaces"]
            br []
            for (n, _) in namespaces do
                a [Href ($"%s{n}.html")] [!!n]
                br []
          ] n.Label

        [("index" , ref); yield! namespaces; yield! modules; yield! types]
        |> List.map (fun (x, y) -> $"%s{n.Label}/%s{x}", y)
      )


let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
    try
        generate' ctx
        |> List.map (fun (n,b) -> n, (Layout.render ctx b))
    with
    | ex ->
        printfn "ERROR IN API REF GENERATION:\n%A" ex
        []
