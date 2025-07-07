#r "nuget: Fornax.Core, 0.15.1"
#r "nuget: FSharp.Formatting, 16.1.1"

#if !FORNAX
#load "../loaders/contentloader.fsx"

#load "../loaders/apirefloader.fsx"
#load "../loaders/globalloader.fsx"
#endif

open Apirefloader
open FSharp.Formatting.ApiDocs
open System.Text.Json


type Entry = {
    uri: string
    title: string
    content: string
}
let generate (ctx : SiteContents) (projectRoot: string) (page: string) =
    let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>().Value
    let rootUrl = siteInfo.root_url

    let pages = ctx.TryGetValues<Contentloader.Post> () |> Option.defaultValue Seq.empty
    let entries =
      pages
      |> Seq.map (fun n ->
          {uri = rootUrl + "/" + n.link.Replace("content/", ""); title = n.title; content = n.text}
      )

    let all = ctx.TryGetValues<AssemblyEntities>()
    let refs =
      match all with
      | None -> []
      | Some all ->
        all
        |> Seq.toList
        |> List.collect (fun n ->
          let generatorOutput = n.GeneratorOutput
          let allModules = n.Modules
          let allTypes = n.Types

          let gen =
              let ctn =
                  let namespaces = generatorOutput.Collection.Namespaces |> Seq.map (fun n -> n.Name) |> String.concat " "
                  $"%s{generatorOutput.Collection.CollectionName} \n %s{namespaces}"
              {uri = $"{rootUrl}/reference/%s{n.Label}/index.html"; title = $"%s{n.Label} - API Reference"; content = ctn }

          let mdlsGen =
              allModules
              |> Seq.map (fun m ->
                  let m = m.Info
                  let cnt =
                      sprintf "%s \n %s \n %s"
                          m.Name
                          (m.Comment.Xml |> Option.map string |> Option.defaultValue "")
                          (m.NestedEntities |> List.map (fun m -> $"{m.Name} {m.Comment.Xml}") |> String.concat " ")

                  {uri = $"{rootUrl}/reference/%s{n.Label}/%s{m.UrlBaseName}.html"; title = m.Name; content = cnt }
              )

          let tsGen =
              allTypes
              |> Seq.map (fun m ->
                  let m = m.Info
                  let cnt =
                      let getComment xml = xml |> Option.map string |> Option.defaultValue ""
                      let allMembers = m.AllMembers |> List.map (fun m -> $"{m.Name} {m.Comment.Xml |> getComment}" ) |> String.concat " "
                      $"%s{m.Name} \n %s{m.Comment.Xml |> getComment} \n %s{allMembers}"


                  {uri = $"{rootUrl}/reference/%s{n.Label}/%s{m.UrlBaseName}.html"; title = m.Name; content = cnt }
              )
          [yield! entries; gen; yield! mdlsGen; yield! tsGen]
        )

    [|yield! entries; yield! refs|]
    |> JsonSerializer.Serialize

