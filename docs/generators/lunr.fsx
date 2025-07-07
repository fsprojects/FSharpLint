#r "../_lib/Fornax.Core.dll"
#r "../../packages/docs/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#r "../../packages/docs/FSharp.Formatting/lib/netstandard2.0/FSharp.MetadataFormat.dll"
#if !FORNAX
#load "../loaders/contentloader.fsx"
#load "../loaders/apirefloader.fsx"
#load "../loaders/globalloader.fsx"

#endif

open Apirefloader
open FSharp.MetadataFormat


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
                  let namespaces = generatorOutput.AssemblyGroup.Namespaces |> Seq.map (fun n -> n.Name) |> String.concat " "
                  $"%s{generatorOutput.AssemblyGroup.Name} \n %s{namespaces}"
              {uri = $"{rootUrl}/reference/%s{n.Label}/index.html"; title = $"%s{n.Label} - API Reference"; content = ctn }

          let mdlsGen =
              allModules
              |> Seq.map (fun m ->
                  let m = m.Info
                  let cnt =
                      sprintf "%s \n %s \n %s \n %s \n %s \n %s"
                          m.Name
                          m.Comment.FullText
                          (m.NestedModules |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                          (m.NestedTypes |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                          (m.ValuesAndFuncs |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")
                          (m.TypeExtensions |> List.map (fun m -> m.Name + " " + m.Comment.FullText ) |> String.concat " ")


                  {uri = $"{rootUrl}/reference/%s{n.Label}/%s{m.UrlName}.html"; title = m.Name; content = cnt }
              )

          let tsGen =
              allTypes
              |> Seq.map (fun m ->
                  let m = m.Info
                  let cnt =
                      let allMembers = m.AllMembers |> List.map (fun m -> $"{m.Name} {m.Comment.FullText}" ) |> String.concat " "
                      $"%s{m.Name} \n %s{m.Comment.FullText} \n %s{allMembers}"


                  {uri = $"{rootUrl}/reference/%s{n.Label}/%s{m.UrlName}.html"; title = m.Name; content = cnt }
              )
          [yield! entries; gen; yield! mdlsGen; yield! tsGen]
        )

    [|yield! entries; yield! refs|]
    |> Newtonsoft.Json.JsonConvert.SerializeObject

