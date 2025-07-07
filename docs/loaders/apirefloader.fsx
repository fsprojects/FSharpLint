#r "nuget: Fornax.Core, 0.15.1"
#r "nuget: FSharp.Formatting, 16.1.1"

open System
open System.IO
open FSharp.Formatting.ApiDocs
open FSharp.Formatting.Templating

type ApiPageInfo<'a> = {
    ParentName: string
    ParentUrlName: string
    NamespaceName: string
    NamespaceUrlName: string
    Info: 'a
}

type AssemblyEntities = {
  Label: string
  Modules: ApiPageInfo<ApiDocEntity> list
  Types: ApiPageInfo<ApiDocEntity> list
  GeneratorOutput: ApiDocModel
}

let rec collectModules pn pu nn nu (m: ApiDocEntity) =
    [
        yield { ParentName = pn; ParentUrlName = pu; NamespaceName = nn; NamespaceUrlName = nu; Info = m}
        yield! m.NestedEntities |> List.collect (collectModules m.Name m.UrlBaseName nn nu)
    ]


let loader (projectRoot: string) (siteContet: SiteContents) =
    try
      // Try to find FSharpLint.Core.dll in the project build output
      let projectDir = Path.Combine(projectRoot, "..", "src", "FSharpLint.Core")
      let binDir = Path.Combine(projectDir, "bin", "Release", "net9.0")
      let dllPath = Path.Combine(binDir, "FSharpLint.Core.dll")
      
      let dlls =
        [
          "FSharpLint.Core", dllPath
        ]
      let libs =
        [
          binDir
        ]
      for (label, dll) in dlls do
        if File.Exists dll then
          let inputs = [ApiDocInput.FromFile(dll)]
          let output = ApiDocs.GenerateModel(inputs, label, [], libDirs = libs)

          let allModules =
              output.Collection.Namespaces
              |> List.collect (fun n ->
                  List.collect (collectModules n.Name n.Name n.Name n.Name) n.Entities
              )

          let allTypes =
              [
                  yield!
                      output.Collection.Namespaces
                      |> List.collect (fun n ->
                          n.Entities |> List.choose (fun t ->
                              if t.IsTypeDefinition then
                                  Some {ParentName = n.Name; ParentUrlName = n.Name; NamespaceName = n.Name; NamespaceUrlName = n.Name; Info = t}
                              else
                                  None)
                      )
                  yield!
                      allModules
                      |> List.collect (fun n ->
                          // Get nested types from nested entities
                          n.Info.NestedEntities
                          |> List.choose (fun e ->
                              if e.IsTypeDefinition then
                                  Some {ParentName = n.Info.Name; ParentUrlName = n.Info.UrlBaseName; NamespaceName = n.NamespaceName; NamespaceUrlName = n.NamespaceUrlName; Info = e}
                              else
                                  None)
                      )
              ]
          let entities = {
            Label = label
            Modules = allModules
            Types = allTypes
            GeneratorOutput = output
          }
          siteContet.Add entities
        else
          printfn "Warning: Could not find assembly at %s" dll
    with
    | ex ->
      printfn "%A" ex

    siteContet
