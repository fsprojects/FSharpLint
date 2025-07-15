#r "nuget: Fornax.Core, 0.16.0-beta002"
#r "nuget: FSharp.Formatting, 20.0.1"

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
        // We need the console location as it contains all the dependencies
        let projectDir = Path.Combine(projectRoot, "..", "src", "FSharpLint.Console")
        let dotNetMoniker = "net9.0"
        let projectName = "FSharpLint.Console"
        let projectArtifactName = "FSharpLint.Core.dll"
        // Try multiple possible locations for the assembly
        let possiblePaths = [
            // Release build
            Path.Combine(projectDir, "bin", "Release", dotNetMoniker, projectArtifactName)
            // Debug build
            Path.Combine(projectDir, "bin", "Debug", dotNetMoniker, projectArtifactName)
            // Default build output (no custom output path)
            Path.Combine(projectDir, "bin", "Release", projectArtifactName)
            Path.Combine(projectDir, "bin", "Debug", projectArtifactName)
        ]

        let foundDll = possiblePaths |> List.tryFind File.Exists

        match foundDll with
        | Some dllPath ->
            let binDir = Path.GetDirectoryName(dllPath)
            printfn $"Found assembly at: %s{dllPath}"
            printfn $"Using lib directory: %s{binDir}"

            let libs = [binDir]

            // Try to load with minimal dependencies first
            let inputs = [ApiDocInput.FromFile(dllPath, mdcomments = true)]
            try
                let output = ApiDocs.GenerateModel(inputs, projectName, [], libDirs = libs)

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
                  Label = "FSharpLint.Core"
                  Modules = allModules
                  Types = allTypes
                  GeneratorOutput = output
                }
                siteContet.Add entities
                printfn $"Successfully loaded API documentation for {projectName}"
            with
            | ex ->
                printfn $"Failed to generate API docs from %s{dllPath}: %A{ex}"
                printfn "Continuing without API documentation..."
        | None ->
            printfn $"Warning: Could not find {projectArtifactName} in any of the expected locations:"
            possiblePaths |> List.iter (printfn "  - %s")
            printfn "API documentation will not be generated."
            Environment.Exit 1
    with
    | ex ->
        printfn "Error in API reference loader: %A" ex
        Environment.Exit 1
    siteContet
