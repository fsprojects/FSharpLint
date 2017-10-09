namespace FSharpLint.Framework

/// Provides functionality to parse F# files using `FSharp.Compiler.Service`.
module ParseFile = 

    open System.IO
    open System.Reflection
    open FSharpLint.Framework
    open FSharpLint.Framework.Configuration
    open Microsoft.FSharp.Compiler
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    /// Information for a file to be linted that is given to the analysers.
    [<NoEquality; NoComparison>]
    type FileParseInfo =
        { /// Contents of the file.
          Text: string

          /// File represented as an AST.
          Ast: ParsedInput

          /// Optional results of inferring the types on the AST (allows for a more accurate lint).
          TypeCheckResults: FSharpCheckFileResults option

          /// Path to the file.
          File: string }
          
    [<NoComparison>]
    type ParseFileFailure =
        | FailedToParseFile of FSharpErrorInfo []
        | AbortedTypeCheck
        
    [<NoComparison>]
    type ParseFileResult<'t> =
        | Failed of ParseFileFailure
        | Success of 't
        
    let private parse configuration file source (checker:FSharpChecker, options) =
        let parseResults = 
            checker.ParseFileInProject(file, source, options)
            |> Async.RunSynchronously

        let typeCheckFile () =
            let results = 
                checker.CheckFileInProject(parseResults, file, 0, source, options) 
                |> Async.RunSynchronously

            match results with
            | FSharpCheckFileAnswer.Succeeded(x) -> Success(Some(x))
            | FSharpCheckFileAnswer.Aborted -> Failed(AbortedTypeCheck)

        match parseResults.ParseTree with
        | Some(parseTree) -> 
            match typeCheckFile() with
            | Success(typeCheckResults) ->
                { Text = source
                  Ast = parseTree
                  TypeCheckResults = typeCheckResults
                  File = file } |> Success
            | Failed(_) -> Failed(AbortedTypeCheck)
        | None -> Failed(FailedToParseFile(parseResults.Errors))

    /// Todo: Remove this when fsharp.core is updated to version that no longer requires sigdata/optdata
    /// Added this as FSharp.Core was resolving to GAC where there's no sigdata/optdata
    let getProjectOptionsFromScript (checker:FSharpChecker) file source =
        let fsharplintAssembly = typeof<FSharpLint.Framework.Ast.AstNode>.Assembly
        let assemblyDirectory = Path.GetDirectoryName fsharplintAssembly.Location
        let bundledFsharpCore = Path.Combine(assemblyDirectory, "FSharp.Core.dll")
        
        let (options, _diagnostics) = 
            checker.GetProjectOptionsFromScript(file, source) 
            |> Async.RunSynchronously
            
        if File.Exists bundledFsharpCore then
            let useOwnFSharpCore =
                options.OtherOptions 
                |> Array.map (fun x -> if x.Contains "FSharp.Core.dll" then "-r:" + bundledFsharpCore else x)

            { options with OtherOptions = useOwnFSharpCore }
        else
            options

    /// Parses a file using `FSharp.Compiler.Service`.
    let parseFile file configuration (checker:FSharpChecker) projectOptions =
        let source = File.ReadAllText(file)

        let projectOptions =
            match projectOptions with
            | Some(existingOptions) -> existingOptions
            | None -> getProjectOptionsFromScript checker file source
        
        parse configuration file source (checker, projectOptions)
        
    /// Parses source code using `FSharp.Compiler.Service`.
    let parseSourceFile fileName source configuration (checker:FSharpChecker) =        
        let options = getProjectOptionsFromScript checker fileName source

        parse configuration fileName source (checker, options)
        
    let parseSource source configuration (checker:FSharpChecker) = 
        parseSourceFile "test.fsx" source configuration checker