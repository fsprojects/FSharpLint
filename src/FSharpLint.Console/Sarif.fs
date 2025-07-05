module internal Sarif

open FSharpLint.Framework
open System.IO
open System
open Microsoft.CodeAnalysis.Sarif
open Microsoft.CodeAnalysis.Sarif.Writers
open FSharpLint.Console.Output

let writeReport (results: Suggestion.LintWarning list) (codeRoot: string option) (report: string) (logger: IOutput) =
    try
        let codeRoot =
            match codeRoot with
            | None -> Directory.GetCurrentDirectory() |> Uri
            | Some root -> Path.GetFullPath root |> Uri

        // Construct full path to ensure path separators are normalized.
        let report = Path.GetFullPath report
        // Ensure the parent directory exists
        let reportFile = FileInfo(report)
        reportFile.Directory.Create()

        let driver = ToolComponent()
        driver.Name <- "FSharpLint.Console"
        driver.InformationUri <- Uri("https://fsprojects.github.io/FSharpLint/")
        driver.Version <- string<Version> (System.Reflection.Assembly.GetExecutingAssembly().GetName().Version)
        let tool = Tool()
        tool.Driver <- driver
        let run = Run()
        run.Tool <- tool

        use sarifLogger =
            new SarifLogger(
                report,
                logFilePersistenceOptions =
                    (FilePersistenceOptions.PrettyPrint ||| FilePersistenceOptions.ForceOverwrite),
                run = run,
                levels = BaseLogger.ErrorWarningNote,
                kinds = BaseLogger.Fail,
                closeWriterOnDispose = true
            )

        sarifLogger.AnalysisStarted()

        for analyzerResult in results do
            let reportDescriptor = ReportingDescriptor()
            reportDescriptor.Id <- analyzerResult.RuleIdentifier
            reportDescriptor.Name <- analyzerResult.RuleName

            (*
            analyzerResult.ShortDescription
            |> Option.iter (fun shortDescription ->
                reportDescriptor.ShortDescription <-
                    MultiformatMessageString(shortDescription, shortDescription, dict [])
            )
            *)

            let helpUri = $"https://fsprojects.github.io/FSharpLint/how-tos/rules/%s{analyzerResult.RuleIdentifier}.html"
            reportDescriptor.HelpUri <- Uri(helpUri)

            let result = Result()
            result.RuleId <- reportDescriptor.Id
            
            (*
            result.Level <-
                match analyzerResult.Message.Severity with
                | Severity.Info -> FailureLevel.Note
                | Severity.Hint -> FailureLevel.Note
                | Severity.Warning -> FailureLevel.Warning
                | Severity.Error -> FailureLevel.Error
            *)
            result.Level <- FailureLevel.Warning

            let msg = Message()
            msg.Text <- analyzerResult.Details.Message
            result.Message <- msg

            let physicalLocation = PhysicalLocation()

            physicalLocation.ArtifactLocation <-
                let al = ArtifactLocation()
                al.Uri <- codeRoot.MakeRelativeUri(Uri(analyzerResult.Details.Range.FileName))
                al

            physicalLocation.Region <-
                let r = Region()
                r.StartLine <- analyzerResult.Details.Range.StartLine
                r.StartColumn <- analyzerResult.Details.Range.StartColumn + 1
                r.EndLine <- analyzerResult.Details.Range.EndLine
                r.EndColumn <- analyzerResult.Details.Range.EndColumn + 1
                r

            let location: Location = Location()
            location.PhysicalLocation <- physicalLocation
            result.Locations <- [| location |]

            sarifLogger.Log(reportDescriptor, result, System.Nullable())

        sarifLogger.AnalysisStopped(RuntimeConditions.None)

        sarifLogger.Dispose()
    with ex ->
        logger.WriteError($"Could not write sarif to %s{report}: %s{ex.Message}")