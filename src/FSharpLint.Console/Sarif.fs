module internal Sarif

open System
open System.IO
open Microsoft.CodeAnalysis.Sarif
open Microsoft.CodeAnalysis.Sarif.Writers
open FSharpLint.Framework
open FSharpLint.Console.Output

let writeReport (results: Suggestion.LintWarning list) (codeRoot: string option) (report: string) (logger: IOutput) =
    try
        let codeRootDirectory =
            match codeRoot with
            | None -> Directory.GetCurrentDirectory() |> Uri
            | Some root -> Path.GetFullPath root |> Uri

        // Construct full path to ensure path separators are normalized.
        let reportPath = Path.GetFullPath report
        // Ensure the parent directory exists
        let reportFile = FileInfo(reportPath)
        reportFile.Directory.Create()

        let driver = 
            ToolComponent(
                Name = "FSharpLint.Console",
                InformationUri = Uri("https://fsprojects.github.io/FSharpLint/"),
                Version = string<Version> (System.Reflection.Assembly.GetExecutingAssembly().GetName().Version)
            )

        let tool = Tool(Driver = driver)
        let run = Run(Tool = tool)

        use sarifLogger =
            new SarifLogger(
                reportPath,
                logFilePersistenceOptions =
                    (FilePersistenceOptions.PrettyPrint ||| FilePersistenceOptions.ForceOverwrite),
                run = run,
                levels = BaseLogger.ErrorWarningNote,
                kinds = BaseLogger.Fail,
                closeWriterOnDispose = true
            )

        sarifLogger.AnalysisStarted()

        for analyzerResult in results do
            let helpUri = $"https://fsprojects.github.io/FSharpLint/how-tos/rules/%s{analyzerResult.RuleIdentifier}.html"

            let reportDescriptor = 
                ReportingDescriptor(
                    Id = analyzerResult.RuleIdentifier,
                    HelpUri = Uri(helpUri),
                    Name = analyzerResult.RuleName
                )

            let msg = Message(Text = analyzerResult.Details.Message)           

            let artifactLocation = 
                ArtifactLocation(
                    Uri = codeRootDirectory.MakeRelativeUri(Uri(analyzerResult.Details.Range.FileName))
                )

            let region = 
                Region(
                    StartLine = analyzerResult.Details.Range.StartLine,
                    StartColumn = analyzerResult.Details.Range.StartColumn + 1,
                    EndLine = analyzerResult.Details.Range.EndLine,
                    EndColumn = analyzerResult.Details.Range.EndColumn + 1
                )

            let physicalLocation = 
                PhysicalLocation(
                    ArtifactLocation = artifactLocation,
                    Region = region 
                )

            let location = Location(PhysicalLocation = physicalLocation)

            let result = 
                Result(
                    RuleId = reportDescriptor.Id,
                    Level = FailureLevel.Warning,
                    Locations = [| location |],
                    Message = msg
                )

            sarifLogger.Log(reportDescriptor, result, System.Nullable())

        sarifLogger.AnalysisStopped(RuntimeConditions.None)

        sarifLogger.Dispose()
    with ex ->
        logger.WriteError($"Could not write sarif to %s{report}: %s{ex.Message}")