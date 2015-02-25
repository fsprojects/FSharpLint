using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.CrossDomain
{
    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        public FSharpLint.Worker.Result RunLint(string projectFile)
        {
            System.Console.Write(string.Join("\n", System.AppDomain.CurrentDomain.GetAssemblies().Select(x => x.FullName)));

            return FSharpLint.Worker.Result.NewFailure("woof");
        }
                /*
                try
                    let parseInfo =
                        {
                            FinishEarly = options.FinishEarly
                            ProjectFile = projectFile
                            Progress = System.Action<_>(ignore)//(toWorkerProgress >> options.Progress.Invoke)
                            ErrorReceived = System.Action<_>(ignore)//(toWorkerError >> options.ErrorReceived.Invoke)
                        }

                    match parseProject parseInfo with
                        | Result.Failure(ProjectFile.ProjectFileCouldNotBeFound(projectPath)) -> 
                            failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
                        | Result.Failure(ProjectFile.MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                            failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
                        | Result.Failure(ProjectFile.UnableToFindProjectOutputPath(projectPath)) -> 
                            failed "ConsoleUnableToFindProjectOutputPath" [|projectPath|]
                        | Result.Failure(ProjectFile.UnableToFindReferencedProject(referencedProjectPath)) -> 
                            failed "ConsoleUnableToFindReferencedProject" [|referencedProjectPath|]
                        | Result.Failure(ProjectFile.FailedToLoadConfig(message)) -> 
                            failed "ConsoleFailedToLoadConfig" [|message|]
                        | Result.Failure(ProjectFile.RunTimeConfigError) -> 
                            failed "ConsoleRunTimeConfigError" [||]
                        | Result.Failure(ProjectFile.FailedToResolveReferences) -> 
                            failed "ConsoleFailedToResolveReferences" [||]
                        | Result.Success -> 
                            FSharpLint.Worker.Success
                with
                    | FSharpLint.Framework.Ast.ParseException({ File = file; Errors = errors }) ->
                        FSharpLint.Worker.Failure(
                            "Lint failed while analysing " + 
                            projectFile + 
                            ".\nFailed with: " + 
                            System.String.Join("\n", errors))
                    | e -> 
                        FSharpLint.Worker.Failure("Lint failed while analysing " + projectFile + ".\nFailed with: " + e.Message + "\nStack trace: " + e.StackTrace)*/
    }
}
