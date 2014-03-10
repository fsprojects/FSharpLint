using System;
using EnvDTE80;
using Microsoft.VisualStudio.Shell;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.CommandBars;
using Microsoft.VisualStudio.Shell.Interop;
using System.Collections.Generic;
using EnvDTE;
using Extensibility;

namespace FSharpLint.VisualStudioAddin
{
    public class Connect : IDTExtensibility2, System.IServiceProvider, IDTCommandTarget
    {
        private DTE app;
        private ErrorListProvider errorListProvider;
        private IList<ErrorTask> errorTasks;
        private SolutionEvents solutionEvents;
        private AddIn addinInstance;

        private const string CommandName = "FSharpLintRun";

        public void OnConnection(object application, ext_ConnectMode connectMode, object addInInst, ref System.Array a)
        {
            app = (DTE)application;

            addinInstance = (AddIn)addInInst;

            //app.Events.BuildEvents.OnBuildDone += (f, s) => AddErrors();

            var commandName = addinInstance.ProgID + "." + CommandName;

            EnvDTE.Command codeWindowCommand = null;

            try
            {
                codeWindowCommand = app.Commands.Item(commandName);
            }
            catch (Exception ignore) { }

            if (codeWindowCommand == null)
            {
                codeWindowCommand = app.Commands.AddNamedCommand(addinInstance, CommandName,
                    "Run FSharpLint", "Runs FSharpLint on all open projects.", true, 18, 
                    null, 
                    (int)(vsCommandStatus.vsCommandStatusSupported | vsCommandStatus.vsCommandStatusEnabled));
            }

            var codeWindow = ((CommandBars)app.CommandBars)["Code Window"];
            var tools = ((CommandBars)app.CommandBars)["Tools"];

            var button = (CommandBarControl)codeWindowCommand.AddControl(codeWindow);
            button.Caption = "Run FSharpLint";
            button.TooltipText = "Runs FSharpLint on all open projects.";

            var toolsButton = (CommandBarControl)codeWindowCommand.AddControl(tools);
            button.Caption = "Run FSharpLint";
            button.TooltipText = "Runs FSharpLint on all open projects.";

            if (connectMode == ext_ConnectMode.ext_cm_AfterStartup)
            {
                System.Array arr = null;
                OnStartupComplete(ref arr);
            }
        }

        public void OnDisconnection(ext_DisconnectMode disconnectMode, ref Array a)
        {
            RemoveTasks();
        }

        public void OnAddInsUpdate(ref Array a) { }

        public void OnBeginShutdown(ref Array a) { }

        public void OnStartupComplete(ref Array a)
        {
            solutionEvents = app.Events.SolutionEvents;

            solutionEvents.BeforeClosing += objSolutionEvents_BeforeClosing;
            solutionEvents.ProjectRemoved += objSolutionEvents_ProjectRemoved;

            InitializeErrorListProvider();
        }

        private void InitializeErrorListProvider()
        {
            errorTasks = new List<ErrorTask>();

            errorListProvider = new Microsoft.VisualStudio.Shell.ErrorListProvider(this);

            errorListProvider.ProviderName = "My Error Provider";

            // TODO: (IMPORTANT) Use your OWN Guid here: 
            errorListProvider.ProviderGuid = new Guid("5BA8BB0D-D517-45ae-966C-864C53645667");

            errorListProvider.Show();
        }

        private void OutputProgress(Console.ProjectFile.ParserProgress progress)
        {
            var outputPane = ((DTE2)this.app).ToolWindows.OutputWindow.ActivePane;

            if (progress.IsStarting)
                outputPane.OutputString("Started parsing ");
            else if (progress.IsReachedEnd)
                outputPane.OutputString("Finished parsing ");
            else
                outputPane.OutputString("Failed to parse ");

            outputPane.OutputString(progress.Filename() + "\n");
        }

        private Action<Console.ErrorHandling.Error> AddError(EnvDTE.Project project)
        {
            return error =>
                {
                    var range = error.Range;

                    AddErrorToErrorList(
                        project,
                        range.FileName,
                        error.Info,
                        TaskErrorCategory.Warning,
                        range.StartLine,
                        range.StartColumn);
                };
        }

        private void AddErrors()
        {
            new System.Threading.Tasks.Task(() =>
            {
                if (app.Solution.IsOpen && app.Solution.Projects.Count > 0)
                {
                    for (var i = 1; i <= app.Solution.Projects.Count; i++)
                    {
                        var project = app.Solution.Projects.Item(i);

                        if (System.IO.Path.GetExtension(project.FileName) == ".fsproj")
                        {
                            FSharpLint.Console.ProjectFile.parseProject(
                                project.FileName,
                                OutputProgress,
                                AddError(project));
                        }
                    }
                }
            }).Start();
        }

        private void AddErrorToErrorList(Project objProject, string fileName,
           string sErrorText, TaskErrorCategory eTaskErrorCategory, int iLine,
           int iColumn)
        {
            var objIVsSolution = (IVsSolution)GetService(typeof(SVsSolution));

            IVsHierarchy objVsHierarchy;
            var project = objIVsSolution.GetProjectOfUniqueName(objProject.UniqueName, out objVsHierarchy);

            ErrorHandler.ThrowOnFailure(project);

            var objErrorTask = new Microsoft.VisualStudio.Shell.ErrorTask();
            objErrorTask.ErrorCategory = eTaskErrorCategory;
            objErrorTask.HierarchyItem = objVsHierarchy;
            objErrorTask.Document = fileName;
            objErrorTask.Line = iLine - 1;
            objErrorTask.Column = iColumn;

            objErrorTask.Text = sErrorText;
            objErrorTask.Navigate += ErrorTaskNavigate;

            errorTasks.Add(objErrorTask);

            errorListProvider.Tasks.Add(objErrorTask);
        }

        private void ErrorTaskNavigate(object sender, EventArgs e)
        {
            var objErrorTask = sender as ErrorTask;

            objErrorTask.Line += 1; // Fix the index start
            var bResult = errorListProvider.Navigate(objErrorTask, new Guid(EnvDTE.Constants.vsViewKindCode));
            objErrorTask.Line -= 1; // Restore the index start
        }

        public object GetService(System.Type serviceType)
        {
            return Microsoft.VisualStudio.Shell.Package.GetGlobalService(serviceType);
        }

        private void objSolutionEvents_BeforeClosing()
        {
            RemoveTasks();
        }

        private void objSolutionEvents_ProjectRemoved(EnvDTE.Project objProject)
        {
            if (errorTasks != null)
            {
                foreach (var errorTask in errorTasks)
                {
                    object project;

                    ErrorHandler.ThrowOnFailure(errorTask.HierarchyItem.GetProperty(
                        VSConstants.VSITEMID_ROOT,
                        (int)__VSHPROPID.VSHPROPID_ExtObject,
                        out project));

                    if (project is Project)
                    {
                        var objErrorTaskProject = (Project)project;

                        if (objErrorTaskProject.UniqueName == objProject.UniqueName)
                        {
                            RemoveTask(errorTask);
                        }
                    }
                }
            }
        }

        private void RemoveTasks()
        {
            if (errorTasks != null)
            {
                foreach (var error in errorTasks)
                {
                    RemoveTask(error);
                }
            }
        }

        private void RemoveTask(ErrorTask errorTask)
        {
            try
            {
                errorListProvider.SuspendRefresh();

                errorTask.Navigate -= ErrorTaskNavigate;

                errorTasks.Remove(errorTask);

                errorListProvider.Tasks.Remove(errorTask);
            }
            finally
            {
                errorListProvider.ResumeRefresh();
            }
        }
       
        public void Exec(string CmdName, vsCommandExecOption ExecuteOption, ref object VariantIn, ref object VariantOut, ref bool Handled)
        {
            if (CmdName == addinInstance.ProgID + "." + CommandName)
            {
                AddErrors();
            }
        }

        public void QueryStatus(string CmdName, vsCommandStatusTextWanted NeededText, ref vsCommandStatus StatusOption, ref object CommandText)
        {
            if (NeededText == vsCommandStatusTextWanted.vsCommandStatusTextWantedNone)
            {
                if (CmdName == addinInstance.ProgID + "." + CommandName)
                {
                    StatusOption = (int)vsCommandStatus.vsCommandStatusSupported + vsCommandStatus.vsCommandStatusEnabled;
                }
            }
        }
    }
}