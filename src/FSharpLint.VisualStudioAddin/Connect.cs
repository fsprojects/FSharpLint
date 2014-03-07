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

        public void OnConnection(object application, ext_ConnectMode connectMode, object addInInst, ref System.Array a)
        {
            app = (DTE)application;

            app.Events.BuildEvents.OnBuildDone += (f, s) => AddSampleErrors();

            var addInInstance = (AddIn)addInInst;
            if (connectMode == ext_ConnectMode.ext_cm_UISetup)
            {
                object[] contextGUIDS = new object[] { };
                Commands2 commands = (Commands2)app.Commands;
                string toolsMenuName = "Tools";

                Microsoft.VisualStudio.CommandBars.CommandBar menuBarCommandBar = ((Microsoft.VisualStudio.CommandBars.CommandBars)app.CommandBars)["MenuBar"];

                CommandBarControl toolsControl = menuBarCommandBar.Controls[toolsMenuName];
                CommandBarPopup toolsPopup = (CommandBarPopup)toolsControl;

                try
                {
                    Command command = commands.AddNamedCommand2(
                        addInInstance, 
                        "FSharpLint",
                        "FSharpLint", 
                        "", 
                        true, 
                        59, 
                        ref contextGUIDS, 
                        (int)vsCommandStatus.vsCommandStatusSupported + (int)vsCommandStatus.vsCommandStatusEnabled, 
                        (int)vsCommandStyle.vsCommandStylePictAndText, 
                        vsCommandControlType.vsCommandControlTypeButton);

                    if ((command != null) && (toolsPopup != null))
                    {
                        command.AddControl(toolsPopup.CommandBar, 1);
                    }

                    CommandBar standardCommandBar = ((CommandBars)app.CommandBars)["Standard"];
                    CommandBarButton standardCommandBarButton = (CommandBarButton)command.AddControl(
                                standardCommandBar, standardCommandBar.Controls.Count + 1);
                    standardCommandBarButton.Style = MsoButtonStyle.msoButtonIcon;
                }
                catch (System.ArgumentException)
                {
                    // If we are here, then the exception is probably because a command with that name
                    // already exists. If so there is no need to recreate the command and we can 
                    // safely ignore the exception.
                }
            }

            if (connectMode == ext_ConnectMode.ext_cm_AfterStartup)
            {
                System.Array arr = null;
                OnStartupComplete(ref arr);
            }
        }

        public void OnDisconnection(ext_DisconnectMode disconnectMode, ref System.Array a)
        {
            RemoveTasks();
        }

        public void OnAddInsUpdate(ref System.Array a) { }

        public void OnBeginShutdown(ref System.Array a) { }

        public void OnStartupComplete(ref System.Array a)
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

        private void AddSampleErrors()
        {
            new System.Threading.Tasks.Task(() =>
            {
                if (app.Solution.IsOpen)
                {
                    if (app.Solution.Projects.Count > 0)
                    {
                        for (var i = 1; i < app.Solution.Projects.Count; i++)
                        {
                            var project = app.Solution.Projects.Item(i);

                            if (System.IO.Path.GetExtension(project.FileName) == ".fsproj")
                            {
                                var errors = FSharpLint.Console.ProjectFile.parseProject(project.FileName);

                                foreach (var error in errors)
                                {
                                    var range = error.Range;

                                    AddErrorToErrorList(
                                        project,
                                        range.FileName,
                                        error.Info,
                                        TaskErrorCategory.Warning,
                                        range.StartLine,
                                        range.StartColumn);
                                }
                            }
                        }
                    }
                }
            }).Start();
        }

        private void AddErrorToErrorList(Project objProject, string fileName,
           string sErrorText, TaskErrorCategory eTaskErrorCategory, int iLine,
           int iColumn)
        {
            IVsHierarchy objVsHierarchy;

            var objIVsSolution = (IVsSolution)GetService(typeof(SVsSolution));

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
                for (var i = errorTasks.Count - 1; i >= 0; i--)
                {
                    var objErrorTask = errorTasks[i];

                    object objObject;

                    ErrorHandler.ThrowOnFailure(objErrorTask.HierarchyItem.GetProperty(
                        VSConstants.VSITEMID_ROOT,
                        (int)__VSHPROPID.VSHPROPID_ExtObject,
                        out objObject));

                    if (objObject is Project)
                    {
                        var objErrorTaskProject = (Project)objObject;

                        if (objErrorTaskProject.UniqueName == objProject.UniqueName)
                        {
                            RemoveTask(objErrorTask);
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

        private void RemoveTask(ErrorTask objErrorTask)
        {
            try
            {
                errorListProvider.SuspendRefresh();

                objErrorTask.Navigate -= ErrorTaskNavigate;

                errorTasks.Remove(objErrorTask);

                errorListProvider.Tasks.Remove(objErrorTask);
            }
            catch (Exception e)
            { }
            finally
            {

                errorListProvider.ResumeRefresh();
            }
        }

        /// <summary>Implements the QueryStatus method of the IDTCommandTarget interface. This is called when the command's availability is updated</summary>
        /// <param term='commandName'>The name of the command to determine state for.</param>
        /// <param term='neededText'>Text that is needed for the command.</param>
        /// <param term='status'>The state of the command in the user interface.</param>
        /// <param term='commandText'>Text requested by the neededText parameter.</param>
        /// <seealso class='Exec' />
        public void QueryStatus(string commandName, vsCommandStatusTextWanted neededText, ref vsCommandStatus status, ref object commandText)
        {
            if (neededText == vsCommandStatusTextWanted.vsCommandStatusTextWantedNone)
            {
                if (commandName == "dog.Connect.dog")
                {
                    status = (vsCommandStatus)vsCommandStatus.vsCommandStatusSupported | vsCommandStatus.vsCommandStatusEnabled;
                }
            }
        }

        /// <summary>Implements the Exec method of the IDTCommandTarget interface. This is called when the command is invoked.</summary>
        /// <param term='commandName'>The name of the command to execute.</param>
        /// <param term='executeOption'>Describes how the command should be run.</param>
        /// <param term='varIn'>Parameters passed from the caller to the command handler.</param>
        /// <param term='varOut'>Parameters passed from the command handler to the caller.</param>
        /// <param term='handled'>Informs the caller if the command was handled or not.</param>
        /// <seealso class='Exec' />
        public void Exec(string commandName, vsCommandExecOption executeOption, ref object varIn, ref object varOut, ref bool handled)
        {
            handled = false;
            if (executeOption == vsCommandExecOption.vsCommandExecOptionDoDefault)
            {
                if (commandName == "dog.Connect.dog")
                {
                    handled = true;
                }
            }
        }
    }
}