using System;
using System.Linq;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.ComponentModel.Design;
using Microsoft.Win32;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using EnvDTE;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace MattMcveigh.FSharpLint_VisualStudioExtension
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [ProvideAutoLoad("{f1536ef8-92ec-443c-9ed7-fdadf150da82}")]
    [Guid(GuidList.guidFSharpLint_VisualStudioExtensionPkgString)]
    public sealed class FSharpLint_VisualStudioExtensionPkg : Package
    {
        private IVsOutputWindowPane outputPane;
        private Guid fsharpLintOutputGuid = new Guid("69e38f97-532c-4974-9612-fbdd515aa3d7");
        private ErrorListProvider errorListProvider;
        private DTE dte;
        private IList<ErrorTask> errorsDisplayed = new List<ErrorTask>();

        private CancellationTokenSource cts;
        private OleMenuCommand m;

        private const string ToolsButtonText = "Run FSharpLint on open projects.";

        private bool IsProjectFSharpProject(Project project)
        {
            return System.IO.Path.GetExtension(project.FileName) == ".fsproj";
        }

        protected override void Initialize()
        {
            base.Initialize();

            this.dte = (DTE)GetService(typeof(DTE));

            this.errorListProvider = new ErrorListProvider(this)
            {
                ProviderName = "FSharpLint Error Provider",
                ProviderGuid = new Guid("5BA8BB0D-D517-45ae-966C-864C53645667")
            };

            this.errorListProvider.Show();

            var menuCommandService = GetService(typeof(IMenuCommandService)) as OleMenuCommandService;
            if (menuCommandService != null)
            {
                var commandId = new CommandID(GuidList.guidFSharpLint_VisualStudioExtensionCmdSet, (int)PkgCmdIDList.menuRunFSharpLint);
                this.m = new OleMenuCommand(this.MenuItemCallbackAsync, commandId)
                {
                    Text = ToolsButtonText
                };
                menuCommandService.AddCommand(this.m);

                this.dte.Events.SolutionEvents.Opened += () => 
                {
                    if (this.Projects().Any(this.IsProjectFSharpProject))
                    {
                        this.m.Visible = true;
                    }
                };

                this.dte.Events.SolutionEvents.BeforeClosing += () =>
                {
                    if (cts != null)
                    {
                        cts.Cancel();
                    }

                    this.m.Visible = false;
                };

                this.dte.Events.BuildEvents.OnBuildBegin += (d, g) => 
                {
                    if (cts != null)
                    {
                        cts.Cancel();
                    }

                    this.m.Text = ToolsButtonText + " (Building)";
                    this.m.Enabled = false;
                };

                this.dte.Events.BuildEvents.OnBuildDone += (d, g) =>
                {
                    this.m.Text = ToolsButtonText;
                    this.m.Enabled = true;
                };
            }

            if (this.outputPane == null)
            {
                var outputWindow = Package.GetGlobalService(typeof(SVsOutputWindow)) as IVsOutputWindow;
                outputWindow.CreatePane(this.fsharpLintOutputGuid, "FSharpLint", 1, 0);
                outputWindow.GetPane(this.fsharpLintOutputGuid, out this.outputPane);
                this.outputPane.Activate();
            }
        }

        private IEnumerable<Project> Projects()
        {
            if (this.dte.Solution.IsOpen && this.dte.Solution.Projects.Count > 0)
            {
                for (var i = 1; i <= this.dte.Solution.Projects.Count; i++)
                {
                    yield return this.dte.Solution.Projects.Item(i);
                }
            }
        }

        private async void MenuItemCallbackAsync(object sender, EventArgs e)
        {
            this.m.Text = ToolsButtonText + " (Running)";
            this.m.Enabled = false;

            foreach (var error in errorsDisplayed)
            {
                this.errorListProvider.Tasks.Remove(error);
            }

            this.cts = new CancellationTokenSource();

            try
            {
                await System.Threading.Tasks.Task.Run(() => this.RunLint(cts.Token));
            }
            catch (OperationCanceledException)
            {
                this.outputPane.OutputString("Stopped running - you probably started building a project.");
            }

            this.m.Text = ToolsButtonText;
            this.m.Enabled = true;
        }

        private void RunLint(CancellationToken token)
        {
            Func<bool> endEarly = () => token.IsCancellationRequested;

            foreach (var project in this.Projects().Where(this.IsProjectFSharpProject))
            {
                if (!endEarly())
                {
                    FSharpLint.Application.ProjectFile.parseProject(
                        endEarly,
                        project.FileName,
                        this.OutputProgress,
                        this.AddError(project));
                }

                token.ThrowIfCancellationRequested();
            }
        }

        private void OutputProgress(FSharpLint.Application.ProjectFile.ParserProgress progress)
        {
            if (progress.IsStarting)
            {
                outputPane.OutputString("Started parsing ");
            }
            else if (progress.IsReachedEnd)
            {
                outputPane.OutputString("Finished parsing ");
            }
            else if (progress.IsFailed)
            {
                outputPane.OutputString("Failed to parse ");
            }

            outputPane.OutputString(progress.Filename() + "\n");
        }

        private void AddErrorToErrorList(Project project, string fileName,
           string errorText, TaskErrorCategory taskErrorCategory, int line,
           int column)
        {
            var iVsSolution = (IVsSolution)GetService(typeof(SVsSolution));

            IVsHierarchy vsHierarchy;
            iVsSolution.GetProjectOfUniqueName(project.UniqueName, out vsHierarchy);

            var errorTask = new Microsoft.VisualStudio.Shell.ErrorTask
            {
                ErrorCategory = taskErrorCategory,
                HierarchyItem = vsHierarchy,
                Document = fileName,
                Line = line - 1,
                Column = column,
                Text = errorText
            };

            errorTask.Navigate += ErrorTaskNavigate;

            this.errorsDisplayed.Add(errorTask);

            this.errorListProvider.Tasks.Add(errorTask);
        }

        private void ErrorTaskNavigate(object sender, EventArgs e)
        {
            var errorTask = (ErrorTask)sender;

            errorTask.Line += 1; // Fix the index start
            this.errorListProvider.Navigate(errorTask, new Guid(EnvDTE.Constants.vsViewKindCode));
            errorTask.Line -= 1; // Restore the index start
        }

        private Action<FSharpLint.Application.ErrorHandling.Error> AddError(EnvDTE.Project project)
        {
            return error =>
            {
                var range = error.Range;

                this.AddErrorToErrorList(
                    project,
                    range.FileName,
                    error.Info,
                    TaskErrorCategory.Warning,
                    range.StartLine,
                    range.StartColumn);
            };
        }
    }
}
