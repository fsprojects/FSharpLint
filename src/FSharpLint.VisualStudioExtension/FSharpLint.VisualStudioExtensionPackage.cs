using System;
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

namespace MattMcveigh.FSharpLint_VisualStudioExtension
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    [ProvideMenuResource("Menus.ctmenu", 1)]
    [Guid(GuidList.guidFSharpLint_VisualStudioExtensionPkgString)]
    public sealed class FSharpLint_VisualStudioExtensionPkg : Package
    {
        private IVsOutputWindowPane outputPane;
        private Guid fsharpLintOutputGuid = new Guid("69e38f97-532c-4974-9612-fbdd515aa3d7");
        private ErrorListProvider errorListProvider;
        private DTE dte;

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
                var menuItem = new MenuCommand(MenuItemCallback, commandId);
                menuCommandService.AddCommand(menuItem);
            }

            if (this.outputPane == null)
            {
                var outputWindow = Package.GetGlobalService(typeof(SVsOutputWindow)) as IVsOutputWindow;
                outputWindow.CreatePane(this.fsharpLintOutputGuid, "FSharpLint", 1, 0);
                outputWindow.GetPane(this.fsharpLintOutputGuid, out this.outputPane);
                this.outputPane.Activate();
            }
        }

        private void MenuItemCallback(object sender, EventArgs e)
        {
            new System.Threading.Tasks.Task(() =>
            {
                if (this.dte.Solution.IsOpen && this.dte.Solution.Projects.Count > 0)
                {
                    for (var i = 1; i <= this.dte.Solution.Projects.Count; i++)
                    {
                        var project = this.dte.Solution.Projects.Item(i);

                        if (System.IO.Path.GetExtension(project.FileName) == ".fsproj")
                        {
                            FSharpLint.Console.ProjectFile.parseProject(
                                project.FileName,
                                this.OutputProgress,
                                this.AddError(project));
                        }
                    }
                }
            }).Start();
        }

        private void OutputProgress(FSharpLint.Console.ProjectFile.ParserProgress progress)
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

            this.errorListProvider.Tasks.Add(errorTask);
        }

        private void ErrorTaskNavigate(object sender, EventArgs e)
        {
            var errorTask = (ErrorTask)sender;

            errorTask.Line += 1; // Fix the index start
            this.errorListProvider.Navigate(errorTask, new Guid(EnvDTE.Constants.vsViewKindCode));
            errorTask.Line -= 1; // Restore the index start
        }

        private Action<FSharpLint.Console.ErrorHandling.Error> AddError(EnvDTE.Project project)
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
