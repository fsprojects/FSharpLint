using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FSharpLint.Worker;
using System.ServiceModel;
using System.Runtime.Remoting.Messaging;

namespace FSharpLint.CrossDomain
{
    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        public event ErrorReceivedEventHandler ErrorReceived;

        public event ReportProgressEventHandler ReportProgress;

        private LintOptions options { get; set; }

        public FSharpLint.Worker.Result RunLint(string projectFile, LintOptions options)
        {
            this.options = options;

            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup { PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true };

            var evidence = AppDomain.CurrentDomain.Evidence;

            var appDomain = AppDomain.CreateDomain("Lint Domain", evidence, setup);

            var worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") as FSharpLint.Worker.IFSharpLintWorker;

            worker.ErrorReceived += new ErrorReceivedEventHandler(ReportError);

            return worker.RunLint(projectFile, null);
        }

        [OneWay]
        public void ReportError(Error error)
        {
            options.ErrorReceived(error);
        }
    }
}
