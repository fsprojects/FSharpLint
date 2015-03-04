using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FSharpLint.Worker;
using System.ServiceModel;
using System.Runtime.Remoting.Messaging;
using System.Collections.Concurrent;
using System.Threading;

namespace FSharpLint.CrossDomain
{
    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        public event ErrorReceivedEventHandler ErrorReceived;

        public event ReportProgressEventHandler ReportProgress;

        public FSharpLint.Worker.Result RunLint(string projectFile)
        {
            var worker = GetWorker();

            worker.ErrorReceived += ErrorReceived;

            worker.ReportProgress += ReportProgress;

            return worker.RunLint(projectFile);
        }
        
        private IFSharpLintWorker GetWorker()
        {
            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup { LoaderOptimization = LoaderOptimization.SingleDomain, PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true };

            var evidence = AppDomain.CurrentDomain.Evidence;

            var appDomain = AppDomain.CreateDomain("Lint Domain", evidence, setup);

            return appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.FSharpLintWorker+FSharpLintWorker") as FSharpLint.Worker.IFSharpLintWorker;
        }
    }
}
