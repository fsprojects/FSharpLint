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

        private LintOptions options;

        private readonly BlockingCollection<Error> errorsReceived = new BlockingCollection<Error>();

        private readonly CancellationTokenSource cancelToken = new CancellationTokenSource();

        private TaskCompletionSource<bool> taskCompletionSource = new TaskCompletionSource<bool>();

        public FSharpLint.Worker.Result RunLint(string projectFile, LintOptions options)
        {
            this.options = options;

            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup { LoaderOptimization = LoaderOptimization.SingleDomain, PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true };

            var evidence = AppDomain.CurrentDomain.Evidence;

            var appDomain = AppDomain.CreateDomain("Lint Domain", evidence, setup);

            var worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") as FSharpLint.Worker.IFSharpLintWorker;

            worker.ErrorReceived += new ErrorReceivedEventHandler(ReportError);

            Task task = new Task(ReportResults);

            task.Start();

            var result = worker.RunLint(projectFile, null);

            cancelToken.Cancel(false);

            task.Wait();

            return result;
        }

        public void ReportResults()
        {
            while (!cancelToken.IsCancellationRequested)
            {
                try
                {
                    options.ErrorReceived(errorsReceived.Take(cancelToken.Token));
                }
                catch (OperationCanceledException) { }
            }
        }

        [OneWay]
        public void ReportError(Error error)
        {
            errorsReceived.Add(error);
        }
    }
}
