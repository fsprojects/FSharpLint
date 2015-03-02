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
    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.ICrossDomainWorker
    {
        private LintOptions options;

        private readonly BlockingCollection<object> reportsReceived = new BlockingCollection<object>();

        private readonly CancellationTokenSource cancelToken = new CancellationTokenSource();

        private readonly TaskCompletionSource<bool> taskCompletionSource = new TaskCompletionSource<bool>();

        public FSharpLint.Worker.Result RunLint(string projectFile, LintOptions options)
        {
            this.options = options;

            var worker = GetWorker();

            worker.ErrorReceived += new ErrorReceivedEventHandler(ReportError);

            var task = new Task(ReportResults);

            task.Start();

            var result = worker.RunLint(projectFile);

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
                    var report = reportsReceived.Take(cancelToken.Token);

                    if (report is Error)
                    {
                        options.ErrorReceived((Error)report);
                    }
                    else
                    {
                        options.Progress((Progress)report);
                    }
                }
                catch (OperationCanceledException) { }
            }
        }

        [OneWay]
        public void ReportError(Error error)
        {
            reportsReceived.Add(error);
        }

        [OneWay]
        public void ReportProgress(Progress progress)
        {
            reportsReceived.Add(progress);
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
