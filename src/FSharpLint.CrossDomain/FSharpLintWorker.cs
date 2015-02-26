using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FSharpLint.Worker;
using System.ServiceModel;

namespace FSharpLint.CrossDomain
{
    [ServiceContract]
    [ServiceBehavior(InstanceContextMode = InstanceContextMode.Single)]
    public class Reporter : ILintReporter
    {
        private readonly LintOptions lintOptions;

        public Reporter(LintOptions lintOptions)
        {
            this.lintOptions = lintOptions;
        }

        public void ReportProgress(Progress progress)
        {
            this.lintOptions.Progress(progress);
        }

        public void ErrorReceived(Error error)
        {
            this.lintOptions.ErrorReceived(error);
        }
    }

    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        public FSharpLint.Worker.Result RunLint(string projectFile, LintOptions options)
        {
            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup { PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true };

            var evidence = AppDomain.CurrentDomain.Evidence;

            var appDomain = AppDomain.CreateDomain("Lint Domain", evidence, setup);

            var worker = appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.RunLint+FSharpLintWorker") as FSharpLint.Worker.IFSharpLintWorker;

            using (var host = new ServiceHost(new Reporter(options), new[] { new Uri("net.pipe://localhost") }))
            {
                host.AddServiceEndpoint(typeof(ILintReporter), new NetNamedPipeBinding(), "Lint");

                host.Open();

                var result = worker.RunLint(projectFile, null);

                host.Close();

                return result;
            }
        }
    }
}
