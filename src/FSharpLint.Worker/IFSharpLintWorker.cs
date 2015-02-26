using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    public delegate void ErrorReceivedEventHandler(Error error);

    public delegate void ReportProgressEventHandler(Progress args); 

    public interface IFSharpLintWorker
    {
        event ErrorReceivedEventHandler ErrorReceived;

        event ReportProgressEventHandler ReportProgress;

        Result RunLint(string projectFile, LintOptions options);
    }
}
