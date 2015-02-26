using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    [ServiceContract]
    public interface ILintReporter
    {
        [OperationContract]
        void ReportProgress(Progress progress);

        [OperationContract]
        void ErrorReceived(Error error);
    }

    public interface IFSharpLintWorker
    {
        Result RunLint(string projectFile, LintOptions options);
    }
}
