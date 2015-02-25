using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    public interface IFSharpLintWorker
    {
        Result RunLint(string projectFile, LintOptions options);
    }
}
