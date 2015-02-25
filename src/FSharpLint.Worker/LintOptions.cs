using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    [Serializable]
    public class LintOptions
    {
        public Func<bool> FinishEarly { get; set; }
        public Action<Progress> Progress { get; set; }
        public Action<Error> ErrorReceived { get; set; }
    }
}
