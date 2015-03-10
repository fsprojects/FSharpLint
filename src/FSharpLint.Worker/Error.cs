using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    [Serializable]
    public class Error
    {
        public string Info { get; set; }
        public Range Range { get; set; }
        public string Input { get; set; }
        public string FormattedError { get; set; }
    }
}
