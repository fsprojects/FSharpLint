using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    [Serializable]
    public class Result
    {
        public string Message { get; set; }

        public bool IsSuccess { get; set; }

        public static Result Failure(string message)
        {
            return new Result { Message = message };
        }

        public static Result Success()
        {
            return new Result { IsSuccess = true };
        }
    }
}
