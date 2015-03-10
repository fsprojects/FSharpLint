using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharpLint.Worker
{
    [Serializable]
    public class Progress
    {
        private readonly string filename;

        private readonly ProgressType progress;

        private readonly Exception exception;

        public string Filename { get { return filename; } }

        public ProgressType State { get { return progress; } }

        public Exception Exception { get { return exception; } }

        public enum ProgressType
        {
            Starting,
            ReachedEnd,
            Failed
        }

        public Progress(string filename, ProgressType progress)
        {
            this.filename = filename;
            this.progress = progress;
        }

        public Progress(string filename, ProgressType progress, Exception exception) 
            : this(filename, progress)
        {
            this.exception = exception;
        }

        public static Progress Starting(string filename)
        {
            return new Progress(filename, ProgressType.Starting);
        }

        public static Progress ReachedEnd(string filename)
        {
            return new Progress(filename, ProgressType.ReachedEnd);
        }

        public static Progress Failed(string filename, Exception exception)
        {
            return new Progress(filename, ProgressType.Failed);
        }
    }
}
