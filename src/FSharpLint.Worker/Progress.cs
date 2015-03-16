/// FSharpLint, a linter for F#.
/// Copyright (C) 2014 Matthew Mcveigh
/// 
/// This program is free software: you can redistribute it and/or modify
/// it under the terms of the GNU General Public License as published by
/// the Free Software Foundation, either version 3 of the License, or
/// (at your option) any later version.
/// 
/// This program is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU General Public License for more details.
/// 
/// You should have received a copy of the GNU General Public License
/// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.Worker
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;

    [Serializable]
    public class Progress
    {
        private readonly string filename;

        private readonly ProgressType progress;

        private readonly Exception exception;

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

        public enum ProgressType
        {
            Starting,
            ReachedEnd,
            Failed
        }

        public string Filename 
        {
            get { return this.filename; } 
        }

        public ProgressType State 
        {
            get { return this.progress; } 
        }

        public Exception Exception 
        { 
            get { return this.exception; } 
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
