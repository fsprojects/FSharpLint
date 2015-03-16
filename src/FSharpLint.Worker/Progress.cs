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

    /// <summary>
    /// Provides information on the current progress of the linter.
    /// C# representation of <see cref="FSharpLint.Application.RunLint.ParserProgress"/>.
    /// </summary>
    [Serializable]
    public class Progress
    {
        private readonly string filename;

        private readonly ProgressType progress;

        private readonly Exception exception;

        private Progress(string filename, ProgressType progress)
        {
            this.filename = filename;
            this.progress = progress;
        }

        private Progress(string filename, ProgressType progress, Exception exception) 
            : this(filename, progress)
        {
            this.exception = exception;
        }

        public enum ProgressType
        {
            /// <summary>
            /// Started linting a file.
            /// </summary>
            Starting,

            /// <summary>
            /// Linter reached the end of a file.
            /// </summary>
            ReachedEnd,

            /// <summary>
            /// Linter failed to lint the file.
            /// </summary>
            Failed
        }

        /// <summary>
        /// Gets the name of the file the progress is reporting for.
        /// </summary>
        public string Filename 
        {
            get { return this.filename; } 
        }

        /// <summary>
        /// Gets the type of progress being reported.
        /// </summary>
        public ProgressType State 
        {
            get { return this.progress; } 
        }

        /// <summary>
        /// Gets the exception that caused the lint to fail progress on a file.
        /// </summary>
        /// <remarks>This should only have a value when the progress type is Failed.</remarks>
        public Exception Exception 
        { 
            get { return this.exception; } 
        }

        /// <summary>
        /// Creates an instance that reports that the linter has started linting a given file.
        /// </summary>
        /// <param name="filename">File being linted.</param>
        /// <returns>Instance that reports that the linter has started linting a given file.</returns>
        public static Progress Starting(string filename)
        {
            return new Progress(filename, ProgressType.Starting);
        }

        /// <summary>
        /// Creates an instance that reports that the linter has reached the end of a given file.
        /// </summary>
        /// <param name="filename">File that was linted.</param>
        /// <returns>Instance that reports that the linter has reached the end of a given file.</returns>
        public static Progress ReachedEnd(string filename)
        {
            return new Progress(filename, ProgressType.ReachedEnd);
        }

        /// <summary>
        /// Creates an instance that reports that the linter has failed to lint a file.
        /// </summary>
        /// <param name="filename">Name of the file that failed.</param>
        /// <param name="exception">Exception that occurred that caused the failure.</param>
        /// <returns>Instance that reports that the linter has failed to lint a file.</returns>
        public static Progress Failed(string filename, Exception exception)
        {
            return new Progress(filename, ProgressType.Failed);
        }
    }
}
