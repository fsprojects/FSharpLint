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
    using System.ServiceModel;
    using System.Text;
    using System.Threading.Tasks;

    /// <summary>
    /// Handles lint errors received while linting a project.
    /// </summary>
    /// <param name="error">Lint error.</param>
    public delegate void ErrorReceivedEventHandler(Error error);

    /// <summary>
    /// Handles the current progress of lint running on a project.
    /// </summary>
    /// <param name="progress">Linter progress.</param>
    public delegate void ReportProgressEventHandler(Progress progress);

    /// <summary>
    /// Worker interface to be used to implement proxy objects to run lint in separate app domains.
    /// </summary>
    public interface IFSharpLintWorker
    {
        /// <summary>
        /// Reports any lint errors found while running lint.
        /// </summary>
        event ErrorReceivedEventHandler ErrorReceived;

        /// <summary>
        /// Reports progress made while running lint so we know what file is currently being linted.
        /// </summary>
        event ReportProgressEventHandler ReportProgress;

        /// <summary>
        /// Lints a given project file.
        /// </summary>
        /// <param name="projectFile">Path to the project file to be linted.</param>
        /// <returns>Result indicating whether the lint ran successfully or failed.</returns>
        Result RunLint(string projectFile);
    }
}
