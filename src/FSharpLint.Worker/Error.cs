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
    /// Represents a lint error found in the user's code.
    /// C# representation of <see cref="FSharpLint.Application.LintWarning.Error"/>.
    /// </summary>
    [Serializable]
    public class Error
    {
        /// <summary>
        /// Gets or sets the description of the rule broken that caused the error.
        /// </summary>
        public string Info { get; set; }

        /// <summary>
        /// Gets or sets the location of the error.
        /// </summary>
        public Range Range { get; set; }

        /// <summary>
        /// Gets or sets the contents of the file containing the error.
        /// </summary>
        public string Input { get; set; }

        /// <summary>
        /// Gets or sets an error message that can be displayed to the user.
        /// Contains the broken rule's description, and the line containing the error with
        /// the error marked.
        /// </summary>
        public string FormattedError { get; set; }
    }
}
