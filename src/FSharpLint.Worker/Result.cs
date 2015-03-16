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
    /// Represents the result of an operation as either a success or a failure.
    /// </summary>
    [Serializable]
    public class Result
    {
        /// <summary>
        /// Gets the failure message.
        /// </summary>
        public string Message { get; private set; }

        /// <summary>
        /// Gets a value indicating whether the result is a success.
        /// </summary>
        public bool IsSuccess { get; private set; }

        /// <summary>
        /// Creates a failure instance.
        /// </summary>
        /// <param name="message">Failure message.</param>
        /// <returns>Instance of <see cref="Result"/> representing a failure.</returns>
        public static Result Failure(string message)
        {
            return new Result { Message = message };
        }

        /// <summary>
        /// Creates a success instance.
        /// </summary>
        /// <returns>Instance of <see cref="Result"/> representing a failure.</returns>
        public static Result Success()
        {
            return new Result { IsSuccess = true };
        }
    }
}
