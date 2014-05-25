(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Framework

[<System.Diagnostics.DebuggerNonUserCodeAttribute()>]
type Resources() =
    static let resourceManager = System.Resources.ResourceManager("Resources.Text", typeof<Resources>.Assembly)

    static member GetString(str) = resourceManager.GetString(str)

    static member GetString(str, culture) = resourceManager.GetString(str, culture)