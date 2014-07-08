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

/// Provides a way of getting string values from the framework's resource files (files in src/FSharpLint.Framework/Resources/).
/// Used to retrieve multi-lingual strings inside of the app.
type Resources() =
    static let resourceName =
        let isRunningOnMono = System.Type.GetType("Mono.Runtime") <> null
        if isRunningOnMono then
            "Text"
        else
            "Resources.Text"

    static let resourceManager = System.Resources.ResourceManager(resourceName, typeof<Resources>.Assembly)

    /// Returns the value of the specified string resource for the current culture.
    static member GetString(name) = resourceManager.GetString(name)

    /// Returns the value of the specified string resource for a given culture.
    static member GetString(name, culture) = resourceManager.GetString(name, culture)