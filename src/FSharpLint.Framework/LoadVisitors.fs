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

/// Dynamically loads visitors (plugins) from an assembly.
/// Used so that visitors can easily be registered using the Hollywood principle 
/// by implementing IRegisterPlugin, this means a new visitor can be added 
/// by just adding a single new file to a project without having to modify any other files.
module LoadVisitors =

    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework
    open System.Linq

    /// Visitor that visits the nodes in the abstract syntax trees of the F# files in a project.
    type AstVisitor = Ast.VisitorInfo -> FSharpCheckFileResults -> Ast.Visitor

    type PlainTextVisitorInfo =
        {
            File: string
            Input: string
            SuppressedMessages: (Ast.SuppressedMessage * range) list
        }

        with
            /// Has a given rule been suppressed by SuppressMessageAttribute?
            member this.IsSuppressed(range:range, analyserName, ?rulename) =
                let rangeContainsOtherRange (containingRange:range) (range:range) =
                    range.StartLine >= containingRange.StartLine && range.EndLine <= containingRange.EndLine

                let isAnalyserSuppressed (suppressedMessage:Ast.SuppressedMessage, suppressedMessageRange:range) =
                    suppressedMessage.Category = analyserName && 
                    (Option.exists ((=) suppressedMessage.Rule) rulename || suppressedMessage.Rule = "*") &&
                    rangeContainsOtherRange suppressedMessageRange range

                this.SuppressedMessages |> List.exists isAnalyserSuppressed

    /// Visitor that visists the plain text of the F# files in a project.
    type PlainTextVisitor = Ast.VisitorInfo -> PlainTextVisitorInfo -> unit

    type VisitorType =
        | Ast of AstVisitor
        | PlainText of PlainTextVisitor

    type VisitorPlugin =
        {
            Name: string
            Visitor: VisitorType
        }

    /// Interface to be implemented to register a plugin.
    type IRegisterPlugin =
        abstract RegisterPlugin : VisitorPlugin with get

    [<RequireQualifiedAccess>]
    type CheckConfigResult =
        | Failed of string
        | Success

    /// Extension of IRegisterPlugin that gets passed a configuration file before the plugin is registered. 
    type IRegisterPluginWithConfigChecker =
        abstract CheckConfig : Map<string, Configuration.Analyser> -> CheckConfigResult

        inherit IRegisterPlugin

    /// Loads all implementations of IRegisterPluginWithConfigChecker from a given assembly.
    let loadConfigCheckers (assembly:System.Reflection.Assembly) =
        assembly.GetTypes()
                |> Array.filter (fun (t:System.Type) -> 
                                    t.GetInterfaces().Contains(typeof<IRegisterPluginWithConfigChecker>)
                                    && t.GetConstructor(System.Type.EmptyTypes) <> null)
                |> Array.toList

    /// <summary>
    /// Gets a list of failures that occurred when checking a given config file using a given list of checkConfigs.
    /// </summary>
    /// <param name="checkConfigs">List of config checkers, these can be loaded from an assembly using loadConfigCheckers</param>
    let checkConfigsForFailures config checkConfigs =
        checkConfigs
            |> List.map (fun (t:System.Type) -> System.Activator.CreateInstance(t) :?> IRegisterPluginWithConfigChecker)
            |> List.map (fun (x:IRegisterPluginWithConfigChecker) -> x.CheckConfig(config))
            |> List.choose (function 
                | CheckConfigResult.Failed(failMessage) -> Some(failMessage) 
                | CheckConfigResult.Success -> None)

    /// Loads all registered visitors (files containing a class implementing IRegisterPlugin) from a given assembly.
    let loadPlugins (assembly:System.Reflection.Assembly) =
        assembly.GetTypes()
                |> Array.filter (fun (t:System.Type) -> 
                                    t.GetInterfaces().Contains(typeof<IRegisterPlugin>)
                                    && t.GetConstructor(System.Type.EmptyTypes) <> null)
                |> Array.map (fun (t:System.Type) -> System.Activator.CreateInstance(t) :?> IRegisterPlugin)
                |> Array.map (fun (x:IRegisterPlugin) -> x.RegisterPlugin)
                |> Array.toList