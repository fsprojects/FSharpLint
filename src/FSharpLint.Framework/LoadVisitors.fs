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

    open System
    open System.Linq
    open System.Reflection
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Range
    open FSharpLint.Framework

    /// Visitor that visits the nodes in the abstract syntax trees of the F# files in a project.
    type AstVisitor = Ast.VisitorInfo -> FSharpCheckFileResults option -> Ast.Visitor

    type PlainTextVisitorInfo =
        { File: string
          Input: string
          SuppressedMessages: (Ast.SuppressedMessage * range) list
          StringLiterals: (string * range) list }

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

    // TODO: Support suppressed messages.
    type SyntaxArrayVisitor = Ast.VisitorInfo -> AbstractSyntaxArray.Node [] -> unit

    type VisitorType =
        | Ast of AstVisitor
        | PlainText of PlainTextVisitor
        | SyntaxArray of SyntaxArrayVisitor

    type VisitorPlugin =
        { Name: string
          Visitor: VisitorType }

    /// Interface to be implemented to register a plugin.
    type IRegisterPlugin =
        abstract RegisterPlugin : VisitorPlugin with get

    /// Loads all registered visitors (files containing a class implementing IRegisterPlugin) from a given assembly.
    let loadPlugins (assembly:Assembly) =
        let isPluginType (t:Type) = 
            if t.GetInterfaces().Contains(typeof<IRegisterPlugin>) then
                match t.GetConstructor(Type.EmptyTypes) with
                | null -> false
                | _ -> true
            else
                false

        let pluginFromType t = 
            if isPluginType t then
                match Activator.CreateInstance(t) with
                | :? IRegisterPlugin as plugin -> Some(plugin.RegisterPlugin)
                | _ -> None
            else
                None

        assembly.GetTypes()
        |> Array.choose pluginFromType
        |> Array.toList