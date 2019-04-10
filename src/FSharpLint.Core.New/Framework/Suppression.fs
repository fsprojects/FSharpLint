module FSharpLint.Framework.Suppression

open FSharp.Compiler

let checkIfSuppressedBasedOnRange suppressMessageAttributes range ruleName =
    suppressMessageAttributes
    |> Seq.exists (fun (suppressedMessage:Ast.SuppressedMessage, suppressedRange:Range.range) ->
        suppressedMessage.Rule = ruleName || suppressedMessage.Rule  = "*"
        && ExpressionUtilities.rangeContainsOtherRange suppressedRange range)