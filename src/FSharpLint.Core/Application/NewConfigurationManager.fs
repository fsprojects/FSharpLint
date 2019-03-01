module FSharpLint.Application.NewConfiguration

open Newtonsoft.Json
open FSharpLint.Framework.Rules
open FSharpLint.Rules

type FormattingConfig =
    { typedItemSpacing : TypedItemSpacing.Config option }

type Configuration = 
    { formatting : FormattingConfig option }
    
let parseConfig (configText : string) =
    let settings = JsonSerializerSettings()
    settings.Converters.Add(Converters.StringEnumConverter())
    JsonConvert.DeserializeObject<Configuration> configText
    
let flattenFormattingConfig (config : FormattingConfig) =
    config.typedItemSpacing
    |> Option.map TypedItemSpacing.rule
    |> Option.toArray
    
let flattenConfig (config : Configuration) =
    config.formatting
    |> Option.map flattenFormattingConfig
    |> Option.toArray
    |> Array.concat
