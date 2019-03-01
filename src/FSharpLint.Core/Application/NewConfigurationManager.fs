module FSharpLint.Application.NewConfiguration

open Newtonsoft.Json
open FSharpLint.Rules
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Linq

type FormattingConfig =
    { typedItemSpacing : TypedItemSpacing.Config option }

type Configuration = 
    { ignoreFiles : string []
      formatting : FormattingConfig option }

let mergeConfig (baseConfig : string) (overridingConfig : string) =
    let baseConfigJson = JObject.Parse baseConfig
    let overridingConfigJson = JObject.Parse overridingConfig
    baseConfigJson.Merge(overridingConfig)
    baseConfigJson.ToString()

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
