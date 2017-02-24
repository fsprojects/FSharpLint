namespace FSharpLint.Framework

open System.Reflection
open System.Resources

/// Provides a way of getting string values from the framework's resource files (files in src/FSharpLint.Framework/Resources/).
/// Used to retrieve multi-lingual strings inside of the app.
type Resources() =
    static let resourceManager = ResourceManager("Text", typeof<Resources>.GetTypeInfo().Assembly)

    /// Returns the value of the specified string resource for the current culture.
    static member GetString(name) = resourceManager.GetString(name)

    /// Returns the value of the specified string resource for a given culture.
    static member GetString(name, culture) = resourceManager.GetString(name, culture)