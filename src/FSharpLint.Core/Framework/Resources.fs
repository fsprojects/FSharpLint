namespace FSharpLint.Framework

open System.Reflection
open System.Resources

/// Provides a way of getting string values from the framework's resource files (files in src/FSharpLint.Framework/Resources/).
/// Used to retrieve multi-lingual strings inside of the app.
type Resources() =
    static let resourceName = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                              |> Seq.find (fun resource -> resource.EndsWith("Text.resources", System.StringComparison.Ordinal))

    static let resourceManager = ResourceManager(resourceName.Replace(".resources", System.String.Empty), typeof<Resources>.GetTypeInfo().Assembly)

    /// Returns the value of the specified string resource for the current culture.
    static member GetString(name) = resourceManager.GetString(name)

    /// Returns the value of the specified string resource for a given culture.
    static member GetString(name, culture) = resourceManager.GetString(name, culture)