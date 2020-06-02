namespace FSharpLint.Console

open System
open System.Reflection
open System.Resources

/// Provides a way of getting string values from the resource files.
/// Used to retrieve multi-lingual strings inside of the app.
type internal Resources() =
    static let resourceName =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("Text.resources", StringComparison.Ordinal))

    static let resourceManager =
        ResourceManager(resourceName.Replace(".resources", String.Empty), typeof<Resources>.GetTypeInfo().Assembly)

    /// Returns the value of the specified string resource for the current culture.
    static member GetString(name) = resourceManager.GetString(name)

    /// Returns the value of the specified string resource for a given culture.
    static member GetString(name, culture) = resourceManager.GetString(name, culture)

    static member Format(name, [<System.ParamArray>] args) =
        let format = resourceManager.GetString(name)
        String.Format(format, args)
