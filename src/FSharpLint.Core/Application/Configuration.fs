namespace FSharpLint.Framework    

/// Loads configuration files from JSON into an object.
/// When a configuration file has already been loaded, loading another one overwrites the existing configuration.
/// The overwrite works by only changing existing properties with properties from the new file,
/// so properties in the original configuration file not in the new configuration file will remain.
module Configuration =

    open System.Reflection
    open FSharpLint.Framework
    open FSharpLint.Application.ConfigurationManager
    open FSharp.Json

    [<Literal>]
    let SettingsFileName = "fsharplint.json"

    /// Merges settings from `diff` and `partial` with settings from
    /// `diff` taking precedence.
    let private mergeSettings full diff partial =
        full
        |> Map.toList
        |> List.choose (fun (key, _) ->
            match Map.tryFind key diff with
            | Some(value) -> Some(key, value)
            | None ->
                match Map.tryFind key partial with
                | Some(value) -> Some(key, value)
                | None -> None)
        |> Map.ofList

    /// A default configuration specifying every analyser and rule is included as a resource file in the framework.
    /// This function loads and returns this default configuration.
    let defaultConfiguration =
        let assembly = typeof<Rules.Rule>.GetTypeInfo().Assembly
        let resourceName = Assembly.GetExecutingAssembly().GetManifestResourceNames()
                         |> Seq.find (fun n -> n.EndsWith("DefaultConfiguration.json", System.StringComparison.Ordinal))
        use stream = assembly.GetManifestResourceStream(resourceName)
        match stream with
        | null -> failwithf "Resource '%s' not found in assembly '%s'" resourceName (assembly.FullName)
        | stream ->
            use reader = new System.IO.StreamReader(stream)

            reader.ReadToEnd()
            |> parseConfig

    /// Module to manage the loading and updating of configuration files.
    /// Keeps loaded configurations cached in memory so they can be quickly retrieved.
    module Management =

        open System
        open System.IO

        type Path = string list

        [<NoComparison>]
        type GlobalConfig = { Path: Path; Name: string; Configuration: Configuration option }

        /// Keeps configuration files loaded for a list of paths so that
        /// they can be quickly retrieved and updated
        [<NoComparison>]
        type LoadedConfigs =
            { /// Cached configurations for each path.
              LoadedConfigs: Map<Path, Configuration option>

              /// Full paths added, there could be multiple <see cref="LoadedConfigs.LoadedConfigs" />
              /// for each full path. If you wanted to load the configurations for a solution
              /// this should be a list of absolute paths to the project directories.
              PathsAdded: Path list

              /// Global configuration files in order of precedence.
              /// All PathsAdded will override these files.
              GlobalConfigs: GlobalConfig list }

            static member Empty = { LoadedConfigs = Map.empty; PathsAdded = []; GlobalConfigs = [] }

        let private getAllPaths path =
            let rec getAllPaths = function
                | x::rest, currentPath, pathsFound ->
                    let pathFound = currentPath@[x]
                    getAllPaths (rest, pathFound, pathFound::pathsFound)
                | [], _, pathsFound -> pathsFound

            getAllPaths (path, [], []) |> List.rev

        /// Loads all configurations needed to form a complete configuration for a given path.
        /// A `complete configuration` is one that has overridden every configuration file in ancestor directories.
        let addPath tryLoadConfig loadedConfigs path =
            let pathHasAlreadyBeenLoaded =
                loadedConfigs.PathsAdded |> List.exists (fun x -> x = path)

            if pathHasAlreadyBeenLoaded then loadedConfigs
            else
                let paths = getAllPaths path

                let rec updateLoadedConfigs loadedConfigs = function
                | path::rest ->
                    if loadedConfigs |> Map.containsKey path then
                        updateLoadedConfigs loadedConfigs rest
                    else
                        let updatedLoadedConfigs =
                            loadedConfigs |> Map.add path (tryLoadConfig path)

                        updateLoadedConfigs updatedLoadedConfigs rest
                | [] -> loadedConfigs

                { loadedConfigs with
                    PathsAdded = path::loadedConfigs.PathsAdded
                    LoadedConfigs = updateLoadedConfigs loadedConfigs.LoadedConfigs paths }

        let rec private listStartsWith = function
        | (_, []) -> true
        | (x::list, y::startsWithList) when x = y ->
            listStartsWith (list, startsWithList)
        | _ -> false

        let private isPathPartOfAnyPaths path paths =
            paths |> List.exists (fun x -> listStartsWith (x, path))

        /// Removes a loaded path and all cached configurations that aren't used by any other paths.
        let removePath loadedConfigs path =
            let pathNeverLoaded =
                loadedConfigs.PathsAdded |> List.exists (fun x -> x = path) |> not

            if pathNeverLoaded then
                loadedConfigs
            else
                let updatedPaths =
                    loadedConfigs.PathsAdded |> List.filter (fun x -> x <> path)

                let updatedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.filter (fun configPath _ ->
                        isPathPartOfAnyPaths configPath updatedPaths)

                { loadedConfigs with
                    PathsAdded = updatedPaths
                    LoadedConfigs = updatedConfigs }

        /// With a given list of paths, any paths loaded not in the list will be removed
        /// and any in the list but not loaded will be added.
        let updatePaths tryLoadConfig loadedConfigs paths =
            let existingPaths = Set.ofList loadedConfigs.PathsAdded
            let newPaths = Set.ofList paths

            let pathsToAdd = Set.difference newPaths existingPaths |> List.ofSeq

            let pathsToRemove = Set.difference existingPaths newPaths |> List.ofSeq

            let loadedConfigs = pathsToAdd |> List.fold (addPath tryLoadConfig) loadedConfigs
            let loadedConfigs = pathsToRemove |> List.fold removePath loadedConfigs

            loadedConfigs

        let rec private transpose matrix =
            match matrix with
            | (col::cols)::rows ->
                let first = List.map (function [] -> None | h::_ -> Some h) matrix
                let rest = transpose (List.map (function [] -> [] | _::t -> t) matrix)
                first :: rest
            | _ -> []

        /// Attempts to get a path that is common to all paths
        /// that have been added to a node.
        /// If a given preferred path is found to be a common path then
        /// that path will always be returned, useful if you want to prefer
        /// the solution directory for example.
        let commonPath loadedConfigs preferredPath =
            let commonPath =
                transpose loadedConfigs.PathsAdded
                |> Seq.takeWhile (function
                    | (first::_) as segments -> List.forall ((=) first) segments
                    | [] -> false)
                |> Seq.toList
                |> List.choose List.head

            if List.isEmpty commonPath then None
            else if listStartsWith (commonPath, preferredPath) then Some preferredPath
            else Some commonPath

        /// Tries to reload the configuration for all paths.
        /// Call when the user has edited a configuration file on disk.
        let refresh tryLoadConfig loadedConfigs =
            { loadedConfigs with
                  LoadedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.map (fun configPath _ -> tryLoadConfig configPath)
                  GlobalConfigs =
                    loadedConfigs.GlobalConfigs
                    |> List.map (fun x -> { x with Configuration = tryLoadConfig x.Path }) }

        /// Gets the configuration file located at a given path.
        /// The configuration file returned may be incomplete as it
        /// will not have overrided any previous configuration files.
        let getPartialConfig loadedConfigs path =
            let config = Map.tryFind path loadedConfigs.LoadedConfigs

            match config with
            | Some(Some(config)) -> Some(config)
            | Some(None) | None ->
                List.tryFind (fun { Path = x } -> x = path) loadedConfigs.GlobalConfigs
                |> Option.bind (fun x -> x.Configuration)

        let private tryOverrideConfig (configToOverride:Configuration option) (config:Configuration option) =
            match (configToOverride, config) with
            | Some(configToOverride), Some(config) ->
                Some(configToOverride.Override(config))
            | Some(x), None
            | None, Some(x) -> Some(x)
            | None, None -> None
            
        let overrideConfiguration (configToOverride:Configuration) (config:Configuration) =
            configToOverride.Override(config)

        /// Gets the complete configuration file located at a given path.
        /// "complete" configuration means that it has overridden any previous
        /// configuration files.
        let getConfig loadedConfigs path =
            let (globalConfigs, pathWasAGlobalConfig) =
                let rec takeUntilPathMatch built = function
                | config::_ when config.Path = path -> (config::built, true)
                | config::rest -> takeUntilPathMatch (config::built) rest
                | [] -> (built, false)

                takeUntilPathMatch [] loadedConfigs.GlobalConfigs
                |> fun (configs, matchFound) -> (List.rev configs, matchFound)

            let globalConfig =
                globalConfigs
                |> List.map (fun x -> x.Configuration)
                |> List.fold tryOverrideConfig (Some defaultConfiguration)

            if pathWasAGlobalConfig then globalConfig
            else
                getAllPaths path
                |> List.fold (fun config path ->
                    match loadedConfigs.LoadedConfigs.TryFind path with
                    | Some(loadedConfig) -> tryOverrideConfig config loadedConfig
                    | None -> config) globalConfig

        /// Updates a configuration file at a given path.
        let updateConfig loadedConfigs path config =
            { loadedConfigs with
                  LoadedConfigs =
                    loadedConfigs.LoadedConfigs
                    |> Map.map (fun key value -> if key = path then config else value)
                  GlobalConfigs =
                    loadedConfigs.GlobalConfigs
                    |> List.map (fun globalConfig ->
                        if globalConfig.Path = path then { globalConfig with Configuration = config }
                        else globalConfig) }

        /// Tries to normalise paths to a format that can be used as a path in <see cref="LoadedConfigs" />.
        let normalisePath (path:string) =
            Path.GetFullPath path
            |> fun x -> x.Split([|Path.DirectorySeparatorChar|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
