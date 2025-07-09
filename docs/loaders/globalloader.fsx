#r "nuget: Fornax.Core, 0.16.0-beta002"

type SiteInfo = {
    title: string
    description: string
    theme_variant: string option
    root_url: string
}

let config = {
    title = "FSharpLint"
    description = "Lint tool for F#"
    theme_variant = Some "blue"
    root_url =
      #if WATCH
        "http://localhost:8080"
      #else
        "https://fsprojects.github.io/FSharpLint"
      #endif
}

let loader (projectRoot: string) (siteContet: SiteContents) =
    siteContet.Add(config)

    siteContet
