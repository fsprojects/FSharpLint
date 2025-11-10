#r "nuget: Fornax.Core, 0.16.0"
#if !FORNAX
#load "../../loaders/apirefloader.fsx"
#load "../../loaders/contentloader.fsx"
#load "../../loaders/pageloader.fsx"
#load "../../loaders/globalloader.fsx"
#endif

open Html

let menu (ctx : SiteContents) (page: string) =
  let shortcuts = ctx.GetValues<Pageloader.Shortcut> ()
  let all = ctx.TryGetValues<Apirefloader.AssemblyEntities>() |> Option.defaultValue Seq.empty

  let content = ctx.GetValues<Contentloader.Post> ()
  let siteInfo = ctx.TryGetValue<Globalloader.SiteInfo>()
  let rootUrl = 
    match siteInfo with
    | Some info -> info.root_url
    | None -> ""

  let group = content |> Seq.tryFind (fun n -> n.title = page) |> Option.map (fun n -> n.category)

  let topLevel =
    content
    |> Seq.filter (fun n -> n.category = Contentloader.TopLevel && not n.hide_menu )
    |> Seq.sortBy (fun n -> n.menu_order)

  let howtos =
    content
    |> Seq.filter (fun n -> n.category = Contentloader.HowTo && not n.hide_menu )
    |> Seq.sortBy (fun n -> n.menu_order)

  let apiReferencesSection =
    if Seq.isEmpty all then
      // If no API references are available, don't show the section
      []
    else
      [
        li [Class "dd-item parent"] [
          a [if group = None then Class "active" else Class ""] [!! "API References"]
          ul [Class "child"] [
            for r in all ->
              li [Class "dd-item"] [
                a [Href (rootUrl + "/reference/" +  r.Label + "/index.html"); if r.Label = page then Class "active" else Class "" ] [
                  !! r.Label
                ]
              ]
          ]
        ]
      ]

  let menuHeader =
    [
      li [Class "dd-item"] [
        a [Href rootUrl; if page = "Overview" then Class "active" else Class ""] [!! "Overview"]
      ]
      li [Class "dd-item parent" ] [
        a [if group = Some Contentloader.HowTo then Class "active" else Class ""] [!! "How-To Guides"]
        ul [Class "child"] [
          for r in howtos ->
            li [Class "dd-item"] [
              a [Href (rootUrl + "/" +  r.link); if r.title = page then Class "active" else Class "" ] [
                !! r.title
              ]
            ]
        ]
      ]
      yield! apiReferencesSection
    ]

  let renderShortcuts =
    section [Id "shortcuts"] [
        h3 [] [!! "Shortcuts"]
        ul [] [
          for s in shortcuts do
            yield
              li [] [
                a [Class "padding"; Href s.link ] [
                  i [Class s.icon] []
                  !! s.title
                ]
              ]
        ]
      ]

  let renderFooter =
    section [Id "footer"] [
      !! """<p>Built with <a href="https://github.com/ionide/Fornax">Fornax</a>"""
    ]

  let title = 
    match siteInfo with
    | Some info -> info.title
    | None -> "FSharpLint"

  nav [Id "sidebar"] [
    div [Id "header-wrapper"] [
      div [Id "header"] [
        h2 [Id "logo"] [!! title]
      ]
      div [Class "searchbox"] [
        label [HtmlProperties.Custom ("for", "search-by")] [i [Class "fas fa-search"] []]
        input [HtmlProperties.Custom ("data-search-input", ""); Id "search-by"; Type "search"; Placeholder "Search..."]
        span  [HtmlProperties.Custom ("data-search-clear", "")] [i [Class "fas fa-times"] []]
      ]
      script [Type "text/javascript"; Src (rootUrl + "/static/js/lunr.min.js")] []
      script [Type "text/javascript"; Src (rootUrl + "/static/js/auto-complete.js")] []
      script [Type "text/javascript";] [!! ($"var baseurl ='%s{rootUrl}'")]
      script [Type "text/javascript"; Src (rootUrl + "/static/js/search.js")] []
    ]
    div [Class "highlightable"] [
      ul [Class "topics"] menuHeader
      renderShortcuts
      renderFooter
    ]
  ]

