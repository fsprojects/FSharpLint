#r "nuget: Fornax.Core, 0.16.0-beta005"

open Config

let customRename (page: string) =
    System.IO.Path.ChangeExtension(page.Replace ("content/", ""), ".html")


let config = {
    Generators = [
        {Script = "page.fsx"; Trigger = OnFileExt ".md"; OutputFile = Custom customRename }
        {Script = "apiref.fsx"; Trigger = Once; OutputFile = MultipleFiles (sprintf "reference/%s.html") }

        {Script = "lunr.fsx"; Trigger = Once; OutputFile = NewFileName "index.json" }
    ]
}
