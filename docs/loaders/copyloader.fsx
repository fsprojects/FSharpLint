#r "nuget: Fornax.Core, 0.16.0-beta005"

open System.IO


let loader (projectRoot: string) (siteContet: SiteContents) =
    let intputPath = Path.Combine(projectRoot, "static")
    let outputPath = Path.Combine(projectRoot, "_public", "static")
    if Directory.Exists outputPath then Directory.Delete(outputPath, true)
    Directory.CreateDirectory outputPath |> ignore

    for dirPath in Directory.GetDirectories(intputPath, "*", SearchOption.AllDirectories) do
        Directory.CreateDirectory(dirPath.Replace(intputPath, outputPath)) |> ignore

    for filePath in Directory.GetFiles(intputPath, "*.*", SearchOption.AllDirectories) do
        File.Copy(filePath, filePath.Replace(intputPath, outputPath), true)

    siteContet
