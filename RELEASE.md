# Releasing a new version of FSharpLint

1. Update the [changelog](CHANGELOG.md) since last release and get the changes onto master.
2. Tag the head of master with the version number in the format `vx.x.x` - for example: `v0.20.2`. 
3. Push the tag to remote.

After pushing the tag to remote, the publish pipeline will be kicked off which will:
* Build the package
* Create a github release
* Pushes the package to NuGet
* Publishes the docs to github pages

## Dependencies

* Github secret: `NUGET_KEY`.
  * This is the API key from nuget, used to push the package to nuget.
  * At the moment the key lives under `duckmatt`'s nuget account, but I believe the `fsprojects` account should also be able to create an applicable API key.
  * "Expires in 7 months" from 2021/11/16
