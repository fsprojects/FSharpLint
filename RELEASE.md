# Releasing a new version of FSharpLint

1. Update the [changelog](CHANGELOG.md) since last release: choose a version that increases the Major or Minor part of the version by 1, or the Revision
part of the version by 2, e.g.: `0.20.2` -> `0.23.0` or `0.20.2` -> `1.0` or `0.20.2` -> `0.20.4`, and add the entry (`## [X.Y.Z]`) summarizing all the
changes since the previous release (ideally one line per PR or commit).
2. Commit and push the change onto master branch, and wait until CI finishes successfully.
3. Tag the head of master with the version number in the format `vx.x.x` - e.g.: `git tag v0.20.4`.
4. Push the tag to remote: `git push origin vX.Y.Z` - this will start CI process that will create a GitHub release and publish the packages to NuGet.

After pushing the tag to remote, the publish pipeline will be kicked off which will:
* Build the package
* Create a github release
* Pushes the package to NuGet
* Publishes the docs to github pages

## Dependencies

* Github secret: `NUGET_KEY`.
  * This is the API key from nuget, used to push the package to nuget.
  * At the moment the key lives under `knocte`'s nuget account, but maybe the `fsprojects` account should also be able to create an applicable API key.
  * "Expires in 12 months" from 2023/11/23
