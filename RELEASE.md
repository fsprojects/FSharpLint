# Releasing a new version of FSharpLint

1. Choose a version (X.Y.Z) for your new release, either by increasing the Major (X) or Minor (Y) part of the version by 1, or by increasing the
Revision (Z) part of the version by 2, e.g.: `0.23.2 -> 0.24.0` or `0.23.2 -> 1.0` or `0.23.2 -> 0.23.4`
2. Update the [changelog](CHANGELOG.md) since last release: add the entry (`## [X.Y.Z]`) summarizing all the changes since the previous release
(ideally one line per PR or commit).
3. Commit, but don't push yet!
4. Tag the head of master with the version number in the format `vx.x.x` - e.g.: `git tag v0.20.4`.
5. Push to remote: `git push origin vX.Y.Z master` - this will start CI processes that will:
* Build the package
* Create a github release
* Pushes the package to NuGet
* Publishes the docs to github pages

## Dependencies

* Github secret: `NUGET_KEY`.
  * This is the API key from nuget, used to push the package to nuget.
  * At the moment the key lives under `knocte`'s nuget account, but maybe the `fsprojects` account should also be able to create an applicable API key.
  * "Expires in 12 months" from 2023/11/23
