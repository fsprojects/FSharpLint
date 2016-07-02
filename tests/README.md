# Test Projects

Project Name | Description
------------ | --------
**`FSharpLint.Core.Tests`** | Unit tests for `FSharpLint.Core`.
**`FSharpLint.Core.Tests.netcore`** | dotnetcore support for `FSharpLint.Core.Tests`.
**`FSharpLint.FunctionalTest`** | End to end acceptance tests for all projects.
**`FSharpLint.FunctionalTest.TestedProject`** | Test input for acceptance tests in `FSharpLint.FunctionalTest`.

Note: *Performance tests are included but should not form part of the build process, at least not on the CI server. Any performance tests added must be given the (test category)[http://www.nunit.org/index.php?p=category&r=2.2.10] of `"Acceptance Tests"`*