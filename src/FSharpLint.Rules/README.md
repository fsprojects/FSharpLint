#FSharpLint.Rules

##Writing a Rule

An analyser (visitor) can be used to check one or more rules, each rule should be able to be enabled and disabled using a configuration file. Rules are designed to be simple to add - a new rule can be created by adding a single file without modifying any files other than the [default configuration file](https://github.com/duckmatt/FSharpLint/blob/master/src/FSharpLint.Framework/DefaultConfiguration.FSharpLint).

###Types of Analysers

An analyser can analyse either the plaintext in a file or the abstract syntax tree of a file.

#####Function definition of a plaintext analyser:

    FSharpLint.Framework.Ast.VisitorInfo -> string -> unit
    
The second parameter is the content of the file, the third is the absolute path of the file.

######Function definition of a AST analyser:

    FSharpLint.Framework.Ast.VisitorInfo -> FSharpLint.Framework.Ast.CurrentNode -> FSharpLint.Framework.Ast.VisitorResult
    
Check [src/Framework's README](https://github.com/duckmatt/FSharpLint/tree/master/src/FSharpLint.Framework) for information on how to write an AST visitor.

###Posting Errors and Reading Configuration

An instance of ```FSharpLint.Framework.Ast.VisitorInfo``` is passed as a parameter to all analysers, this contains a field ```PostError``` which should be used to 'log' any errors, and a field ```Config``` which contains the configuration of the tool.

###Registering an Analyser

Registering rules follows the [Hollywood Principle](http://en.wikipedia.org/wiki/Hollywood_principle), you must implement ```IRegisterPlugin``` from ```FSharpLint.Framework.LoadAnalysers``` with the ```RegisterPlugin``` returning your plugin. No more needs to be done.

An example of registering an analyser's visitor:

    open FSharpLint.Framework.LoadAnalysers

    type RegisterAnalyser() = 
        let plugin =
            {
                Name = AnalyserName
                Analyser = Ast(visitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin
