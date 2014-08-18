#XmlDocumentation

Set of rules that analyse whether or not certain areas of code contain XML documentation.

###Analyser Settings

N/A

###Rules

|Rules|
|---|
|[ExceptionDefinitionHeader](#exceptiondefinitionheader)|

####ExceptionDefinitionHeader

#####Cause

An exception was defined and did not have a header XML documentation comment e.g.

    exception ParseException of string

#####Rationale

Provides users of your code more information through their IDE e.g. intellisense.

#####How To Fix

Add XML header documentation to the exception definition e.g.

    /// Failure during parsing, contains a failure message.
    exception ParseException of string

#####Rule Settings

`Enabled` - A boolean property that can enable and disable this rule. (Default false)