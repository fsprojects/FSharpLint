#XmlDocumentation

Set of rules that analyse whether or not certain areas of code contain XML documentation.

###Rationale

Provides users of your code more information through their IDE e.g. intellisense.

###Analyser Settings

`Enabled` - A boolean property that can enable and disable this analyser. (Default false)

####Each Rule Settings
`Access` - A string property that defines when the rule is applied. (Default None)

`Enabled` - A boolean property that can enable and disable this rule. (Default false)

Allowed `Access` values:
* Public - only evaluate when the rule target is public
* Internal - only evaluate when the rule target is internal
* Private - only evaluate when the rule target is private
* All - always evaluate the rule
* NotPrivate - evaluate the rule when the rule target is Public or Internal
* NotPublic - evaluate the rule when the rule target is Internal or Private
* None - never evaluate the rule

###Rules

####ModuleDefinitionHeader

#####Cause

A module was defined and did not have a header XML documentation comment e.g.

    module XmlDocumentation

#####How To Fix

Add XML header documentation to the module definition e.g.

    /// Rules to enforce the use of XML documentation in various places.
    module XmlDocumentation


####ExceptionDefinitionHeader

#####Cause

An exception was defined and did not have a header XML documentation comment e.g.

    exception ParseException of string

#####How To Fix

Add XML header documentation to the exception definition e.g.

    /// Failure during parsing, contains a failure message.
    exception ParseException of string


####TypeDefinitionHeader

#####Cause

A type was defined and did not have a header XML documentation comment e.g.

    type SimpleType =
        member this.Member = "Foo"

#####How To Fix

Add XML header documentation to the type definition e.g.

    /// A simple type
    type SimpleType =
        member this.Member = "Foo"

####MemberDefinitionHeader

#####Cause

A member was defined and did not have a header XML documentation comment e.g.

    type IsAType =
        member this.Member = "Foo"

#####How To Fix

Add XML header documentation to the member definition e.g.

    type IsAType =
        /// A simple member
        member this.Member = "Foo"

####EnumDefinitionHeader

#####Cause

An enum case was defined and did not have a header XML documentation comment e.g.

    type Colors =
        | Red = 0
        | Green = 1
        | Blue = 2

#####How To Fix

Add XML header documentation to the enum case definition e.g.

    type Colors =
        /// This is red
        | Red = 0
        /// This is green
        | Green = 1
        /// This is blue
        | Blue = 2

####UnionDefinitionHeader

#####Cause

A union case was defined and did not have a header XML documentation comment e.g.

    type Shape =
        | Rectangle of width : float * length : float
        | Circle of radius : float
        | Shape

#####How To Fix

Add XML header documentation to the union case definition e.g.

    type Shape =
        /// this is a Rectangle
        | Rectangle of width : float * length : float
        /// this is a Circle
        | Circle of radius : float
        /// this is a Shape
        | Shape

####RecordDefinitionHeader

#####Cause

A record field was defined and did not have a header XML documentation comment e.g.

    type GeoCoord = {
        lat: float
        long: float
        }

#####How To Fix

Add XML header documentation to the record field definition e.g.

    type GeoCoord = {
        /// this is latitude
        lat: float
        /// this is longitude
        long: float
        }

####AutoPropertyDefinitionHeader

#####Cause

A property was defined and did not have a header XML documentation comment e.g.

    type GeoCoord() =
        member val Lat = 0 with get, set
        member val Long = 0 with get, set

#####How To Fix

Add XML header documentation to the property definition e.g.

    type GeoCoord() =
        /// this is latitude
        member val Lat = 0 with get, set
        /// this is longitude
        member val Long = 0 with get, set

####LetDefinitionHeader

#####Cause

A let binding was defined and did not have a header XML documentation comment e.g.

    let add1 x = x + 1

#####How To Fix

Add XML header documentation to the let binding definition e.g.

    /// add 1 to a number
    let add1 x = x + 1
