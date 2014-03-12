// Guids.cs
// MUST match guids.h
using System;

namespace MattMcveigh.FSharpLint_VisualStudioExtension
{
    internal static class GuidList
    {
        public const string guidFSharpLint_VisualStudioExtensionPkgString = "ef97a4ac-9d42-4f6c-93c2-b33a75264787";
        public const string guidFSharpLint_VisualStudioExtensionCmdSetString = "1d05d91c-081c-456c-8d53-b19a283dcbaf";

        public static readonly Guid guidFSharpLint_VisualStudioExtensionCmdSet = new Guid(guidFSharpLint_VisualStudioExtensionCmdSetString);
    }
}