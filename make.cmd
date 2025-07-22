@ECHO OFF
where /q dotnet
IF ERRORLEVEL 1 (
    ECHO "ERROR: 'dotnet' not found. Please ensure you have installed .NET (the version specified in global.json)" && EXIT /b 1
) ELSE (
    IF "%~1" == "" (
        dotnet fsi build.fsx --target Build
    ) ELSE (
        IF "%~1" == "check" (
            dotnet fsi build.fsx --target Test
        ) ELSE (
            IF "%~1" == "selfcheck" (
                dotnet fsi build.fsx --target SelfCheck
            ) ELSE (
                IF "%~1" == "docs" (
                    dotnet fsi build.fsx --target Docs
                ) ELSE (
                    ECHO "Target was not recognized" && EXIT /b 1
                )
            )
        )
    )
)
