@ECHO OFF
where /q dotnet
IF ERRORLEVEL 1 (
    ECHO "Please install .NETv6 or higher" && EXIT /b 1
) ELSE (
    IF "%~1" == "" (
        dotnet fake build --target Build
    ) ELSE (
        IF "%~1" == "check" (
            dotnet fake build --target Test
        ) ELSE (
            ECHO "Target was not recognized" && EXIT /b 1
        )
    )
)

