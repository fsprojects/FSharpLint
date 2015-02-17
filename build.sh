#!/bin/bash
if [ ! -f packages/FAKE/tools/FAKE.exe ]; then
  mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi
if [ ! -f tools\NUnit.Runners\tools\nunit-console.exe ]; then
  mono .nuget/NuGet.exe install NUnit.Runners -OutputDirectory tools -ExcludeVersion
fi
#workaround assembly resolution issues in build.fsx
export FSHARPI=`which fsharpi`
cat - > fsharpi <<"EOF"
#!/bin/bash
libdir=$PWD/packages/FAKE/tools/
$FSHARPI --lib:$libdir $@
EOF
chmod +x fsharpi
mono packages/FAKE/tools/FAKE.exe build.fsx $@
EXITCODE=$?
rm fsharpi
exit $EXITCODE
