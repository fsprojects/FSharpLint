#!/bin/bash

run .paket/paket.bootstrapper.exe

run .paket/paket.exe restore

#workaround assembly resolution issues in build.fsx
export FSHARPI=`which fsharpi`
cat - > fsharpi <<"EOF"
#!/bin/bash
libdir=$PWD/packages/tools/FAKE/tools/
$FSHARPI --lib:$libdir $@
EOF
chmod +x fsharpi
mono packages/tools/FAKE/tools/FAKE.exe build.fsx $@
EXITCODE=$?
rm fsharpi
exit $EXITCODE
