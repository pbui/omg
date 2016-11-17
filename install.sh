#!/bin/sh

SITELIBDIR=`gauche-config --sitelibdir`

echo "Clearing $SITELIBDIR/omg..."
rm -fr $SITELIBDIR/omg
echo "Creating $SITELIBDIR/omg..."
mkdir -p $SITELIBDIR/omg

for f in `find . -path './_darcs*' -prune -o -iname '*.scm' -print | sort`; do
    echo "Installing $f..."
    mkdir -p $SITELIBDIR/omg/`dirname $f`
    cp -f $f $SITELIBDIR/omg/`dirname $f`
done

echo "Done"
