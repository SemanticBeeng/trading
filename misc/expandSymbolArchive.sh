#!/bin/bash

SYMBOL=$1

echo Expanding archive for symbol $SYMBOL ...

pushd /datascience/marketdata
tar -xf archives/$SYMBOL.tar -C storage --strip-components=3

echo Renaming data files ...

pushd /datascience/marketdata/storage/$SYMBOL
# Source: http://askubuntu.com/questions/35922/how-to-change-extension-of-multiple-files-from-command-line
for file in *.RData; do
  mv "$file" "${file%.RData}.rda"
done

popd
popd

