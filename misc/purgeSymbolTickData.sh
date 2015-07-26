#!/bin/bash

SYMBOL=$1

echo Purging tick data files ...

pushd /datascience/marketdata/storage/$SYMBOL
# Source: http://askubuntu.com/questions/35922/how-to-change-extension-of-multiple-files-from-command-line
for file in *.RData; do
  rm "$file" 
done

for file in *.rda; do
  rm "$file" 
done

popd

