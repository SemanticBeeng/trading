#!/bin/bash

SYMBOL=$1

echo Purging tick data files ...

pushd /datascience/marketdata/storage/tick/$SYMBOL
# Source: http://askubuntu.com/questions/35922/how-to-change-extension-of-multiple-files-from-command-line
rm *.RData
rm *.rda

popd

