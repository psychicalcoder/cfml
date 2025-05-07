#!/bin/bash

export CFMLC_FLAGS="-deep_embedding -def_encoders -credits"
export FOLDER=`pwd`
cd ../..
make -f Makefile $@
