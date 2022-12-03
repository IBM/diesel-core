#!/bin/bash

sbt build lint test && cd facade && ./build-ts.sh 