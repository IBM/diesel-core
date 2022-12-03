#!/bin/bash

sbt ci-release && \
cd facade/ts-facade && \
npm publish