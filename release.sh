#!/bin/bash

sbt ci-release && \
cd facade/ts-facade && \
yarn publish --access public