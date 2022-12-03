#!/bin/bash

sbt ci-release && \
cd facade/ts-facade && \
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc && \
npm publish