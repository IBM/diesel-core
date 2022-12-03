#!/bin/bash

sbt build && \
sbt ci-release && \
cd facade && \
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc && \
yarn && \
cd samples-bundle && \
yarn publish --access public --verbose && \
cd  ../ts-facade && \
yarn build && \
yarn publish --access public --verbose


