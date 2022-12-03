#!/bin/bash

#sbt ci-release && \
sbt samplesBundle/fastOptJS && \
cd facade && \
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc && \
yarn && \
cd samples-bundle && \
echo "dist contents:" && \
ls dist && \
yarn publish --access public --verbose && \
cd  ../ts-facade && \
yarn build && \
yarn publish --access public --verbose