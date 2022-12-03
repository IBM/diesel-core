#!/bin/bash

sbt ci-release && \
cd facade && \
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc && \
yarn && \
cd  ts-facade && \
yarn build && \
yarn publish --access public --verbose