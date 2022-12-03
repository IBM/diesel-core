#!/bin/bash

sbt ci-release && \
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > ~/.npmrc && \
cd facade/ts-facade && \
yarn publish --access public