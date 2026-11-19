#!/bin/bash

set -e

# make sure facades / JS has same version as scala jars
export VERSION=$(sbt --batch --server -Dsbt.supershell=false -error --no-colors "print diesel/version" | tail -1)
# use facace/bump-version.js, and commit to the release tag 
( cd facade; ./check-version.js )

sbt ci-release
sbt samplesBundle/fastOptJS

cd facade
echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc
yarn
cd samples-bundle
echo "dist contents:"
ls dist
yarn publish --access public --verbose
cd  ../ts-facade
yarn build
yarn publish --access public --verbose
