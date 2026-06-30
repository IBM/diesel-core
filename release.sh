#!/bin/bash

set -ex

# make sure facades / JS has same version as scala jars
export VERSION=$(sbt -Dsbt.supershell=false -error "print diesel/version")
# use facace/bump-version.js, and commit to the release tag 
( cd facade; ./check-version.js )

sbt ci-release
sbt samplesBundle/fastOptJS

echo "//registry.npmjs.org/:_authToken=$NPM_TOKEN" > .npmrc
yarn
cd samples-bundle
echo "dist contents:"
ls dist
yarn publish --access public --verbose
cd  ../ts-facade
yarn build
yarn publish --access public --verbose