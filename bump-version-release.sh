#!/bin/bash
set -e

VERSION=$1

if [ -z $VERSION ]
then
  echo "version not specified"
  exit 1
fi

echo "Bumping sbt modules to '$VERSION'"
cat >version.sbt <<EOF
ThisBuild / version := "$VERSION"
EOF

git status
