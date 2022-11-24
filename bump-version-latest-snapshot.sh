#!/bin/bash
set -e

echo "Bumping sbt modules to LATEST-SNAPSHOT"
cat >version.sbt <<EOF
ThisBuild / version := "LATEST-SNAPSHOT"
EOF

git status
