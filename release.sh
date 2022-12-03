#!/bin/bash

sbt ci-release && \
npm publish --prefix facade/ts-facade