#!/bin/bash

# sbt ci-release && \
cd facade/ts-facade && \
yarn build && \
yarn publish --access public