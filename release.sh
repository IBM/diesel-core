#!/bin/bash

# sbt ci-release && \
cd facade && \
yarn && \
cd  ts-facade && \
yarn build && \
yarn publish --access public