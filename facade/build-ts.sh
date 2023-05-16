#!/bin/bash

yarn && \
yarn --cwd ts-facade build && \
yarn --cwd ts-facade-tests test