#!/bin/bash

yarn && \
yarn bomlint && \
yarn --cwd ts-facade build && \
yarn --cwd ts-facade-tests test