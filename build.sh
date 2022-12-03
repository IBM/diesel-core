#!/bin/bash

sbt build lint test && \
npm i --prefix facade && \
npm run build --prefix facade/ts-facade && \
npm run test --prefix facade/ts-facade-tests