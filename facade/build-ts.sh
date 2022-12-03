#!/bin/bash

npm install && \
npm run build --prefix ts-facade && \
npm run test --prefix ts-facade-tests