#!/usr/bin/env node

const fs = require("fs");

const VERSION = process.env.VERSION;

console.log("Checking versions : " + VERSION);

function withJsonFile(name, callback) {
  const text = fs.readFileSync(name);
  const json = JSON.parse(text);
  callback(json);
}

withJsonFile("./samples-bundle/package.json", (j) => {
  if (j.version !== VERSION) {
    throw "Samples version invalid : " + j.version;
  }
});

withJsonFile("./ts-facade/package.json", (j) => {
  if (j.version !== VERSION) {
    throw "Facade version invalid : " + j.version;
  }
});

withJsonFile("./ts-facade-tests/package.json", (j) => {
  if (j.dependencies["@diesel-parser/samples"] !== "^" + VERSION) {
    throw (
      "tests samples dep invalid : " + j.dependencies["@diesel-parser/samples"]
    );
  }
  if (j.devDependencies["@diesel-parser/ts-facade"] !== "^" + VERSION) {
    throw (
      "tests facade dep invalid : " +
      j.devDependencies["@diesel-parser/ts-facade"]
    );
  }
});

console.log("npm versions checked ok");
