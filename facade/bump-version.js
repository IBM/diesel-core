#!/usr/bin/env node

const fs = require('fs');

const VERSION = process.env.VERSION;

console.log("Bumping to " + VERSION);

function withJsonFile(name, callback) {
    const text = fs.readFileSync(name);
    const json = JSON.parse(text);
    callback(json)
    fs.writeFileSync(name, JSON.stringify(json, null, "  "))
}

withJsonFile("./samples-bundle/package.json", j => {
    j.version = VERSION;
});

withJsonFile("./ts-facade/package.json", j => {
    j.version = VERSION;
});

withJsonFile("./ts-facade-tests/package.json", j => {
    j.dependencies['@diesel-parser/samples'] = '^' + VERSION;
    j.devDependencies['@diesel-parser/ts-facade'] = '^' + VERSION;
});


