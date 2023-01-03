"use strict";

const { join } = require("path");
const { readFileSync, writeFileSync } = require("fs");
const mo = require("motoko");

const packages = [
  { name: "base", path: "dfinity/motoko-base/master/src" },
  { name: "base-test", path: "dfinity/motoko-base/master/test" },
  { name: "matchers", path: "kritzcreek/motoko-matchers/master/src" },
];

Promise.all(
  packages.map(async ({ name, path }) => {
    const pkg = await mo.fetchPackage(name, path);
    writeFileSync(
      join(__dirname, `../crates/motoko/src/packages/${name}.json`),
      JSON.stringify(pkg),
      "utf8"
    );
    writeFileSync(
      join(__dirname, "../crates/motoko/src/packages/prim.mo"),
      readFileSync(
        join(__dirname, "../submodules/motoko/src/prelude/prim.mo"),
        "utf8"
      ),
      "utf8"
    );
  })
);