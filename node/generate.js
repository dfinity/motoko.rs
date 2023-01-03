"use strict";

const { join } = require("path");
const { readFileSync, writeFileSync } = require("fs");
const mo = require("motoko");

const packages = [
  { name: "base", path: "dfinity/motoko-base/master/src" },
  { name: "base-test", path: "dfinity/motoko-base/master/test" },
  { name: "matchers", path: "kritzcreek/motoko-matchers/master/src" },
];

const primFile = readFileSync(
  join(__dirname, "../submodules/motoko/src/prelude/prim.mo"),
  "utf8"
);
const [, primComment, primSource] =
  /^\s*(\/\*[\s\S]*?\*\/)\s*([\s\S]*)$/gm.exec(primFile);

writeFileSync(
  join(__dirname, "../crates/motoko/src/packages/prim.mo"),
  `${primComment}\n\nmodule {\n${primSource
    .replaceAll(/^func/gm, "public func")
    .replaceAll(/^let/gm, "public let")
    .replaceAll(/^/gm, "  ")}\n}`,
  "utf8"
);

Promise.all(
  packages.map(async ({ name, path }) => {
    console.log(name, ":", path);
    const pkg = await mo.fetchPackage(name, path);
    writeFileSync(
      join(__dirname, `../crates/motoko/src/packages/${name}.json`),
      JSON.stringify(pkg),
      "utf8"
    );
  })
);
