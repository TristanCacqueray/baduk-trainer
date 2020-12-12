"use strict";

let loader = require("../../../wasm-gnugo/javascript/gnugo.js");

exports.getP = (left, right, wasmURL) => () => (
  loader.get(wasmURL).then(right).catch(e => left(e.name + ': ' + e.message ))
);

exports.playU = function (module, seed, gameStr) {
  return module.ccall("play", "string", ["number", "string"], [seed, gameStr]);
};

exports.scoreU = function (module, seed, gameStr) {
  return module.ccall("score", "number", ["number", "string"], [seed, gameStr]);
};
