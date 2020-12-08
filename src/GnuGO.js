"use strict";

let loader = require("../../../wasm-gnugo/javascript/gnugo.js");

exports.withWasmU = (wasmURL, callback) => () => {
  loader.withWasm(wasmURL, callback)
};

exports.playU = function (module, seed, gameStr) {
  return module.ccall("play", "string", ["number", "string"], [seed, gameStr]);
};

exports.scoreU = function (module, seed, gameStr) {
  return module.ccall("score", "number", ["number", "string"], [seed, gameStr]);
};
