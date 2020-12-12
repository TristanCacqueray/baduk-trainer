"use strict";

let loader = require("../../../wasm-gnugo/javascript/gnugo.js");

exports.getP = (wasmURL) => () => (
  loader.get(wasmURL).catch(e => console.log("oops", e))
);

exports.playU = function (module, seed, gameStr) {
  return module.ccall("play", "string", ["number", "string"], [seed, gameStr]);
};

exports.scoreU = function (module, seed, gameStr) {
  return module.ccall("score", "number", ["number", "string"], [seed, gameStr]);
};
