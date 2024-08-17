"use strict";

let loader = require("../../../wasm-gnugo/javascript/gnugo.js");

export const getP = (left, right, wasmURL) => () => (
  loader.get(wasmURL).then(right).catch(e => left(e.name + ': ' + e.message ))
);

// Use setTimeout without delay to evaluate at the next event cycle
const fakeAsync = f => new Promise(resolve => setTimeout(() => f(resolve)))

export const playU = (module, seed, gameStr) => () =>
  fakeAsync(resolve => resolve(module.ccall("play", "string", ["number", "string"], [seed, gameStr])));


export const scoreU = (module, seed, gameStr) => () =>
  fakeAsync(resolve => resolve(module.ccall("score", "number", ["number", "string"], [seed, gameStr])))
