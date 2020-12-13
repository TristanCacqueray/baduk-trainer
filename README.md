# baduk-trainer

A web application to learn the game of [baduk][baduk] using [gnugo][gnugo].

## Usage

Go to the [website](https://tristancacqueray.github.io/baduk-trainer/) and play the training games.

## Features

- Trainer
  - [x] game list
  - [x] save state in local storage
  - [x] display progressive game list
  - [ ] generate new games based on rank
- Editor
  - [x] stones
  - [x] name, size, komi
  - [ ] rank estimator
- Player
  - [x] basic captures
  - [x] ko
  - [ ] property testing

## Contribute

### Requirements

- purescript (spago and purs)
- parcel
- [wasm-gnugo](https://github.com/TristanCacqueray/wasm-gnugo/) built in `../wasm-gnugo`

### Build

Build the javascript by running:

```
spago build && spago test
```

Run the application locally by running:

```
parcel serve src/index.html
ln -s ../../wasm-gnugo/ dist/wasm-gnugo
```

### Distribute

```
spago build && rm -Rf dist/ && parcel build --public-url /baduk-trainer/ src/index.html
git checkout pages && git rm src.* && rsync -a dist/ $(pwd)/ && git add src.* && git commit -a --amend -m "Add build" && git push -f origin pages
```

[baduk]: https://en.wikipedia.org/wiki/Go_(game)
[gnugo]: https://www.gnu.org/software/gnugo/
