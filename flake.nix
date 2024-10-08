{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
      in {
        devShells.default = pkgs.mkShell {
          # You now have access to the standard PureScript toolchain in pkgs
          buildInputs = [
            # pkgs.gnumake
            pkgs.purs
            # pkgs.nodePackages_latest.purty
            pkgs.spago-unstable
            # pkgs.purs-tidy-bin
            pkgs.purs-backend-es
            pkgs.esbuild
          ];
        };
      });
}
