let
  pkgs = import ./nix/nixpkgs.nix { };

  escher = import ./default.nix;

in
escher.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or [ ]) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
