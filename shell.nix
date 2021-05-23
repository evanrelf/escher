let
  pkgs = import ./nix/nixpkgs.nix { };

  inherit (import ./default.nix)
    escher
    minecraft-server
    ;

in
escher.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or [ ]) ++ [
    minecraft-server
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
