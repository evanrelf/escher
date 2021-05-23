let
  pkgs = import ./nix/nixpkgs.nix { };

in
{
  escher = pkgs.haskellPackages.callCabal2nix "escher" ./. { };

  minecraft-server = pkgs.callPackage ./nix/minecraft-server.nix { };
}
