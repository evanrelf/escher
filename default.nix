let
  pkgs = import ./nix/nixpkgs.nix { };

in
rec {
  escher = pkgs.haskellPackages.callCabal2nix "escher" ./. { };

  minecraft-server = minecraft-server_1_17_1;

  minecraft-server_1_16_5 = pkgs.callPackage ./nix/minecraft-server.nix {
    version = "1.16.5";
    objectId = "1b557e7b033b583cd9f66746b7a9ab1ec1673ced";
    sha256 = "19ix6x5ij4jcwqam1dscnqwm0m251gysc2j793wjcrb9sb3jkwsq";
  };

  minecraft-server_1_17 = pkgs.callPackage ./nix/minecraft-server.nix {
    version = "1.17";
    objectId = "0a269b5f2c5b93b1712d0f5dc43b6182b9ab254e";
    sha256 = "0jqz7hpx7zvjj2n5rfrh8jmdj6ziqyp8c9nq4sr4jmkbky6hsfbv";
  };

  minecraft-server_1_17_1 = pkgs.callPackage ./nix/minecraft-server.nix {
    version = "1.17.1";
    objectId = "a16d67e5807f57fc4e550299cf20226194497dc2";
    sha256 = "0pzmzagvrrapjsnd8xg4lqwynwnb5rcqk2n9h2kzba8p2fs13hp8";
  };
}
