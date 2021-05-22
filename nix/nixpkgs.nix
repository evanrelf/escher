args:

let
  # master as of 2021-05-22
  rev = "c912d30f6505fa3ceb0a1a1cd28f66086a386101";

  sha256 = "1rjjb9lzxm9kg8lyymw8dffq508d7q667vbdag97h2xw87iy930f";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
