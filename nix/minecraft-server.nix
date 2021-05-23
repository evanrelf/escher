{ bash, fetchurl, openjdk, writeShellScriptBin }:

let
  jar = fetchurl {
    url = "https://launcher.mojang.com/v1/objects/1b557e7b033b583cd9f66746b7a9ab1ec1673ced/server.jar";
    sha256 = "19ix6x5ij4jcwqam1dscnqwm0m251gysc2j793wjcrb9sb3jkwsq";
  };
in
(writeShellScriptBin "minecraft-server" ''
  exec "${openjdk}/bin/java" -Xms1024M -Xmx1024M -jar "${jar}" --nogui "$@"
'').overrideAttrs (old: rec {
  name = "${pname}-${version}";
  pname = "minecraft-server";
  version = "1.16.5";
})
