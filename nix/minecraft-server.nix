{ bash
, fetchurl
, openjdk
, writeShellScriptBin
  # Arguments
, version
, objectId
, sha256
}:

let
  jar = fetchurl {
    url = "https://launcher.mojang.com/v1/objects/${objectId}/server.jar";
    inherit sha256;
  };

in
(writeShellScriptBin "minecraft-server" ''
  exec "${openjdk}/bin/java" -Xms1024M -Xmx1024M -jar "${jar}" --nogui "$@"
'').overrideAttrs (old: rec {
  name = "${pname}-${version}";
  pname = "minecraft-server";
  inherit version;
})
