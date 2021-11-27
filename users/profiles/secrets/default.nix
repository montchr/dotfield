{ config, lib, pkgs, inputs, ... }:

let
  inherit (config.my.keys) sshHostKeyPaths;

  secretsDir = ../../../secrets;
  secretsFile = "${secretsDir}/secrets.nix";
in

{
  age.sshKeyPaths = sshHostKeyPaths;

  age.secrets =
    if lib.pathExists secretsFile
    then
      lib.mapAttrs'
        (n: _: lib.nameValuePair (lib.removeSuffix ".age" n) {
          file = "${secretsDir}/${n}";
          owner = lib.mkDefault config.my.user.name;
        })
        (import secretsFile)
    else { };
}
