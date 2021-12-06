{ config, lib, pkgs, inputs, ... }:

let
  inherit (config.dotfield) secretsDir;
  inherit (config.my.keys) sshHostKeyPaths;

  secretsFile = "${secretsDir}/secrets.nix";
in

{
  my.env.AGENIX_ROOT = config.dotfield.path;

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
