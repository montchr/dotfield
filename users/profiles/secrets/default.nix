{ config, lib, pkgs, inputs, ... }:

let
  inherit (config.dotfield) secretsDir;

  sshHostKeyPaths = config.my.keys.ssh.hostKeyPaths;
  secretsManifestFile = "${secretsDir}/secrets.nix";

  secretFiles = [
    "aws-cdom-default.pem"
  ];

  mkSecret = name: {
    "${name}" = {
      file = "${secretsDir}/${name}.age";
      owner = lib.mkDefault config.my.user.name;
    };
  };
in

{
  my.env.AGENIX_ROOT = config.dotfield.path;

  age.identityPaths = sshHostKeyPaths;

  age.secrets =
    if lib.pathExists secretsManifestFile
    then
      lib.mapAttrs'
        (n: _: lib.nameValuePair (lib.removeSuffix ".age" n) {
          file = "${secretsDir}/${n}";
          owner = lib.mkDefault config.my.user.name;
        })
        (import secretsManifestFile)
    else
      lib.mkMerge (builtins.map mkSecret secretFiles);
}

# via:
# https://github.com/sei40kr/dotfiles/blob/94ebb6211545949e6967a2834426eee65b7546a0/nixos-modules/agenix.nix
