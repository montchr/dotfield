{ config, lib, pkgs, inputs, ... }:

let
  inherit (config.dotfield) secretsDir;

  sshHostKeyPaths = config.my.keys.ssh.hostKeyPaths;
  secretsFile = "${secretsDir}/secrets.nix";
in

{
  my.env.AGENIX_ROOT = config.dotfield.path;

  age.identityPaths = sshHostKeyPaths;

  age.secrets =
    if lib.pathExists secretsFile
    then
      lib.mapAttrs'
        (n: _: lib.nameValuePair (lib.removeSuffix ".age" n) {
          file = "${secretsDir}/${n}";
          owner = lib.mkDefault config.my.user.name;
        })
        (import secretsFile)
    else {
#      "wireless.env".file = "${secretsDir}/wireless.env.age";
    };
}

# via:
# https://github.com/sei40kr/dotfiles/blob/94ebb6211545949e6967a2834426eee65b7546a0/nixos-modules/agenix.nix
