{ config, ops, ... }:
let
  inherit (ops.hosts) platauc;

in
{
  programs.ssh = {
    extraConfig = ''
      Host platauc
        Hostname ${platauc.ipv4.address}
        IdentitiesOnly yes
        IdentityFile /root/.ssh/id_ed25519_remote_builds
        User nix-remote-builder
    '';
    knownHosts."platauc" = {
      hostNames = [
        "platauc"
        platauc.ipv4.address
        platauc.ipv6.address
      ];
      publicKey = builtins.head platauc.keys;
    };
  };

  nix.buildMachines = [ (config.lib.dotfield.mkBuildMachine "platauc") ];
}
