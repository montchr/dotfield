{ ops, ... }:
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

  nix.buildMachines = [
    {
      hostName = "platauc";
      system = "aarch64-linux";
      speedFactor = 2;
      protocol = "ssh-ng";
      maxJobs = 3;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
    }
  ];
}
