{ ops, ... }:
let
  inherit (ops.hosts) platauc;
in
{
  programs.ssh = {
    extraConfig = ''
      Host platauc
        IdentitiesOnly yes
        IdentityFile /root/.ssh/id_ed25519_remote_builds
        User nix-remote-builder
    '';
    knownHosts."platauc" = {
      hostNames = [
        "platauc"
        platauc.ipv4.address
        # platauc.networks.ts.ipv4.address
      ];
      publicKey = builtins.head platauc.keys;
    };
  };

  nix.buildMachines = [
    {
      hostName = "platauc";
      system = "aarch64-linux";
      speedFactor = 3;
      protocol = "ssh-ng";
      maxJobs = 3;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
      ];
    }
  ];
}
