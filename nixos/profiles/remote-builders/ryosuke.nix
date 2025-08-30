{ flake, ... }:
let
  inherit (flake.config.meta.hosts) ryosuke;
in

{
  programs.ssh.knownHosts."ryosuke" = {
    hostNames = [
      "ryosuke"
      "ryosuke.home.arpa"
      ryosuke.ipv4.address
      ryosuke.networks.ts.ipv4.address
    ];
    publicKey = builtins.head ryosuke.keys.ssh;
  };

  nix.buildMachines = [
    {
      hostName = "ryosuke";
      system = "x86_64-linux";
      speedFactor = 3;
      protocol = "ssh-ng";
      maxJobs = 4;
      supportedFeatures = [
        "benchmark"
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
  ];
}
