{
  config,
  self,
  inputs,
  ...
}:
let
  inherit (config.meta) keys;

  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.ryosuke = {
    system = "x86_64-linux";
    configuration = {
      imports = [
        ./configuration.nix

        inputs.nixos-hardware.nixosModules.common-cpu-amd
        inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
        inputs.nixos-hardware.nixosModules.common-gpu-amd

        (nixos + "/mixins/gnome.nix")
        (nixos + "/mixins/jobwork.nix")
        (nixos + "/mixins/workstation.nix")

        (nixos + "/profiles/hardware/razer.nix")
        (nixos + "/profiles/remote-builders/default.nix")
      ];
    };
  };

  meta.hosts.ryosuke = {
    ipv4.address = "192.168.1.217";
    hardware = {
      mem = 32;
      vcpus = 24;
      system = "x86_64-linux";
    };
    keys.age = keys."ryosuke.age";
    keys.ssh = [
      keys.ssh.ryosuke
      keys.ssh.ryosuke-rsa
    ];
    network = "home";
    networks.ts.ipv4.address = "100.123.41.68";
    users.cdom.keys = {
      age = keys.age.cdom-at-ryosuke;
      ssh = [ keys.ssh.cdom-at-ryosuke ];
    };
    syncthing.id = "2HDN7UF-5YKEBC7-4YB4L4H-A6Y7EGS-YZ5CSQX-AWWDKR7-KH5WIKH-D6LOTQ4";
  };
}
