{
  self,
  inputs,
  ...
}:
let
  inherit (inputs) nixos-hardware;
  hostName = "ryosuke";
in
{
  dotfield.hosts.nixos.${hostName} = {
    modules = (
      with self.dotfield.modules;
      [
        gnome
        jobwork
        workstation
        "hardware/razer"
        # remote-builders-default
      ]
    );
    imports = (
      with nixos-hardware.nixosModules;
      [
        ./configuration.nix

        common-cpu-amd
        common-cpu-amd-pstate
        common-gpu-amd
      ]
    );
  };

  dotfield.metadata.hosts.${hostName} = {
    age = builtins.readFile ./keys/ryosuke.age.pub;
    ipv4.address = "192.168.1.217";
    hardware = {
      mem = 32;
      vcpus = 24;
      system = "x86_64-linux";
    };
    keys = [
      (builtins.readFile ./keys/ryosuke.pub)
      (builtins.readFile ./keys/ryosuke-rsa.pub)
    ];
    network = "home";
    networks.ts.ipv4.address = "100.123.41.68";
    users.cdom = {
      age = builtins.readFile ../../users/cdom/keys/cdom-at-ryosuke.age.pub;
      keys = [ (builtins.readFile ../../users/cdom/keys/cdom-at-ryosuke.pub) ];
    };
    syncthing.id = "2HDN7UF-5YKEBC7-4YB4L4H-A6Y7EGS-YZ5CSQX-AWWDKR7-KH5WIKH-D6LOTQ4";
  };

}
