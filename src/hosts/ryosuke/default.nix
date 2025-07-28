{
  self,
  inputs,
  moduleWithSystem,
  withSystem,
  ...
}:
let
  inherit (inputs) nixos-hardware;
  hostName = "ryosuke";
in
{
  flake.nixosConfigurations.${hostName} = withSystem "x86_64-linux" (
    {
      config,
      system,
      ...
    }:
    inputs.nixos-unstable.lib.nixosSystem {
      inherit system;
      modules = [
        self.dotfield.hosts.nixos.${hostName}
        self.dotfield.nixos.defaults

        {
          networking = { inherit hostName; };
          nixpkgs.config.allowUnfree = true;
          nixpkgs.overlays = (import "${self.outPath}/overlays/default.nix" { inherit inputs; });
        }
      ];
    }
  );

  dotfield.hosts.nixos."hosts/${hostName}".imports =
    (with nixos-hardware.nixosModules; [
      common-cpu-amd
      common-cpu-amd-pstate
      common-gpu-amd
    ])
    ++ (with self.dotfield.nixos; [
      gnome
      jobwork
      workstation
      "hardware/razer"
      remote-builders-default
    ]);

}
