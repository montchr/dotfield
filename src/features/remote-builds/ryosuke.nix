flake@{ inputs, self, ... }:
let
  inherit (inputs) dmerge;
in
{
  aspects.remote-builds__ryosuke.nixos =
    { config, ... }:
    let
      remoteMeta = flake.config.meta.hosts.ryosuke;
    in
    dmerge.merge
      {
        imports = [ ./__common.nix ];
      }
      (
        self.lib.builders.mkBuildMachineModuleFor config.networking.hostName "ryosuke" {
          extraHostNames = [
            "ryosuke.home.arpa"
            remoteMeta.ipv4.address
            remoteMeta.networks.ts.ipv4.address
          ];
        }
      );
}
