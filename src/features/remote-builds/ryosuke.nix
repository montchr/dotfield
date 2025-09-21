flake@{ inputs, self, ... }:
let
  inherit (inputs) dmerge;
in
{
  aspects.remote-builds__ryosuke.nixos =
    { config, ... }:
    dmerge.merge
      {
        imports = [ ./__common.nix ];
      }
      (
        self.lib.builders.mkBuildMachineModuleFor config.networking.hostName "ryosuke" {
          extraHostNames = [
            "ryosuke.home.arpa"
          ];
        }
      );
}
