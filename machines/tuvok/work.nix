{ pkgs, flake, ... }:
let
  inherit (flake.inputs) microvm;
in
{
  imports = [ microvm.nixosModules.host ];

  home-manager.users.cdom.home.packages = [ pkgs.phpactor ];

  # microvm.shares = [
  #   {
  #     tag = "ro-store";
  #     source = "/nix/store";
  #     mountPoint = "/nix/.ro-store";
  #   }
  # ];
}
