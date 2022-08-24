{
  config,
  lib,
  inputs,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system;
  homeManagerPackage = inputs.home-manager.packages.${system}.default;
in {
  environment.systemPackages = [homeManagerPackage];
  home-manager.sharedModules = [
    {
      programs.home-manager.enable = true;
      manual.json.enable = true;
      news.display = "show";
      xdg.enable = true;
      home.stateVersion = lib.mkDefault "22.05";
    }
  ];
}
