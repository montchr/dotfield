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

      # TODO: what benefit does symlinking this provide?
      # xdg.configFile."nix/registry.json".text =
      #   config.environment.etc."nix/registry.json".text;
      home.stateVersion = lib.mkDefault "22.05";
    }
  ];
}
