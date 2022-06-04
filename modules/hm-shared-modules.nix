{config, inputs, pkgs, ...}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  homeManagerPackage = inputs.home-manager.packages.${system}.default;
in
{
  environment.systemPackages = [homeManagerPackage];
  home-manager.sharedModules = [
    {
      programs.home-manager.enable = true;

      # Necessary for home-manager to work with flakes, otherwise it will
      # look for a nixpkgs channel.
      home.stateVersion = "21.11";

      xdg.enable = true;

      # TODO: what benefit does symlinking this provide?
      # xdg.configFile."nix/registry.json".text =
      #   config.environment.etc."nix/registry.json".text;
    }
  ];
}
