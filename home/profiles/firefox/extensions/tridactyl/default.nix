{
  config,
  inputs,
  inputs',
  pkgs,
  packages,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.programs.firefox;
in {
  # TODO: seems potentially fragile -- make sure this works!
  programs.firefox.package = l.mkIf (!isDarwin) (cfg.package.override {enableTridactylNative = true;});
  programs.firefox.extensions = [inputs'.firefox-addons.packages.tridactyl];
  xdg.configFile."tridactyl/themes".source = ./themes;
  xdg.configFile."tridactyl/tridactylrc".source = ./tridactylrc;
}
