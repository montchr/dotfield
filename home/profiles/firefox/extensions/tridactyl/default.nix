{
  config,
  inputs,
  sources,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
  cfg = config.programs.firefox;
in {
  # TODO: seems potentially fragile -- make sure this works!
  programs.firefox.package =
    l.mkIf (!isDarwin)
    (cfg.package.override {enableTridactylNative = true;});
  programs.firefox.extensions = [sources.firefox-addons.tridactyl];
  xdg.configFile."tridactyl/themes".source = ./themes;
  xdg.configFile."tridactyl/tridactylrc".source = ./tridactylrc;
}
