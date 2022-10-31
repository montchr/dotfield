{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = lib // builtins;

  cfg = config.programs.fish;

  mkPlugin = name: {
    inherit name;
    inherit (pkgs.sources."fish-${name}") src;
  };
in {
  options = {
    programs.fish.autopair.enable = l.mkEnableOption "Whether to enable autopairing of symbols with the autopair plugin.";
    programs.fish.fifc.enable = l.mkEnableOption "Whether to enable the fifc fish plugin.";
  };

  config = l.mkIf cfg.enable {
    lib.fish = {inherit mkPlugin;};
    programs.fish.plugins =
      l.map mkPlugin
      ((l.optional cfg.autopair.enable "autopair")
        ++ (l.optional cfg.fifc.enable "fifc")
        ++ (l.optional isDarwin "nix-env"));
  };
}
