moduleArgs @ {
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}: let
  inherit (config.lib.fish) mkPlugin;
  l = lib // builtins;

  hasHomebrew = moduleArgs.darwinConfig.homebrew.enable or false;
  brewPrefix = moduleArgs.darwinConfig.homebrew.brewPrefix or "/opt/homebrew";

  shellAbbrs = import ../abbrs.nix;
  shellAliases = import ../aliases.nix;
in {
  imports = [../common.nix];

  home.packages = with pkgs; [
    fishPlugins.done
    fishPlugins.forgit
  ];

  programs.fish = {
    inherit
      # FIXME: watch out, some of these abbrs may have undesirable results when
      # expanded inline. needs review.
      shellAbbrs
      shellAliases
      ;

    enable = true;
    autopair.enable = true;
    fifc.enable = true;

    plugins = l.map mkPlugin [
      "replay"
    ];

    # Workaround for clobbered `$PATH` with nix-darwin
    # https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1030877541
    loginShellInit = l.mkIf isDarwin ''
      fish_add_path --move --prepend --path \
        $HOME/.nix-profile/bin \
        /run/wrappers/bin \
        /etc/profiles/per-user/$USER/bin \
        /nix/var/nix/profiles/default/bin \
        /run/current-system/sw/bin \
        /opt/homebrew/bin \
        /opt/homebrew/sbin
    '';

    interactiveShellInit = ''
      # "Required" by `fifc`
      # set -Ux fifc_editor $EDITOR
    '';
  };
}
