# xdg.nix
#
# Set up and enforce XDG compliance. Other modules will take care of their own,
# but this takes care of the general cases.
#
# See:
#   - https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/modules/xdg.nix

{ config, home-manager, lib, ... }: {
  environment = {

    # Environment variables to be loaded for all users.
    variables = {

      # TODO: these may need be set in `environment.sessionVariables` for NixOS
      # or in `home.sessionVariables` for other Linux systems.
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";

      # Conform more programs to XDG conventions. The rest are handled by their
      # respective modules.
      #
      # TODO: move a lot of the stuff in ~/.config/shell/profile into here or
      # specific modules.
      #
      # TODO: setup aspell ASPELL_CONF = '' per-conf
      # $XDG_CONFIG_HOME/aspell/aspell.conf; personal
      # $XDG_CONFIG_HOME/aspell/en_US.pws; repl
      # $XDG_CONFIG_HOME/aspell/en.prepl;
      # '';
      HISTFILE = "$XDG_DATA_HOME/bash/history";
      INPUTRC = "$XDG_CONFIG_HOME/readline/inputrc";
      LESSHISTFILE = "$XDG_CACHE_HOME/lesshst";
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
    };
  };
}
