# xdg.nix
#
# Set up and enforce XDG compliance. Other modules will take care of their own,
# but this takes care of the general cases.
#
# See:
#   - https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/modules/xdg.nix


{ config, home-manager, lib, ... }:
let

  xdg = config.my.xdg;

in
{
  # TODO: handled in settings.nix -- move those to this file?
  # home-manager.users.${config.user.name}.xdg.enable = true;

  environment = {

    sessionVariables = {
      # These are the defaults, and xdg.enable does set them, but due to load
      # order, they're not set before environment.variables are set, which could
      # cause race conditions.
      XDG_CACHE_HOME  = "${xdg.cache}";
      XDG_CONFIG_HOME = "${xdg.config}";
      XDG_DATA_HOME   = "${xdg.data}";
      XDG_BIN_HOME    = "${xdg.bin}";
    };

    variables = {
      # Conform more programs to XDG conventions. The rest are handled by their
      # respective modules.
      #
      # TODO: move a lot of the stuff in ~/.config/shell/profile into here
      #
      # TODO: setup aspell
      # ASPELL_CONF = ''
      #   per-conf $XDG_CONFIG_HOME/aspell/aspell.conf;
      #   personal $XDG_CONFIG_HOME/aspell/en_US.pws;
      #   repl $XDG_CONFIG_HOME/aspell/en.prepl;
      # '';
      HISTFILE        = "$XDG_DATA_HOME/bash/history";
      INPUTRC         = "$XDG_CONFIG_HOME/readline/inputrc";
      LESSHISTFILE    = "$XDG_CACHE_HOME/lesshst";
      WGETRC          = "$XDG_CONFIG_HOME/wgetrc";
    };

    # Move ~/.Xauthority out of $HOME (setting XAUTHORITY early isn't enough)
    # extraInit = ''
    #   export XAUTHORITY=/tmp/Xauthority
    #   [ -e ~/.Xauthority ] && mv -f ~/.Xauthority "$XAUTHORITY"
    # '';
  };
}
