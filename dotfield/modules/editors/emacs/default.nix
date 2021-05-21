{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.emacs;
  configDir = config.dotfield.configDir;
  # emacs-server    = "${tmpdir}/emacs-emacs/server";
  # emacsclient     = "${pkgs.emacs}/bin/emacsclient -s ${emacs-server}";
in
{
  options = with lib; {
    my.modules.editors.emacs = {
      enable = mkEnableOption false;
      doom.enable = mkEnableOption true;
    };
  };

  config = mkIf cfg.enable {
    services.emacs = with pkgs; {
      enable = true;
      package = emacs;
    };

    environment.systemPackages = with pkgs; [ emacs ];

    my = {

      user.packages = with pkgs; [
        emacs

        ## Doom dependencies
        (ripgrep.override { withPCRE2 = true; })
        gnutls # for TLS connectivity

        ## Optional dependencies
        fd # faster projectile indexing
        imagemagick # for image-dired
        (
          mkIf (config.programs.gnupg.agent.enable)
            pinentry_emacs
        ) # in-emacs gnupg prompts
        zstd # for undo-fu-session/undo-tree compression

        ## Module dependencies
        # :checkers spell
        # (aspellWithDicts (ds: with ds; [
        #   en en-computers en-science
        # ]))
        # :checkers grammar
        languagetool
        # :tools editorconfig
        editorconfig-core-c
        # :tools lookup & :lang org +roam
        sqlite
        # :lang javascript
        nodePackages.javascript-typescript-langserver
      ];

      # modules.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

      env = {
        DOOMDIR = "$XDG_CONFIG_HOME/doom";
        EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      };

    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    system.activationScripts.postUserActivation.text =
      with config.my;
      mkIf cfg.doom.enable ''
        # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
        if [[ ! -d "${xdg.config}/emacs" ]]; then
          git clone https://github.com/hlissner/doom-emacs "${xdg.config}/emacs"
        fi
      '';
  };
}
