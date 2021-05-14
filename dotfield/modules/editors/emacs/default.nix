{ config, lib, pkgs, emacs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.emacs;
  configDir = config.dotfiles.configDir;
in
{
  options = with lib; {
    my.modules.editors.emacs = {
      enable = mkEnableOption false;
      doom.enable = mkEnableOption true;
    };
  };

  config = mkIf cfg.enable {
    my.user.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this
      # emacsPgtkGcc # 28 + pgtk + native-comp

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

    # my.modules.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # system.activationScripts.postUserActivation.text = mkIf cfg.doom.enable ''
    #   if [[ ! -d $HOME/.config/emacs ]]; then
    #     git clone https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #   fi
    # '';
  };
}
