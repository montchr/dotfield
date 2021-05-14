{ config, lib, pkgs, inputs, ... }:

with lib;
# with lib.my;
let
  cfg = config.my.modules.editors.emacs;
  configDir = config.dotfiles.configDir;
in
{
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs.overlay ];

    programs.emacs.enable = true;

    user.packages = with pkgs; [
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
      editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      sqlite
      # :lang cc
      # ccls
      # :lang javascript
      nodePackages.javascript-typescript-langserver
    ];

    # Doom Emacs bin.
    # TODO: ensure this doesn't overwrite existing PATH!
    # my.env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    my.modules.zsh.rcFiles = [ "${configDir}/emacs/aliases.zsh" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # system.activationScripts.postUserActivation.text = mkIf cfg.doom.enable ''
    #   if [[ ! -d $HOME/.config/emacs ]]; then
    #     git clone https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #   fi
    # '';
  };
}
