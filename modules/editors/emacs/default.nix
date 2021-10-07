{ config, options, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.emacs;
  configDir = "${config.dotfield.configDir}/emacs";
  doomDir = "${config.my.xdgPaths.config}/doom";

  ediffTool = (pkgs.writeScriptBin "ediff-tool"
    (builtins.readFile "${configDir}/bin/ediff-tool"));

in
{
  options = with lib; {
    my.modules.editors.emacs = {
      enable = mkEnableOption false;
      doom.enable = mkEnableOption true;
      ediffTool.package =
        mkOption {
          default = ediffTool;
        };
    };
  };

  config = mkIf cfg.enable (mkMerge [

    (if (builtins.hasAttr "homebrew" options) then
      {
        homebrew.brews = [
          # :lang org (macOS only)
          # TODO: Unavailable in nixpkgs, maybe add it someday (but apparently it's buggy)
          "pngpaste"
        ];
      }
    else { })

    {
      environment.systemPackages = with pkgs; [ emacs ];

      my.hm.xdg.configFile = {
        "doom" = with config.my.hm.lib.file; {
          source = mkOutOfStoreSymlink "${config.dotfield.path}/config/emacs";
        };
      };

      my.modules.zsh.envFiles = [ "${doomDir}/aliases.zsh" ];

      my.env = {
        DOOMDIR = doomDir;
        EDITOR = "emacsclient";
        EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      };

      my.user.packages = with pkgs; [
        emacs

        ediffTool

        ## Doom dependencies
        (ripgrep.override { withPCRE2 = true; })
        gnutls

        ## Optional dependencies
        fd # faster projectile indexing
        imagemagick # for image-dired
        pinentry_emacs
        zstd # for undo-fu-session/undo-tree compression

        ## Module dependencies
        # :checkers spell
        (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

        # :checkers grammar
        languagetool

        # :term vterm
        # "Couldn't find cmake command. Vterm module won't compile"
        # -- Probably unnecessary, but okay.
        cmake

        # :tools editorconfig
        editorconfig-core-c

        # :tools lookup & :lang org +roam
        sqlite

        # :tools terraform
        terraform

        # :lang javascript
        nodePackages.javascript-typescript-langserver

        # :lang ledger
        # TODO: probably worth moving to its own module once ready
        # ledger

        # :lang nix
        nixpkgs-fmt
        rnix-lsp

        # :lang org
        graphviz

        # :lang sh
        nodePackages.bash-language-server

        # :lang web
        nodePackages.stylelint
        nodePackages.js-beautify
      ];

      fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

      system.activationScripts.postUserActivation.text = with config.my;
        mkIf cfg.doom.enable ''
          # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
          if [[ ! -d "${xdg.config}/emacs" ]]; then
            git clone https://github.com/hlissner/doom-emacs "${xdg.config}/emacs"
          fi
        '';
    }
  ]);
}
