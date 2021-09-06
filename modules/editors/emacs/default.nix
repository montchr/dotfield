{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.my.modules.editors.emacs;
  configDir = "${config.dotfield.configDir}/emacs";
  doomDir = "${config.my.xdgPaths.config}/doom";
  # emacsclient     = "${pkgs.emacs}/bin/emacsclient -s ${emacs-server}";

  ediffTool = (pkgs.writeScriptBin "ediff-tool"
    (builtins.readFile "${configDir}/bin/ediff-tool"));

in {
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

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ emacs ];

    my.hm.configFile = {
      "doom" = with config.my.hm.lib.file; {
        source = configDir;
        recursive = true;
        onChange = "doom sync";
      };
    };

    my.modules.zsh.envFiles = [ "${doomDir}/aliases.zsh" ];

    my = {
      env = {
        DOOMDIR = doomDir;
        EDITOR = "emacsclient";
        EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      };

      user.packages = with pkgs; [
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
        nixfmt
        rnix-lsp

        # :lang org
        graphviz
        # TODO: not found in nixpkgs -- so where?
        # pngpaste

        # :lang web
        nodePackages.stylelint
        nodePackages.js-beautify
      ];
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    system.activationScripts.postUserActivation.text = with config.my;
      mkIf cfg.doom.enable ''
        # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
        if [[ ! -d "${xdg.config}/emacs" ]]; then
          git clone https://github.com/hlissner/doom-emacs "${xdg.config}/emacs"
        fi
      '';
  };
}
