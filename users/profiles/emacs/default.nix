{ config, options, lib, pkgs, inputs, ... }:

let
  inherit (config) my;

  configDir = "${config.dotfield.configDir}/emacs";
  emacsDir = "${my.xdg.config}/emacs";
  doomDir = "${my.xdg.config}/doom";

in
lib.mkMerge [
  {
    environment.systemPackages = with pkgs; [ emacs ];
    environment.variables = {
      PATH = [ "${emacsDir}/bin" "$PATH" ];
    };

    my.env = {
      DOOMDIR = doomDir;
      EDITOR = "emacsclient";
      EMACSDIR = emacsDir;
    };

    # FIXME: the module no longer exists!
    # my.modules.shell.rcFiles = [ "${doomDir}/functions.sh" ];

    my.hm.xdg.configFile = {
      "doom".source = configDir;
    };

    my.hm.programs.emacs = {
      enable = true;
      package = pkgs.emacs;
    };

    my.user.packages = with pkgs; [
      dotfield.ediffTool

      ## Doom dependencies
      (ripgrep.override { withPCRE2 = true; })
      (python3.withPackages (ps: with ps; [
        pip
        black
        setuptools
        pylint
        grip
      ]))
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

    system.activationScripts.postUserActivation.text = ''
      # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
      if [[ ! -d "${my.xdg.config}/emacs" ]]; then
        git clone https://github.com/hlissner/doom-emacs "${my.xdg.config}/emacs"
      fi
    '';
  }

  (if (builtins.hasAttr "homebrew" options) then
    {
      homebrew.brews = [
        # :lang org (macOS only)
        # TODO: Unavailable in nixpkgs, maybe add it someday (but apparently it's buggy)
        "pngpaste"
      ];
    }
  else { })
]
