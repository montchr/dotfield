{ config, options, lib, pkgs, inputs, ... }:

let

  inherit (config) home-manager my;
  inherit (home-manager.users.${my.username}.xdg) configHome dataHome stateHome;

  configPath = "${config.dotfield.path}/config";
  chemacsDir = "${configHome}/emacs";

  doomProfilePath = "emacs/profiles/doom";
  vanillaProfilePath = "emacs/profiles/vanilla";
  xtallosProfilePath = "emacs/profiles/xtallos";

  # Note that this points to the doom config directory within the flake source.
  # Consider changing if you run into issues.
  doomDir = "${configPath}/${doomProfilePath}";

  doomDataDir = "${dataHome}/${doomProfilePath}";
  doomStateDir = "${stateHome}/${doomProfilePath}";

  emacsPlus = pkgs.fetchFromGitHub {
    owner = "d12frosted";
    repo = "homebrew-emacs-plus";
    rev = "b7809dd815e7753e20851c81603c82a573d7d1cc";
    sha256 = "sha256-UoMieQKaWB9vSQ75866Kpjb0OKbO1OOj9IwKdAFQit4=";
  };

  emacs29Darwin = ((pkgs.emacsPackagesFor pkgs.emacsPgtkGcc).emacsWithPackages (epkgs: [
    epkgs.vterm
  ]));

  emacsGccDarwin = (pkgs.emacsGcc.overrideAttrs (o: {
    # TODO: add no-titlebar patch! oops.
    patches = o.patches ++ [
      "${emacsPlus}/patches/emacs-28/fix-window-role.patch"
      "${emacsPlus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
      "${emacsPlus}/patches/emacs-28/system-appearance.patch"
    ];
    # TODO: give this a try on next rebuild
    # postPatch = ''
    #   ${postPatch}
    #   cp -f ${emacsPlus}/icons/nobu417-big-sur.icns nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns
    # '';
  }));

in
lib.mkMerge [
  {
    environment.systemPackages = [ emacsGccDarwin ];

    environment.variables = {
      PATH = [ "${doomDataDir}/bin" "$PATH" ];
    };

    my.env = {
      EDITOR = "emacsclient";

      ## doom-emacs
      # user config files
      DOOMDIR = doomDir;
      # local state :: `$DOOMDIR/.local` by default.
      DOOMLOCALDIR = doomStateDir;
      # source
      EMACSDIR = doomDataDir;

      # lsp: use plists instead of hashtables for performance improvement
      # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
      LSP_USE_PLISTS = "true";
    };

    my.hm.xdg.configFile = {
      # chemacs source :: must be installed to $EMACSDIR
      "emacs" = {
        source = pkgs.sources.chemacs.src;
        recursive = true;
      };

      # chemacs config
      "chemacs/profiles.el".text = ''
        (("default" . ((user-emacs-directory . "${doomDataDir}")))
         ("vanilla" . ((user-emacs-directory . "${configPath}/${vanillaProfilePath}")))
         ("xtallos" . ((user-emacs-directory . "${configPath}/${xtallosProfilePath}"))))
      '';

      # chemacs default profile :: will load when no `--with-profile` is provided
      "chemacs/profile".text = "default";
    };

    shell.rcFiles = [ "${configPath}/emacs/functions.sh" ];

    my.hm.programs.emacs = {
      enable = true;
      package = emacsGccDarwin;
      # package = emacs29Darwin;
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

      # :tools docker
      nodePackages.dockerfile-language-server-nodejs

      # :tools lookup
      # & :lang org +roam
      sqlite

      # :tools terraform
      terraform
      terraform-ls

      # :lang css
      nodePackages.vscode-css-languageserver-bin

      # :lang javascript
      nodePackages.eslint
      # eslint, but as a service
      # https://github.com/mantoni/eslint_d.js
      nodePackages.eslint_d
      nodePackages.typescript-language-server

      # :lang json
      nodePackages.vscode-json-languageserver

      # :lang ledger
      ledger

      # :lang markdown
      nodePackages.unified-language-server

      # :lang nix
      nixpkgs-fmt
      rnix-lsp

      # :lang org
      graphviz

      # :lang php
      nodePackages.intelephense

      # :lang python
      python39Packages.python-lsp-server

      # :lang ruby
      rubyPackages.solargraph

      # :lang sh
      nodePackages.bash-language-server

      # :lang toml
      taplo-lsp

      # :lang web
      nodePackages.js-beautify
      nodePackages.stylelint
      nodePackages.vscode-html-languageserver-bin

      # :lang yaml
      nodePackages.yaml-language-server

      ## Additional modes {{{

      # vimrc-mode
      nodePackages.vim-language-server

      ## }}}

    ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

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
