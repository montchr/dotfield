moduleArgs @ { config
, lib
, pkgs
, self
, ...
}:
let
  inherit (pkgs.stdenv) buildPlatform hostPlatform;
  inherit (config.xdg) configHome;
  inherit (config.lib.dag) entryAfter;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (config.lib.dotfield.emacs) profilesBase profilesPath;

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  emacsDir = "${configHome}/emacs";
in
{
  home.sessionVariables = {
    EMACSDIR = emacsDir;

    # "default" profile
    # FIXME: profiles seem broken, see doom issue tracker
    # DOOMPROFILE = "doom";

    DOOMDIR = "${configHome}/doom";

    # local state :: built files, dependencies, etc.
    # TODO: may no longer be necessary with doom profiles. re-evaluated.
    # DOOMLOCALDIR = doomStateDir;

    # lsp: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  home.sessionPath = [ "${configHome}/emacs/bin" "$PATH" ];

  ## Doom Bootloader.
  #: <https://github.com/doomemacs/doomemacs/commit/5b6b204bcbcf69d541c49ca55a2d5c3604f04dad>
  # FIXME: profiles seem broken
  # xdg.configFile."emacs/profiles/doom".source =
  #   mkOutOfStoreSymlink "${profilesPath}/doom";
  # xdg.configFile."emacs/profiles/xtallos".source =
  #   mkOutOfStoreSymlink "${profilesPath}/xtallos";

  # FIXME: use doom profile loader once issues are fixed upstream
  xdg.configFile."doom".source =
    mkOutOfStoreSymlink "${profilesPath}/doom";

  # Install Doom imperatively to make use of its CLI.
  # While <github:nix-community/nix-doom-emacs> exists, it is not recommended
  # due to the number of oddities it introduces.
  home.activation.installDoomEmacs =
    let
      git = "$DRY_RUN_CMD ${pkgs.git}/bin/git";
    in
    entryAfter [ "writeBoundary" ] ''
      if [[ ! -f "${emacsDir}/README.md" ]]; then
        cd ${emacsDir}
        ${git} init --initial-branch master
        ${git} remote add origin ${doomRepoUrl}
        ${git} fetch --depth=1 origin master
        ${git} reset --hard origin/master
      fi
    '';

  programs.emacs = {
    enable = true;
    package =
      if (hostPlatform.isDarwin && buildPlatform.isMacOS)
      then (pkgs.emacsPlusNativeComp or pkgs.emacsNativeComp)
      else if (moduleArgs.osConfig.services.xserver.enable or false)
      then pkgs.emacsPgtkNativeComp
      else pkgs.emacsNativeComp;
    extraPackages = epkgs: with epkgs; [ vterm ];
  };

  services.emacs = lib.mkIf (!hostPlatform.isDarwin) {
    # Doom will take care of running the server.
    enable = lib.mkDefault false;
    defaultEditor = lib.mkForce true;
    socketActivation.enable = true;
  };

  home.packages = with pkgs; [
    ediff-tool
    gnutls
    (ripgrep.override { withPCRE2 = true; })

    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    #: org
    graphviz

    # FIXME: sqlite binary unusable in org-roam and forge even after supplying
    # them... so we let these packages compile the binary...     
    gcc
    sqlite

    editorconfig-core-c

    ##: === writing ===

    (aspellWithDicts (ds:
      with ds; [
        en
        en-computers
        en-science
      ]))
    languagetool

    ##: === lang/lsp ===

    #: docker
    nodePackages.dockerfile-language-server-nodejs
    #: terraform
    terraform
    terraform-ls
    #: css
    nodePackages.vscode-css-languageserver-bin
    #: js
    nodePackages.eslint
    nodePackages.typescript-language-server
    #: json
    nodePackages.vscode-json-languageserver
    #: ledger
    # FIXME: marked as broken upstream
    # ledger
    #: markdown
    nodePackages.unified-language-server
    #: nix
    rnix-lsp
    #: php
    nodePackages.intelephense
    #: python
    pipenv
    (python310.withPackages (ps:
      with ps; [
        black
        grip
        nose
        pip
        poetry
        pylint
        pytest
        setuptools
      ]))
    #: ruby
    rubyPackages.solargraph
    #: sh
    nodePackages.bash-language-server
    #: toml
    taplo-lsp
    #: web-mode
    nodePackages.js-beautify
    nodePackages.stylelint
    nodePackages.vscode-html-languageserver-bin
    #: yaml
    nodePackages.yaml-language-server
    #: vimrc
    nodePackages.vim-language-server
  ];
}
