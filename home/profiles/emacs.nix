{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config.xdg) configHome;
  inherit (config.lib.dag) entryAfter;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  l = lib // builtins;

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  profilesPath = "${configHome}/dotfield/home/users/cdom/config/emacs/profiles";
  emacsDir = "${configHome}/emacs";
in {
  home.sessionVariables = {
    EMACSDIR = emacsDir;

    # "default" profile
    # FIXME: profiles seem broken, see doom issue tracker
    # DOOMPROFILE = "doom";

    DOOMDIR = "${configHome}/doom";

    # local state :: built files, dependencies, etc.
    # TODO: may no longer be necessary with doom profiles. re-evaluate.
    # DOOMLOCALDIR = doomStateDir;

    # lsp: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  home.sessionPath = l.mkAfter ["${configHome}/emacs/bin"];

  ## Doom Bootloader.
  # FIXME: profiles still unusable as of 2022-09-19
  # xdg.configFile."emacs/profiles/doom".source = mkOutOfStoreSymlink "${profilesPath}/doom";
  # xdg.configFile."emacs/profiles/xtallos".source = mkOutOfStoreSymlink "${profilesPath}/xtallos";

  # TODO: use doom profile loader once issues are fixed upstream
  xdg.configFile."doom".source = mkOutOfStoreSymlink "${profilesPath}/doom";

  # Install Doom imperatively to make use of its CLI.
  # While <github:nix-community/nix-doom-emacs> exists, it is not recommended
  # due to the number of oddities it introduces (though I haven't tried it).
  home.activation.installDoomEmacs = let
    git = "$DRY_RUN_CMD ${pkgs.git}/bin/git";
  in
    entryAfter ["writeBoundary"] ''
      if [[ ! -f "${emacsDir}/.doomrc" ]]; then
        mkdir -p "${emacsDir}"
        cd ${emacsDir}
        ${git} init --initial-branch master
        ${git} remote add origin ${doomRepoUrl}
        ${git} fetch --depth=1 origin master
        ${git} reset --hard origin/master
      fi
    '';

  programs.emacs = {
    enable = !isDarwin;
    package = pkgs.emacsNativeComp;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  services.emacs = l.mkIf (!isDarwin) {
    # Doom will take care of running the server.
    enable = l.mkDefault false;
    defaultEditor = l.mkForce true;
    socketActivation.enable = true;
  };

  home.packages = with pkgs; [
    gnutls
    (ripgrep.override {withPCRE2 = true;})

    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    #: org
    graphviz
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

    #: lookup +docsets
    wordnet

    ##: === lang/lsp ===

    # NOTE: TypeScript is naturally used by many LSP servers. Some of these,
    # unfortunately, expect `typescript` to be available in the environment, so
    # the LSP servers fail at runtime without it. `vscode-json-languageserver`
    # is one of these bad actors.
    nodePackages.typescript

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
    # NOTE: requires `typescript` dependency in environment (see above)
    nodePackages.vscode-json-languageserver
    #: ledger
    # FIXME: marked as broken upstream
    # ledger
    #: markdown
    nodePackages.unified-language-server
    #: nix
    nil-lsp
    rnix-lsp
    #: php
    nodePackages.intelephense
    #: ruby
    rubyPackages.solargraph
    #: rust +lsp
    rust-analyzer
    #: sh
    nodePackages.bash-language-server
    #: tailwindcss
    nodePackages.tailwindcss
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
