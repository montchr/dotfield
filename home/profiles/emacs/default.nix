{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) xdg;
  inherit (config.lib.dag) entryAfter;
  l = lib // builtins;

  profilesBasePath = "${xdg.configHome}/dotfield/home/users/cdom/config/emacs/profiles";

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  doomSourceDir = "${xdg.dataHome}/doomemacs";
  doomProfileDir = "${profilesBasePath}/doom";

  # emacsDir = "${xdg.configHome}/emacs";
  defaultProfile = "xtallos";
in {
  home.sessionVariables = {
    # EMACSDIR = emacsDir;
    CHEMACS_PROFILE = defaultProfile;
    DOOMDIR = doomProfileDir;

    ##: lsp-mode: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  home.sessionPath = l.mkAfter ["${doomSourceDir}/bin"];

  xdg.configFile."emacs" = {
    source = inputs.chemacs;
    recursive = true;
  };

  xdg.configFile."chemacs/profiles.el".text = ''
    (("default" . ((user-emacs-directory . "${profilesBasePath}/${defaultProfile}")))
     ("doom" . ((user-emacs-directory . "${doomSourceDir}")
                (server-name . "doom")))
     ("cmachs" . ((user-emacs-directory . "${xdg.configHome}/cmachs")
                   (server-name . "cmachs")))
     ("xtallos" . ((user-emacs-directory . "${profilesBasePath}/xtallos")
                   (server-name . "xtallos"))))
  '';

  # Install Doom imperatively to make use of its CLI.
  # While <github:nix-community/nix-doom-emacs> exists, it is not recommended
  # due to the number of oddities it introduces (though I haven't tried it).
  home.activation.installDoomEmacs = let
    git = "$DRY_RUN_CMD ${pkgs.git}/bin/git";
  in
    entryAfter ["writeBoundary"] ''
      if [[ ! -f "${doomSourceDir}/.doomrc" ]]; then
        mkdir -p "${doomSourceDir}"
        cd ${doomSourceDir}
        ${git} init --initial-branch master
        ${git} remote add origin ${doomRepoUrl}
        ${git} fetch --depth=1 origin master
        ${git} reset --hard origin/master
      fi
    '';

  programs.emacs = {
    enable = !isDarwin;
    package = pkgs.emacsGit; # bleeding edge from emacs-overlay
    extraPackages = epkgs: with epkgs; [vterm];
  };

  # services.emacs = l.mkIf (!isDarwin) {
  #   # Doom will take care of running the server.
  #   enable = l.mkDefault false;
  #   defaultEditor = true;
  #   socketActivation.enable = true;
  # };

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

    # typescript is a required peer dependency for many language servers.
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
