{
  pkgs,
  config,
  flake,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  inherit (flake.perSystem.inputs') emacs-overlay nil-lsp;
  # inherit (config) xdg;
  # inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  imports = [
    # TODO: consolidate all extra non-elisp packages
    ./extra-packages.nix
  ];

  home.sessionVariables = {
    ##: lsp-mode: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    # FIXME: broken since... recent update... (at least with nil-lsp for nix)
    # LSP_USE_PLISTS = "true";
  };

  # xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = true;
    package =
      if isDarwin
      then pkgs.emacs29-macport
      else emacs-overlay.packages.emacs-unstable-pgtk;
    extraPackages = epkgs:
      (import ./emacs-packages.nix epkgs)
      ++ [
        ##: tree-sitter
        # via <https://github.com/pimeys/nixos/blob/cc608789192a1c33a6cdb598b59e1543c91f6fb7/desktop/emacs/default.nix>
        # referred from <https://github.com/NixOS/nixpkgs/pull/150239>
        #
        # FIXME: not working in Emacs yet
        #
        # (treesit-language-available-p 'toml)
        # => nil
        #
        # M-x toml-ts-mode
        # => ⛔ Warning (treesit): Cannot activate tree-sitter, because language grammar for toml is unavailable (not-found): (libtree-sitter-toml.so libtree-sitter-toml.so.0 libtree-sitter-toml.so.0.0 libtree-sitter-toml.dylib libtree-sitter-toml.dylib.0 libtree-sitter-toml.dylib.0.0) No such file or directory
        #
        # installing manually with M-x treesit-install-language-grammar works though
        # FIXME: restore; temporarily disabled to reduce noise in load-path
        # epkgs.tree-sitter
        # (epkgs.tree-sitter-langs.withPlugins (p:
        #   epkgs.tree-sitter-langs.plugins
        #   ++ [
        #     p.tree-sitter-markdown
        #     p.tree-sitter-elisp
        #     p.tree-sitter-make
        #     p.tree-sitter-toml
        #   ]))
      ];
  };

  # services.emacs = {
  #   enable = true;
  #   defaultEditor = true;
  #   socketActivation.enable = isLinux;
  # };

  home.packages = [
    pkgs.fd
    pkgs.ripgrep
    nil-lsp.packages.nil
  ];
}
