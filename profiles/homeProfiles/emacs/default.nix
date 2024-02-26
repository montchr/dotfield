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

  # xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = true;
    package =
      if isDarwin
      then pkgs.emacs29-macport
      # else pkgs.emacs-pgtk; # from master via emacs-overlay
      # else pkgs.emacs29-pgtk;
      else emacs-overlay.packages.emacs-unstable-pgtk; # 29.1.90
    extraPackages = epkgs:
      [
        ##: tree-sitter
        epkgs.treesit-grammars.with-all-grammars
        epkgs.treesit-auto
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

  home.packages = [
    pkgs.fd
    pkgs.ripgrep
    nil-lsp.packages.nil
  ];
}
