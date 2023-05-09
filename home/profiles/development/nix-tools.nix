{pkgs, ...}: {
  home.packages = [
    pkgs.nix-init #   <- generate nix package expressions from url
  ];

  ##: cli+repl docs + ctags nix plugin
  programs.nix-doc.enable = true;
  # FIXME: see note in module
  # programs.nix-doc.plugin.enable = true;
}
