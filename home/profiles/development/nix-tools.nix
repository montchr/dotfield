{pkgs, ...}: {
  home.packages = [
    # Fix an annoying bug in v2.13
    # <https://github.com/NixOS/nix/pull/7764>
    # NOTE: As of <2023-05-09>, v2.15 is not yet usable with <divnix/std>
    # <https://github.com/NixOS/nix/issues/8309>
    # <https://github.com/paisano-nix/direnv/issues/1>
    pkgs.nixVersions.nix_2_14

    pkgs.alejandra
    pkgs.nix-init #   <- generate nix package expressions from url
  ];

  ##: cli+repl docs + ctags nix plugin
  programs.nix-doc.enable = true;
  # FIXME: see note in module
  # programs.nix-doc.plugin.enable = true;
}
