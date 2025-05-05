{
  flake,
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (flake.perSystem.inputs') nil-lsp;
  inherit (flake.lib) mimetypes;

  cfg = config.programs.emacs;

  # via <https://github.com/nix-community/home-manager/blob/80546b220e95a575c66c213af1b09fe255299438/modules/services/emacs.nix#L186C1-L191C11>
  editorPkg = pkgs.writeShellScriptBin "editor" ''
    exec ${lib.getBin cfg.package}/bin/emacsclient "''${@:---create-frame}"
  '';

  sessionVariables = {
    EDITOR = lib.getExe editorPkg;
  };
in
{
  imports = [
    # TODO: consolidate all extra non-elisp packages
    ./extra-packages.nix

    ../writing.nix
  ];

  home = {
    inherit sessionVariables;
  };
  programs.bash = {
    inherit sessionVariables;
  };
  programs.zsh = {
    inherit sessionVariables;
  };

  stylix.targets.emacs.enable = false;
  programs.emacs = {
    enable = true;
    package = if isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
    extraPackages = epkgs: [
      (epkgs.jinx.override { enchant2 = pkgs.enchant; })
      epkgs.pdf-tools
      epkgs.ready-player
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
    ];
  };

  home.packages = [
    nil-lsp.packages.nil

    pkgs.fd
    pkgs.emacs-lsp-booster
    pkgs.enchant
    pkgs.nixd
    pkgs.ripgrep

    editorPkg
  ];

}
