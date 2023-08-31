{
  pkgs,
  config,
  flake,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (flake.perSystem.inputs') emacs-overlay nil-lsp;
  inherit (config) xdg;
  inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  imports = [./extra-packages.nix];

  home.sessionVariables = {
    ##: lsp-mode: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    # TODO: confirm that this variable is passed through to emacs, esp. on darwin
    LSP_USE_PLISTS = "true";
  };

  # xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = isLinux;
    package = emacs-overlay.packages.emacs-unstable-pgtk;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  # services.emacs = {
  #   enable = true;
  #   defaultEditor = true;
  #   socketActivation.enable = isLinux;
  # };

  home.packages = [
    nil-lsp.packages.nil
  ];
}
