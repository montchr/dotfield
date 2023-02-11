{
  config,
  pkgs,
  inputs,
  inputs',
  packages,
  ...
}: let
  inherit (inputs'.emacs-overlay.packages) emacsGit;
  inherit (config) xdg;
  inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  imports = [./extra-packages.nix];

  home.sessionVariables = {
    EDITOR = "emacsclient -c -a emacs";

    ##: lsp-mode: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = true;
    package = emacsGit;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  # services.emacs = l.mkIf (!isDarwin) {
  #   # Server is started upon first run.
  #   enable = l.mkDefault false;
  #   defaultEditor = true;
  #   socketActivation.enable = true;
  # };

  home.packages = [
    # FIXME: avoid regressions by tracking stable version until
    # <https://github.com/oxalica/nil/issues/56> is resolved
    # inputs'.nil-lsp.packages.nil
    pkgs.nil
    inputs'.rnix-lsp.packages.rnix-lsp
  ];
}
