{
  config,
  flake,
  ...
}: let
  inherit (flake.perSystem.inputs') emacs-overlay nil-lsp;
  inherit (config) xdg;
  inherit (config.lib.file) mkOutOfStoreSymlink;
in {
  imports = [./extra-packages.nix];

  home.sessionVariables = {
    # TODO: disabled as $EDITOR until we have something more lightweight for immediate cli usage
    # EDITOR = "emacsclient -c -a emacs";

    ##: lsp-mode: use plists instead of hashtables for performance improvement
    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
    LSP_USE_PLISTS = "true";
  };

  xdg.configFile."emacs".source = mkOutOfStoreSymlink "${xdg.configHome}/ceamx";

  programs.emacs = {
    enable = true;
    package = emacs-overlay.packages.emacs-git;
    extraPackages = epkgs: with epkgs; [vterm];
  };

  # services.emacs = l.mkIf (!isDarwin) {
  #   # Server is started upon first run.
  #   enable = l.mkDefault false;
  #   defaultEditor = true;
  #   socketActivation.enable = true;
  # };

  home.packages = [
    nil-lsp.packages.nil
  ];
}
