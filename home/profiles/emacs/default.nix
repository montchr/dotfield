{
  config,
  pkgs,
  inputs,
  inputs',
  ...
}: let
  inherit (inputs'.emacs-overlay.packages) emacsGit;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) xdg;
  inherit (config.lib.dag) entryAfter;

  l = inputs.nixpkgs.lib // builtins;

  profilesBasePath = "${xdg.configHome}/dotfield/home/users/cdom/config/emacs/profiles";

  doomRepoUrl = "https://github.com/doomemacs/doomemacs";
  doomSourceDir = "${xdg.dataHome}/doomemacs";
  doomProfileDir = "${profilesBasePath}/doom";

  defaultProfile = "ceamx";
  defaultProfilePath =
    if defaultProfile == "ceamx"
    then "${xdg.configHome}/${defaultProfile}"
    else "${profilesBasePath}/${defaultProfile}";
in {
  imports = [./extra-packages.nix];

  home.sessionVariables = {
    # <https://github.com/plexus/chemacs2#example-emacs-as-daemon>
    EDITOR = "emacsclient -c -s ${defaultProfile} -a emacs";

    # EMACSDIR = emacsDir;
    CHEMACS_PROFILE = defaultProfile;
    EMACS_STATE_DIR = "${xdg.cacheHome}/${defaultProfile}";
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
    (("default" . ((user-emacs-directory . "${defaultProfilePath}")
                   (server-name . "default")))
     ("doom" . ((user-emacs-directory . "${doomSourceDir}")
                (server-name . "doom")))
     ("ceamx" . ((user-emacs-directory . "${xdg.configHome}/ceamx")
                   (server-name . "ceamx")))
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
    # darwin emacs packages result in app bundles, which hm does not handle
    # well. nix-darwin currently does a (slightly) better job of it. but
    # emacs-plus via homebrew wins.
    enable = !isDarwin;
    package = emacsGit; # bleeding edge from emacs-overlay
    extraPackages = epkgs: with epkgs; [vterm];
  };

  # services.emacs = l.mkIf (!isDarwin) {
  #   # Doom will take care of running the server.
  #   enable = l.mkDefault false;
  #   defaultEditor = true;
  #   socketActivation.enable = true;
  # };

  home.packages = [
    inputs'.nil-lsp.packages.nil
    inputs'.rnix-lsp.packages.rnix-lsp
  ];
}
