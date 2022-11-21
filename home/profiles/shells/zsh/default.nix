{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (config) xdg;
  shellAliases = (import ../abbrs.nix) // (import ../aliases.nix);
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = ["/share/zsh"];
  home.packages = [
    (pkgs.zsh-completions.overrideAttrs (o: {
      version = inputs.zsh-completions.rev;
      src = inputs.zsh-completions;
    }))
  ];

  programs.starship.enableZshIntegration = true;

  programs.zsh = {
    inherit shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    # enableVteIntegration = true;
    autocd = true;

    # use zdharma-continuum/fast-syntax-highlighting instead
    enableSyntaxHighlighting = false;
    # zsh-autosuggestions is loaded manually below to insure proper load order
    # after fzf-tab.
    enableAutosuggestions = false;

    defaultKeymap = "viins";

    initExtraFirst = ''
      # Initialise the builtin profiler -- run `zprof` to read results
      zmodload zsh/zprof
    '';

    history.path = "${xdg.dataHome}/zsh/history";
    history.expireDuplicatesFirst = true;
    history.extended = true;
    history.ignoreDups = true;
    history.ignoreSpace = true;
    history.save = 10000;
    history.share = true;
    history.size = 10000;

    historySubstringSearch.enable = true;

    initExtraBeforeCompInit = ''
      fpath+=${pkgs.zsh-completions}/src
    '';

    plugins = [
      # NOTE: fzf-tab requires a very-specific load order -- after compinit but
      # before other plugins defining zle widgets. including it here causes
      # fzf-tab to load after zsh-autosuggestions when the latter was enabled
      # via the hm option, which the fzf-tab readme explicitly warns against. so
      # instead we load zsh-autosuggestions manually after fzf-tab.
      # https://github.com/Aloxaf/fzf-tab
      {
        name = "fzf-tab";
        inherit (pkgs.zsh-fzf-tab) src;
      }
      {
        name = "zsh-autosuggestions";
        inherit (pkgs.zsh-autosuggestions) src;
      }
      {
        name = "zsh-autopair";
        # The `v1.0.0` version in nixpkgs is over a year old, so use a recent
        # commit instead.
        src = pkgs.fetchFromGitHub {
          owner = "hlissner";
          repo = "zsh-autopair";
          rev = "396c38a7468458ba29011f2ad4112e4fd35f78e6";
          sha256 = "sha256-PXHxPxFeoYXYMOC29YQKDdMnqTO0toyA7eJTSCV6PGE=";
        };
      }
      {
        name = "fast-syntax-highlighting";
        src = inputs.zsh-fast-syntax-highlighting;
      }
    ];

    initExtra = ''
      export DOTFIELD_USER_ZDOTDIR="$DOTFIELD_DIR/home/users/cdom/config/zsh"

      source $DOTFIELD_USER_ZDOTDIR/main.zsh
    '';

    sessionVariables = {
      ZSH_CACHE = "${xdg.cacheHome}/zsh";
      ZSH_DATA = "${xdg.dataHome}/zsh";
    };
  };
}
