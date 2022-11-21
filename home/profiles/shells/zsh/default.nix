{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (config) xdg;
  shellAliases = (import ../abbrs.nix) // (import ../aliases.nix);
  zsh-completions-latest = pkgs.zsh-completions.overrideAttrs (o: {
    version = inputs.zsh-completions.rev;
    src = inputs.zsh-completions;
  });
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = ["/share/zsh"];
  home.packages = [
    zsh-completions-latest
  ];

  programs.starship.enableZshIntegration = true;

  programs.zsh = {
    inherit shellAliases;

    enable = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableVteIntegration = true;
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
      # Normal plugin load happens too late for `zsh-users/zsh-completions`
      # because of its non-standard source path in `/src/`.
      fpath+=${zsh-completions-latest}/src
    '';

    plugins = [
      # NOTE: `fzf-tab` requires a very-specific load order -- *after*
      # `compinit` but *before* other plugins defining zle widgets. With
      # `enableAutosuggestions` enabled, even with `fzf-tab` as the first plugin
      # in this list, it will load after `zsh-autosuggestions` as enabled by the
      # related option, which the `fzf-tab` docs explicitly warn against. So
      # instead, we disable `enableAutosuggestions` and load zsh-autosuggestions
      # manually after fzf-tab. https://github.com/Aloxaf/fzf-tab
      {
        name = "fzf-tab";
        inherit (pkgs.zsh-fzf-tab) src;
      }
      # See above note -- `zsh-autosuggestions` *MUST* be loaded after
      # `fzf-tab`, but will not do so when enabled via `enableAutosuggestions`.
      {
        name = "zsh-autosuggestions";
        inherit (pkgs.zsh-autosuggestions) src;
      }
      {
        name = "zsh-autopair";
        # The version in nixpkgs is over a year old because it's pinned to the
        # plugin's only tag release (as of 2022-11-21). The author has made some
        # changes since the most recent release.
        src = inputs.zsh-autopair;
      }
      # The `enableSyntaxHighlighting` uses the older and arguably inferior
      # `zsh-users/zsh-syntax-hightlighting` plugin, not the
      # more-commonly-recommended `zdharma-continuum/fast-syntax-highlighting`.
      # The Readme for the latter plugin provides a comparison of the two
      # plugins, demonstrating the improvements made in `fast-syntax-highlighting`.
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
