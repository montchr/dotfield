# FIXME: startup time has slowed to a crawl since refactor to use rc.d
##: References:
# - <https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org>
{
  config,
  pkgs,
  inputs,
  packages,
  ...
}: let
  inherit (config) xdg;
in {
  imports = [../common.nix];

  home.extraOutputsToInstall = [
    "/share/zsh"
    # TODO: is this already implied by `/share/zsh`?
    "/share/zsh/site-functions"
  ];

  home.packages = [
    packages.yabai-zsh-completions
    packages.zsh-completions
  ];

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    # zsh-autosuggestions is loaded manually below to insure proper load order.
    enableAutosuggestions = false;
    # zsh-autocomplete requires removal of any calls to compinit in zsh config.
    enableCompletion = false;
    enableSyntaxHighlighting = true;
    enableVteIntegration = true;
    autocd = true;

    shellAliases = {
      # Make it easy to copy/paste script commands verbatim
      # FIXME: don't use <space> -- this results in excluding the command from history
      "$" = " ";
    };

    defaultKeymap = "emacs";

    initExtraFirst = ''
      # Initialise the builtin profiler -- run `zprof` to read results
      zmodload zsh/zprof

      ###: --- zsh-autocomplete ---

      ##: Early Settings -- Must be set before sourcing.

      # TODO: 'yes'
      zstyle ':autocomplete:*' fzf-completion no
      # no:  Tab uses Zsh's completion system only. [Default]
      # yes: Tab first tries Fzf's completion, then falls back to Zsh's.

      zstyle ':autocomplete:recent-dirs' backend zoxide
      # cdr:  Use Zsh's `cdr` function to show recent directories as completions. [Default]
      # no:   Don't show recent directories.
      # zsh-z|zoxide|z.lua|z.sh|autojump|fasd: Use this instead (if installed).

      zstyle ':autocomplete:*' widget-style menu-select
      # complete-word: (Shift-)Tab inserts the top (bottom) completion. [Default]
      # menu-complete: Press again to cycle to next (previous) completion.
      # menu-select:   Same as `menu-complete`, but updates selection in menu.

      ##: Load zsh-autocomplete

      # Must be loaded as early as possible, before any calls to `compdef`
      source "${packages.zsh-autocomplete}/share/zsh-autocomplete/zsh-autocomplete.plugin.zsh"
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
    historySubstringSearch.searchUpKey = "^p";
    historySubstringSearch.searchDownKey = "^n";

    plugins = [
      # NOTE: `fzf-tab` requires a very-specific load order -- *after*
      # `compinit` but *before* other plugins defining zle widgets. With
      # `enableAutosuggestions` enabled, even with `fzf-tab` as the first plugin
      # in this list, it will load after `zsh-autosuggestions` as enabled by the
      # related option, which the `fzf-tab` docs explicitly warn against. So
      # instead, we disable `enableAutosuggestions` and load zsh-autosuggestions
      # manually after fzf-tab. https://github.com/Aloxaf/fzf-tab
      #
      # TODO: consider upstreaming this as an option due to its specific load-order needs
      # TODO: remove -- disabled so as not to conflict with `zsh-autocomplete`
      #
      # {
      #   name = "fzf-tab";
      #   inherit (pkgs.zsh-fzf-tab) src;
      # }

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
