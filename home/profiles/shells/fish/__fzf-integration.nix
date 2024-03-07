{
  config,
  lib,
  pkgs,
  ...
}:
let
  hasHistoryService = config.programs.atuin.enable && config.programs.atuin.enableFishIntegration;
in
{
  # <https://github.com/PatrickF1/fzf.fish#installation>
  programs.fzf.enableFishIntegration = false;

  # <https://github.com/PatrickF1/fzf.fish#configuration>
  programs.fish.interactiveShellInit = ''
    # `$ fzf_configure_bindings --help` for explainer
    fzf_configure_bindings \
      ${lib.optionalString hasHistoryService "--history="}

    # Emulate default behavior of <Aloxaf/fzf-tab>
    set fzf_complete_opts --cycle --reverse --height=50%
  '';

  programs.fish.plugins = lib.singleton {
    name = "fzf";
    # Pull request with support for tab completions.
    # <https://github.com/PatrickF1/fzf.fish/pull/293>
    src = pkgs.fetchFromGitHub {
      owner = "oddlama";
      repo = "fzf.fish";
      rev = "6331eedaf680323dd5a2e2f7fba37a1bc89d6564";
      sha256 = "sha256-BO+KFvHdbBz7SRA6EuOk2dEC8zORsCH9V04dHhJ6gxw=";
    };
  };
}
