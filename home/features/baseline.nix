{ config, ... }:
{
  imports = [
    ../profiles/core
    ../profiles/development/nix-tools.nix
    ../profiles/fzf.nix
    ../profiles/git/default.nix
    ../profiles/nnn.nix
    ../profiles/ssh.nix

    ./profiles/zoxide.nix
  ];

  programs.atuin.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.helix.enable = true;
  programs.helix.settings = {
    # theme = "base16";
    editor.lsp.display-messages = true;
  };

  programs.navi.enable = true;

  programs.zoxide.enable = true;
  home.sessionVariables."_ZO_DATA_DIR" = config.xdg.dataHome;
}
