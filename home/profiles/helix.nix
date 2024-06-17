{ lib, ... }:
{
  programs.helix.enable = true;
  programs.helix.defaultEditor = false;
  programs.helix.settings = {
    # theme = "base16";
    editor = {
      lsp.display-messages = true;
    };
  };
}
