{lib, ...}: {
  programs.helix.enable = true;
  programs.helix.defaultEditor = lib.mkDefault true;
  programs.helix.settings = {
    # theme = "base16";
    editor = {
      lsp.display-messages = true;
    };
  };
}
