{...}: {
  imports = [../common.nix];
  programs.nushell = {
    enable = true;
  };
}
