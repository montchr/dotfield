{ pkgs, ... }:
{
  services.cliphist.enable = true;
  home.packages = [
    # via <https://github.com/sentriz/cliphist#picker-examples>
    (pkgs.writeShellApplication {
      name = "clipsel";
      runtimeInputs = [
        pkgs.cliphist
        pkgs.fzf
        pkgs.wl-clipboard
      ];
      text = ''
        cliphist list | fzf | cliphist decode | wl-copy
      '';
    })
  ];
}
