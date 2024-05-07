{ lib, ... }:
{
  programs.nh = {
    enable = true;
    # <https://github.com/viperML/nh/issues/88>
    flake = lib.mkDefault "/etc/nixos";
    clean.enable = true;
    clean.extraArgs = "--keep 5 --keep-since 1w";
  };
}
