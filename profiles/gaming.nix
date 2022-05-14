{config, lib, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    steamcmd
    steam
  ];
}
