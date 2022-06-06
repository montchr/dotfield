{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = with pkgs; [ wally-cli ];
}
