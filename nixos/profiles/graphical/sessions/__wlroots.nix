{ pkgs, ... }:
{
  imports = [
    ../common.nix
    ../kanshi.nix
  ];

  environment.systemPackages = with pkgs; [
    # essentials
    brightnessctl # display brightness
    grim # screenshot
    slurp # screenshots
    wev # input monitoring
    wl-clipboard # clipboard

    # swappables
    nautilus # gnome file manager
  ];
}
