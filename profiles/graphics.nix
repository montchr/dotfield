{config, lib, pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    imagemagick
    # inkscape
    gimp
    gimpPlugins.resynthesizer
  ];
}
