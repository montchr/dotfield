{
  imports = [
    ../apple-silicon.nix
    ../macbook.nix
  ];

  environment.etc."sway/config".text = ''
    set $laptop eDP-1

    # output $laptop scale 2
    # output $laptop pos 0 0 res 2560x1600
  '';
}
