{
  aspects.hardware__apple__macbook-14-2 = {
    requires = [
      "hardware__apple__apple-silicon"
      "hardware__apple__macbook"
    ];
    nixos = {
      environment.etc."sway/config".text = ''
        set $laptop eDP-1

        # output $laptop scale 2
        # output $laptop pos 0 0 res 2560x1600
      '';
    };
  };
}
