{
  dotfield.users.cdom.aspects.workstation.nixos =
    { pkgs, ... }:
    {
      hardware.keyboard.qmk.enable = true;
      hardware.keyboard.keyboardio.enable = true;
      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = [
        pkgs.keymapp
        pkgs.wally-cli
      ];
    };

  dotfield.users.cdom.aspects.graphical.home = {
    dconf.settings = {
      "org/gnome/desktop/input-sources".xkb-options = [
        "caps:ctrl_modifier"
      ];
      "org/gnome/desktop/interface".gtk-key-theme = "Emacs";
    };
  };
}
