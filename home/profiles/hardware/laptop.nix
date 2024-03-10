{
  imports = [ ./touchpad.nix ];

  dconf.settings."org/gnome/desktop/interface".show-battery-percentage = true;
}
