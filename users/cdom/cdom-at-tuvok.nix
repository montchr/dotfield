_hmArgs: {
  imports = [
    ../../home/mixins/workstation.nix

    ../../home/profiles/development/work/default.nix
    ../../home/profiles/gpg/with-ssh-support.nix
    ../../home/profiles/graphical/applications/jetbrains.nix
    ../../home/profiles/graphical/applications/microsoft-teams.nix
    ../../home/profiles/shells/prompts/starship/default.nix
  ];

  # The trackpad on this device is huge, and I always end up touching
  # its corner with my palm, which is very disruptive.  Actually, it is
  # not only disruptive, but also has led to pain due to habitual thumb
  # hyper-extension in avoidance of the trackpad.
  #
  # FIXME: still needs some way to disable touch input until explicitly needed...
  dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = false;

  home.stateVersion = "23.05";
}
