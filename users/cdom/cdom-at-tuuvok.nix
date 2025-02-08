_hmArgs: {
  imports = [
    ../../home/mixins/sway.nix
    ../../home/mixins/workstation.nix
    ../../home/mixins/jobwork.nix

    ../../home/profiles/mail/default.nix
    ../../home/profiles/mail/accounts/outlook.nix

    ../../home/profiles/shells/fish/default.nix
    ../../home/profiles/shells/fish/trampoline.nix

    ../../home/profiles/git/with-pgp-signing.nix
    ../../home/profiles/gpg/with-ssh-support.nix
    # ../../home/profiles/graphical/sessions/gnome/extensions/just-perfection.nix
    #    ../../home/profiles/graphical/sessions/gnome/extensions/paperwm.nix
  ];

  programs.firefox.profiles.work.isDefault = true;
  programs.firefox.profiles.home.isDefault = false;

  # The trackpad on this device is huge, and I always end up touching
  # its corner with my palm, which is very disruptive.  Actually, it is
  # not only disruptive, but also has led to pain due to habitual thumb
  # hyper-extension in avoidance of the trackpad.
  #
  # FIXME: still needs some way to disable touch input until explicitly needed...
  dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = false;

  home.stateVersion = "23.05";
}
