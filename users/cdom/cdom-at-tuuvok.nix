_hmArgs: {
  imports = [
    ./profile.nix

    # ../../home/mixins/hyprland.nix
    ../../home/mixins/sway.nix
    ../../home/profiles/graphical/sessions/gnome/common.nix
    ../../home/mixins/workstation.nix
    ../../home/mixins/jobwork.nix

    ../../home/profiles/mail/default.nix
    # ../../home/profiles/mail/accounts/outlook.nix

    ../../home/profiles/git/with-gpg-signing.nix
    ../../home/profiles/gpg/with-ssh-support.nix
    ../../home/profiles/jujutsu/with-gpg-signing.nix
    ../../home/profiles/jujutsu/with-sign-on-push.nix

    ../../home/profiles/multimedia/music/music-production.nix
  ];

  programs.firefox.profiles.work.isDefault = true;
  programs.firefox.profiles.home.isDefault = false;

  services.kanshi.settings = [
    {
      output.criteria = "eDP-1";
      output.scale = 2.0;
    }
    {
      output.criteria = "LG Electronics LG Ultra HD 0x000668B9";
      output.scale = 2.0;
      output.mode = "3840x2160";
    }
    {
      output.criteria = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
      output.scale = 1.0;
      output.mode = "2560x1440";
    }
    {
      profile.name = "undocked";
      profile.outputs = [
        {
          criteria = "eDP-1";
          status = "enable";
          scale = 2.0;
        }
      ];
    }
    {
      profile.name = "workdock";
      profile.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
          scale = 2.0;
        }
        {
          criteria = "LG Electronics LG Ultra HD 0x000668B9";
          status = "enable";
          position = "0,0";
          scale = 2.0;
        }
      ];
    }
    {
      profile.name = "homedock";
      profile.outputs = [
        {
          criteria = "eDP-1";
          status = "disable";
        }
        {
          criteria = "LG Electronics LG ULTRAGEAR 107NTBKA5869";
          status = "enable";
          position = "0,0";
        }
      ];
    }
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
