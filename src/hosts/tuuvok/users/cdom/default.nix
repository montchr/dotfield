flake@{ ... }:
{
  hosts.nixos.tuuvok = {
    configuration =
      {
        config,
        pkgs,
        ...
      }:
      let
        username = "cdom";
      in
      {
        sops.secrets."users/${username}/hashed-password".neededForUsers = true;

        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
          openssh.authorizedKeys.keys = flake.config.meta.users.cdom.keys.ssh;
          shell = pkgs.bashInteractive;
          extraGroups = [ "wheel" ];
        };
      };

    users.cdom = {
      aspects = with flake.config.users.cdom.aspects; [
        mail
        music-production
      ];
      configuration =
        { pkgs, ... }:
        {
          # FIXME: maybe?
          # programs.firefox.profiles.work.isDefault = true;
          # programs.firefox.profiles.home.isDefault = false;

          programs.jujutsu.signing.gpg.enable = true;
          programs.jujutsu.signing.onPush = true;

          # The trackpad on this device is huge, and I always end up touching
          # its corner with my palm, which is very disruptive.  Actually, it is
          # not only disruptive, but also has led to pain due to habitual thumb
          # hyper-extension in avoidance of the trackpad.
          #
          # FIXME: still needs some way to disable touch input until explicitly needed...
          dconf.settings."org/gnome/desktop/peripherals/touchpad".tap-to-click = false;

          home.packages = with pkgs; [
            zed-editor
          ];

          home.stateVersion = "23.05";
        };
    };
  };
}
