flake@{ self, ... }:
let
  mixins = self.outPath + "/home/mixins";
  profiles = self.outPath + "/home/profiles";
in
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
        };
      };

    users.cdom = {
      configuration =
        { pkgs, ... }:
        {
          imports = [
            (mixins + "/niri.nix")
            (mixins + "/sway.nix")
            (mixins + "/jobwork.nix")

            (profiles + "/ai.nix")
            (profiles + "/graphical/sessions/gnome/common.nix")
            (profiles + "/mail")
            # (profiles + "/mail/accounts/outlook.nix")

            (profiles + "/development/rust.nix")

            (profiles + "/multimedia/music/music-production.nix")
          ];

          programs.firefox.profiles.work.isDefault = true;
          programs.firefox.profiles.home.isDefault = false;

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
