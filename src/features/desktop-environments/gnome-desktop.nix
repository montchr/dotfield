{
  lib,
  lib',
  self,
  ...
}:
{
  dotfield.modules."desktop-environments/gnome-desktop".nixos =
    { config, pkgs, ... }:
    let
      isAutoLoginEnabled = config.services.displayManager.autoLogin.enable;
    in
    {
      imports = [ self.dotfield.modules.nixos."greeters/gdm" ];

      services.xserver.enable = true;
      services.desktopManager.gnome.enable = true;

      services.gnome.core-developer-tools.enable = true;
      services.gnome.gnome-browser-connector.enable = true;

      environment.systemPackages = with pkgs.gnomeExtensions; [
        clipboard-history # https://extensions.gnome.org/extension/4839/clipboard-history/
        just-perfection # https://gitlab.gnome.org/jrahmatzadeh/just-perfection
      ];

      # Prevent GNOME session crashes when auto-login is enabled.
      # <https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229>
      systemd.services."getty@tty1".enable = (!isAutoLoginEnabled);
      systemd.services."autovt@tty1".enable = (!isAutoLoginEnabled);

      programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;
    };

  dotfield.modules.graphical.home =
    { pkgs, ... }:
    {
      xdg.mimeApps.defaultApplications = lib.mkDefault (
        (lib'.mimetypes.genAssoc lib'.mimetypes.archive "org.gnome.FileRoller.desktop")
        // (lib'.mimetypes.genAssoc lib'.mimetypes.image "org.gnome.Loupe.desktop")
      );
    };
}
