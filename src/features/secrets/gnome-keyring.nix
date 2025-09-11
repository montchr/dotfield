{
  aspects.secret-service__gnome-keyring.nixos =
    { pkgs, ... }:
    {
      services.gnome.gnome-keyring.enable = true;
      environment.systemPackages = [ pkgs.seahorse ];
      # TODO: probably not necessary
      # xdg.portal.config.common."org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
    };
}
