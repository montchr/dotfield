{
  dotfield.users.cdom.features.mail.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.protonmail-bridge-gui ];
    };
}
