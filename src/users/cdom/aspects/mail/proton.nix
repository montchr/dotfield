{
  dotfield.users.cdom.aspects.mail.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.protonmail-bridge-gui ];
    };
}
