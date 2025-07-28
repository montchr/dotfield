{
  dotfield.modules.mail.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.protonmail-bridge-gui ];
    };
}
