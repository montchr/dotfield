{
  dotfield.users.cdom.features.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustlings
      ];
    };
}
