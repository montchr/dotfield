{
  dotfield.users.cdom.aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustlings
      ];
    };
}
