flake@{ ... }:
{
  dotfield.users.cdom.aspects.theme.home = {
    imports = [ flake.config.dotfield.aspects.theme.home ];
  };
}
