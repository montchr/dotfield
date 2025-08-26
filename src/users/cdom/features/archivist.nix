{ config, ... }:
{
  dotfield.users.cdom.aspects.archivist.home = {
    imports = [ config.dotfield.aspects.archivist.home ];
  };
}
