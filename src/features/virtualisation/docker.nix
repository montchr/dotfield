{ lib, ... }:
{
  dotfield.modules."virtualisation/docker".nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        virtualisation.docker.enable = true;
        environment.systemPackages = [
          pkgs.docker-compose
        ];
      }
    ]
    # Any other user who needs to be able to run Docker containers will need to be
    # added to this group.  However, note that this essentially gives container
    # `root` users access to the host system via the socket.
    ++ (config.lib.generateSudoersExtraGroupsModules [ "docker" ]);
}
