{
  dotfield.aspects.docker.nixos =
    { config, pkgs, ... }:
    {
      # Any other user who needs to be able to run Docker containers will need to be
      # added to this group.  However, note that this essentially gives container
      # `root` users access to the host system via the socket.
      users.groups.docker.members = config.users.groups.wheel.members;

      virtualisation.docker.enable = true;
      environment.systemPackages = [
        pkgs.docker-compose
      ];
    }

    ++ (config.lib.generateSudoersExtraGroupsModules [ "docker" ]);
}
