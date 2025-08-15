### podman

#### Background:

# This particular configuration precludes the usage of Docker and Podman
# alongside each other. It aims to allow Podman to replace Docker and preserve
# compatibility with tools that expect Docker.
#
# For side-by-side Docker/Podman usage, exporting the `DOCKER_HOST` environment
# variable along these lines may work:
#
# $ export DOCKER_HOST="unix:///run/podman/podman.sock"
#
# And then `unset DOCKER_HOST` should restore the original Docker socket path.

#### Sources:

# - https://docs.hercules-ci.com/arion/#_nixos
# - https://github.com/hercules-ci/arion/issues/122

{ lib, ... }:
{
  dotfield.features."virtualisation/podman".nixos =
    { config, pkgs, ... }:
    lib.mkMerge [
      {
        # Any other user who needs to be able to run Docker-compatible Podman
        # containers will need to be added to this group. However, note that this
        # essentially gives container `root` users access to the host system via the
        # socket.
        users.groups.podman.members = config.users.groups.wheel.members;

        environment.systemPackages = with pkgs; [
          arion
          dive # inspect image layers
          podman-tui

          # Allow the Docker CLI to talk to Podman, as we are going for full
          # compatibility in this scenario. This replaces the need for
          # `podman-compose`.
          docker-client
          docker-compose
        ];

        virtualisation.containers.enable = true;
        virtualisation.docker.enable = false;
        virtualisation.podman = {
          enable = true;
          # NOTE: This only creates a shell alias mapping `docker` to `podman`.
          dockerCompat = true;
          dockerSocket.enable = true;
          # Required for containers under podman-compose to be able to talk to each other.
          defaultNetwork.settings.dns_enabled = true;
        };
      }
    ];
}
