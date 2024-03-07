##: Background
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
#
##: Sources
# - https://docs.hercules-ci.com/arion/#_nixos
# - https://github.com/hercules-ci/arion/issues/122
{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    arion

    # Allow the Docker CLI to talk to Podman, as we are going for full
    # compatibility in this scenario. This replaces the need for
    # `podman-compose`.
    docker-client
  ];
  virtualisation.docker.enable = false;
  virtualisation.podman = {
    enable = true;
    # NOTE: This only creates a shell alias mapping `docker` to `podman`.
    dockerCompat = true;
    dockerSocket.enable = true;
  };
  # Any other user who needs to be able to run Docker-compatible Podman
  # containers will need to be added to this group. However, note that this
  # essentially gives container `root` users access to the host system via the
  # socket.
  dotfield.guardian.user.extraGroups = [ "podman" ];
}
