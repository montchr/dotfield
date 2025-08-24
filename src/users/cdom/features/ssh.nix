flake@{ ... }:
let
  inherit (flake.config.dotfield.meta) hosts;
in
{
  dotfield.baseline.home = {
    programs.ssh.includes = [ "~/.config/ssh/config.local" ];
    programs.ssh.matchBlocks = {
      "synoxyn" = {
        hostname = hosts.synoxyn.ipv4.address;
        port = 2367;
      };

      "atlantis" = {
        hostname = "atlantis.whatbox.ca";
        user = "syadasti";
      };
    };
  };
}
