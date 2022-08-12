# TODO: figure out how to include personal or work matches based on which nix
# profiles are enabled. "profiles" may need to have more module-like
# enable/disable behavior to determine what we're working with.
#
# espanso does have the ability to import files from within configs.
#
# https://espanso.org/docs/matches/organizing-matches/#imports
#
# one idea for a workaround: a proxy config file could be written to the config
# directory, containing only an import directive. but all that said, there's
# probably a much simpler way.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  xdg.configFile."espanso" = {
    source = "${pkgs.dotfield-config}/espanso";
    recursive = true;
  };
}
