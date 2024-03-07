{
  pkgs,
  # config,
  ...
}:
let
  # inherit (config.lib.file) mkOutOfStoreSymlink;
  # dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";
  cmx-espanso-list-triggers = pkgs.writeShellApplication {
    name = "cmx-espanso-list-triggers";
    runtimeInputs = [ pkgs.ripgrep ];
    text = ''
      rg '^\s*(-\s)?(trigger:\s)(.+)$'
    '';
  };
in
{
  # FIXME: just use gnu stow i guess........?
  # xdg.configFile."espanso" = {
  #   source = mkOutOfStoreSymlink "${dotfieldDir}/users/cdom/config/espanso";
  #   recursive = true;
  # };

  home.packages = [ cmx-espanso-list-triggers ];
}
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
