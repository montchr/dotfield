{
  aspects.core = {
    home =
      { config, pkgs, ... }:
      let
        lib' = config.lib;
        homeshickPath = "$HOME/.homesick/repos/homeshick";
      in
      {
        programs.bash.bashrcExtra = ''
          . "${homeshickPath}/homeshick.sh"
        '';
        programs.fish.interactiveShellInit = ''
          . "${homeshickPath}/homeshick.fish"
          . "${homeshickPath}/completions/homeshick.fish"
        '';

        home.activation.ensureHomeshick = lib'.dag.entryAfter [ "writeBoundary" ] ''
          if [[ ! -d "${homeshickPath}" ]]; then
            $DRY_RUN_CMD ${pkgs.git}/bin/git clone https://github.com/andsens/homeshick.git ${homeshickPath}
          fi
        '';
      };
  };
}
