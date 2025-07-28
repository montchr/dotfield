{
  dotfield.home = {
    programs.atuin.enable = true;

    ##: <https://atuin.sh/docs/config/>
    programs.atuin.settings = {
      dialect = "us";

      search_mode = "fuzzy";
      filter_mode = "directory";
      filter_mode_shell_up_key_binding = "directory";

      # Pseudo-filter for Git repository scope.
      workspaces = true;

      # Hide commands from history by regex pattern.
      # <https://atuin.sh/docs/config/#history_filter>
      history_filter = [
        # "^secret-cmd"
        # "^innocuous-cmd .*--secret=.+"
      ];

      ##: === sync ===

      auto_sync = true;
      sync_frequency = "10m";

      # Opt in to new sync version for existing installs.
      sync.records = true;
    };
  };
}
