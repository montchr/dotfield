{
  programs.atuin.enable = true;

  ##: <https://atuin.sh/docs/config/>
  programs.atuin.settings = {
    auto_sync = true;
    dialect = "us";
    sync_frequency = "10m";
    search_mode = "skim";
    filter_mode = "global";
    filter_mode_shell_up_key_binding = "directory";
    #: Hide commands from history by regex pattern.
    #: <https://atuin.sh/docs/config/#history_filter>
    history_filter = [
      # "^secret-cmd"
      # "^innocuous-cmd .*--secret=.+"
    ];
  };
}
