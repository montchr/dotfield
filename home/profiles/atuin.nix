{
  programs.atuin.enable = true;
  programs.atuin.settings = {
    auto_sync = true;
    dialect = "us";
    sync_frequency = "10m";
    sync_address = "https://api.atuin.sh";
    search_mode = "fuzzy"; # 'prefix' | 'fulltext' | 'fuzzy'

    ##: options: 'global' (default) | 'host' | 'session' | 'directory'
    filter_mode = "global";
    filter_mode_shell_up_key_binding = "directory";
  };
}
