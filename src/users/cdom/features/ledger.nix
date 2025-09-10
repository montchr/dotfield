{
  aspects.workstation.home =
    { config, ... }:
    let
      inherit (config.home) homeDirectory;
      ledgerDir = "${homeDirectory}/Documents/ledger";
    in
    {
      home.sessionVariables."DOTFIELD_LEDGER_DIR" = ledgerDir;

      services.git-sync.repositories."montchr__ledger" = {
        uri = "git@codeberg.org:montchr/ledger";
        path = ledgerDir;
      };
    };
}
