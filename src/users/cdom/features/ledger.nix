{
  dotfield.features.workstation.home =
    { config, ... }:
    let
      ledgerDir = "${config.home.homeDirectory}/Documents/ledger";
    in
    {
      home.sessionVariables."DOTFIELD_LEDGER_DIR" = ledgerDir;
      services.git-sync.repositories."montchr__ledger" = {
        uri = "git@codeberg.org:montchr/ledger";
        path = ledgerDir;
      };
    };
}
