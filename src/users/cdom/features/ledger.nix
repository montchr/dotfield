# - <https://www.gizra.com/content/plain-text-accounting-hledger/>
# - <https://memo.barrucadu.co.uk/personal-finance.html>
# - <https://github.com/edkedk99/awesome-hledger?tab=readme-ov-file>
# - <https://github.com/edkedk99/awesome-hledger/tree/master/contrib-resources>
{
  users.cdom.aspects.workstation.home =
    { config, pkgs, ... }:
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

      # TODO:
      # - <https://github.com/olimorris/hledger-forecast>
      home.packages = [
        pkgs.hledger
        pkgs.hledger-ui
        pkgs.hledger-web

        pkgs.hledger-utils # <https://gitlab.com/nobodyinperson/hledger-utils>
        pkgs.ledger-autosync # sync with financial institutions via OFX
        pkgs.puffin # hledger dashboard
      ];
    };
}
