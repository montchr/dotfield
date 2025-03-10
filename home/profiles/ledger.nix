# - <https://www.gizra.com/content/plain-text-accounting-hledger/>
# - <https://memo.barrucadu.co.uk/personal-finance.html>
# - <https://github.com/edkedk99/awesome-hledger?tab=readme-ov-file>
# - <https://github.com/edkedk99/awesome-hledger/tree/master/contrib-resources>
{ config, pkgs, ... }:
let
  inherit (config.home) homeDirectory;
  ledgerDir = "${homeDirectory}/Documents/ledger";
in
{
  # TODO:
  # - <https://github.com/siddhantac/puffin>
  # - <https://github.com/olimorris/hledger-forecast>
  home.packages = [
    pkgs.hledger
    pkgs.hledger-ui

    # <https://gitlab.com/nobodyinperson/hledger-utils>
    pkgs.hledger-utils

    # XXX(2024-08-14): build failure
    # pkgs.ledger-autosync

    ## broken:
    # pkgs.hledger-iadd
    # pkgs.haskellPackages.hledger-flow
  ];

  home.sessionVariables."DOTFIELD_LEDGER_DIR" = ledgerDir;

  services.git-sync.repositories."montchr__ledger" = {
    uri = "git@codeberg.org:montchr/ledger";
    path = ledgerDir;
  };
}
