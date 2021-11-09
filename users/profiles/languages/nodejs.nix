{ config, lib, pkgs, ... }:

let
  inherit (config.my)
    email
    name
    nix_managed
    website
    ;
in

{
  imports = [
    ../prettier.nix
  ];

  my.env = {
    NODE_REPL_HISTORY = "$XDG_STATE_HOME/node/repl_history";
    NVM_DIR = "$XDG_DATA_HOME/node/nvm";
    NVM_AUTO_USE = "true";
    NVM_BIN = "$XDG_BIN_HOME";
    NVM_COMPLETION = "true";
    NVM_LAZY_LOAD = "true";
  };

  my.hm.xdg.configFile = {
    "npm/npmrc" = {
      # npmrc requires environment variables to be encosed in `${...}`
      # braces, but Nix will interpret this as antiquotation within its
      # own language. For that reason, we need to escape the `$` character
      # by preceding it with double single-quotes.
      # https://docs.npmjs.com/cli/v7/configuring-npm/npmrc
      # https://nixos.org/manual/nix/stable/#idm140737322046656
      text = ''
        # ${nix_managed}
        # vim:ft=conf
        ${lib.optionalString (email != "") "email=${email}"}
        init-license=MIT
        ${lib.optionalString (email != "") "init-author-email=${email}"}
        ${lib.optionalString (name != "") "init-author-name=${name}"}
        ${lib.optionalString (website != "") "init-author-url=${website}"}
        init-version=0.0.1
        cache=''${XDG_CACHE_HOME}/npm
        tmp=''${XDG_RUNTIME_DIR}/npm
      '';
    };
  };
}
