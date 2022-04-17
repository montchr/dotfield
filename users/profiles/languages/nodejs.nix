moduleArgs @ {
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    nodejs-16_x
    (yarn.override {nodejs = nodejs-16_x;})
  ];

  home.sessionVariables = {
    NODE_REPL_HISTORY = "${config.xdg.dataHome}/node/repl_history";
    NVM_DIR = "${config.xdg.dataHome}/node/nvm";
    NVM_AUTO_USE = "true";
    NVM_BIN = moduleArgs.osConfig.environment.variables.XDG_BIN_HOME or "~/.local/bin";
    NVM_COMPLETION = "true";
    NVM_LAZY_LOAD = "true";
  };

  # direnv + nvm
  programs.direnv.stdlib = ''
    use_nvm() {
      local node_version="$1"

      nvm_sh="$NVM_DIR/nvm.sh"
      if [[ -e "$nvm_sh" ]]; then
        source "$nvm_sh"
        nvm use "$node_version"
      fi
    }
  '';

  # npmrc *requires* that environment variables are encosed in `${...}`
  # braces, but Nix will interpret this as antiquotation within its
  # own language. For that reason, we need to escape the `$` character
  # by preceding it with double single-quotes.
  #
  # https://docs.npmjs.com/cli/v7/configuring-npm/npmrc
  # https://nixos.org/manual/nix/stable/#idm140737322046656
  xdg.configFile."npm/npmrc".text = ''
    email=chris@cdom.io
    init-author-name="Chris Montgomery"
    init-version=0.0.1
    cache=''${XDG_CACHE_HOME}/npm
    tmp=''${XDG_RUNTIME_DIR}/npm
  '';
}
