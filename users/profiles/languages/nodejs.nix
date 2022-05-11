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

    # volta: the hassle-free javascript tool manager
    # https://volta.sh/
    VOLTA_HOME = "${config.xdg.dataHome}/volta";
  };

  programs.direnv.stdlib = ''
    use_nvm() {
      watch_file .nvmrc

      export NVM_DIR="$PWD/.nvm"
      local nvm_path="$NVM_DIR/nvm.sh"

      if ! [ -f "$nvm_path" ]; then
        echo "Installing NVM" >&2
        mkdir -p "$NVM_DIR"

        # Run the NVM installer but prevent it from attempting to install its
        # hooks into the user's global shell configuration files.
        curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh \
          | PROFILE="/dev/null" bash
      fi

      . "$nvm_path"

      nvm install
      layout node
    }

    # https://github.com/direnv/direnv/wiki/Node#use-volta-with-node
    use_volta() {
      export VOLTA_HOME="$PWD/.volta"
      PATH_add "$VOLTA_HOME/bin"

      if ! [ -f "$VOLTA_HOME/bin/volta" ]; then
        curl https://get.volta.sh/ | bash
      fi

      layout node
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
