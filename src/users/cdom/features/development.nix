{ lib, ... }:
{
  dotfield.features.development.home =
    { config, pkgs, ... }:
    {
      home.shellAliases."d" = "direnv";

      home.packages = [
        pkgs.copier
        # FIXME: build failure
        # pkgs.ast-grep
        pkgs.lynis # security auditing
        pkgs.quicktype # json schema toolkit
        pkgs.universal-ctags

        pkgs.jq-lsp

        # checkers & formatters {{{
        pkgs.dotenv-linter

        pkgs.nodePackages.prettier
        pkgs.shfmt
        pkgs.yamllint
        # }}}

      ];

      home.sessionVariables = {
        NODE_REPL_HISTORY = "${config.xdg.dataHome}/node/repl_history";
        NVM_DIR = "${config.xdg.dataHome}/node/nvm";
      };

      # npmrc *requires* that environment variables are encosed in `${...}`
      # braces, but Nix will interpret this as antiquotation within its
      # own language. For that reason, we need to escape the `$` character
      # by preceding it with double single-quotes.
      #
      # https://docs.npmjs.com/cli/v7/configuring-npm/npmrc
      # https://nixos.org/manual/nix/stable/#idm140737322046656
      xdg.configFile."npm/npmrc".text = ''
        email="${whoami.email}"
        init-author-name="${whoami.name}"
        init-version=0.0.1
        cache=''${XDG_CACHE_HOME}/npm
      '';

      # NOTE: This will significantly slow down builds.  However, it enables more
      # manpage integrations across various tools (e.g. `apropos`, `man -k`).
      programs.man.generateCaches = true;

    };
}
