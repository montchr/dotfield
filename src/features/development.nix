flake@{ lib, ... }:
{
  dotfield.aspects.development.home =
    { config, pkgs, ... }:
    let
      inherit (flake.config.dotfield.meta.users.${config.home.username}) whoami;
    in
    {
      home.packages = [
        pkgs.asciinema
        pkgs.biome
        pkgs.csvkit
        pkgs.jq-lsp
        pkgs.nixpkgs-review
        pkgs.nixfmt-rfc-style
        pkgs.nix-init
        pkgs.nix-update
        pkgs.nodejs
        pkgs.quicktype # json schema toolkit
        pkgs.shellcheck
        pkgs.shfmt
        pkgs.treefmt
        pkgs.universal-ctags
        pkgs.vhs
      ]
      ++ lib.optionals config.programs.fish.enable [ pkgs.fish-lsp ];

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
