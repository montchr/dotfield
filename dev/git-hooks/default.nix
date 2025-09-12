{
  perSystem =
    { pkgs, ... }:
    {
      pre-commit = {
        check.enable = false;
        settings = {
          default_stages = [
            "pre-commit"
            "pre-push"
          ];
          excludes = [
            # "**/*.secrets.yaml"
            "\.(age|gpg)$"
            "secrets.ya?ml$"
            "blesh/init.sh$"
          ];
          hooks = {
            actionlint.enable = true;
            biome.enable = true;
            check-added-large-files.enable = true;
            check-case-conflicts.enable = true;
            check-executables-have-shebangs.enable = true;
            check-merge-conflicts.enable = true;
            check-symlinks.enable = true;
            check-toml.enable = true;
            check-vcs-permalinks.enable = true;
            detect-private-keys.enable = true;
            eclint.enable = true;
            markdownlint.enable = true;
            shellcheck.enable = true;
            yamllint.enable = true;
          };
        };
      };
    };
}
