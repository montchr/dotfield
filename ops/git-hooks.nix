{
  perSystem =
    { pkgs, ... }:
    {
      # FIXME: must be added to devshell but numtide/devshell is inflexible
      pre-commit = {
        check.enable = true;
        settings.hooks = {
          # Lint GitHub Actions workflow files.
          actionlint.enable = true;
          check-added-large-files.enable = true;
          # Ensure compatibility with case-insensitive filesystems.
          check-case-conflicts.enable = true;
          check-json.enable = true;
          check-merge-conflicts.enable = true;
          check-shebang-scripts-are-executable.enable = true;
          # Check for broken symbolic links.
          check-symlinks.enable = true;
          check-toml.enable = true;
          check-yaml.enable = true;
          # commitizen.enable = true;
          # deadnix.enable = true;
          # TODO: verify
          detect-private-keys.enable = true;
          # EditorConfig linter written in Go.
          eclint.enable = true;
          editorconfig-checker.enable = true;
          flake-checker.enable = true;
          # lychee.enable = true;
          markdownlint.enable = true;
          pretty-format-json.enable = true;
          shellcheck.enable = true;
          shfmt.enable = true;
          # statix.enable = true;
          # treefmt.enable = true;
          vale.enable = true;
        };
      };
    };
}
