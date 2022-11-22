
# Format source files
fmt *FILES='$PRJ_ROOT':
  treefmt --no-cache {{FILES}}

# Format all changed source files
fmt-changed:
  treefmt --no-cache `git diff --name-only --cached`

# Check for Nix syntax errors
evalnix file:
  nix-instantiate --parse --quiet {{file}} >/dev/null

# Check Nix files for unused statements
deadnix *ARGS='$PRJ_ROOT':
  deadnix check --no-underscore --fail \
    --no-lambda-arg \
    --no-lambda-pattern-names \
    {{ARGS}}
