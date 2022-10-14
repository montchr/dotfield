#!/usr/bin/env bash
#
# Run treefmt on every commit and abort if some files have changed.
#
# To install, copy this file to .git/hooks/pre-commit and make sure it's
# executable.
#
set -euo pipefail

# Redirect stdout to stderr
exec 1>&2

# Run treefmt on the files in the index and record the result.
treefmt --no-cache --fail-on-change --quiet
