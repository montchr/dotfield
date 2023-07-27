#!/usr/bin/env bash

# FIXME: does not output valid nix because it needs `{` and `}` wrappers...

VERSION=${1:-master}

curl "https://raw.githubusercontent.com/black7375/Firefox-UI-Fix/v${VERSION}/user.js" \
  | sed -E 's/user_pref\((".+"),(\s+)(.+)\)/\1\2= \3/gm'
's%(\s*)/{2,3}%\1#%gm' - >user.js.nix
