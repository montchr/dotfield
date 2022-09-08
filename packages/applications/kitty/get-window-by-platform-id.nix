{
  jq,
  writeShellScriptBin,
  stdenv,
  kitty,
}: let
  kitty =
    if stdenv.targetPlatform.isDarwin
    then "kitty"
    else "${kitty}/bin/kitty";
in
  # FIXME: provide a better indication that there can be multiple kitty windows in
  # a single platform window. perhaps 'tab' is better suited?
  writeShellScriptBin "kitty-get-window-by-platform-id" ''
    ${kitty} @ --to $KITTY_SOCKET ls \
      | ${jq}/bin/jq -r --argjson id "$1" \
        '.[] | select(.platform_window_id==$id)'
  ''
