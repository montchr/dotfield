{ lib, jq, sources, writeShellScriptBin }:

lib.makeExtensible (self: {
  # FIXME: provide a better indication that there can be multiple kitty windows in
  # a single platform window. perhaps 'tab' is better suited?
  #
  # TODO: note that `kitty` is inherited from PATH here due to constant build
  # issues on darwin
  getWindowByPlatformId = writeShellScriptBin "kitty-get-window-by-platform-id" ''
    kitty @ --to $KITTY_SOCKET ls \
      | ${jq}/bin/jq -r --argjson id "$1" \
        '.[] | select(.platform_window_id==$id)'
  '' ;

  # FIXME: this should depend on the yabai-sa-kickstart script because killing
  # Dock will unload the scripting addition...
  setAppIcon = writeShellScriptBin "kitty-set-app-icon" ''
    SCHEME="''${1:-dark}"
    cp ${sources.kitty-bortflower-icons.src}/kitty-$SCHEME.icns /Applications/kitty.app/Contents/Resources/kitty.icns
    rm /var/folders/*/*/*/com.apple.dock.iconcache
    killall Dock
  '';
})
