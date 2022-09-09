# FIXME: indicate platform support. right now this will only work on darwin.
# FIXME: this should depend on the yabai-sa-kickstart script because killing
# Dock will unload the scripting addition...
{
  source,
  writeShellScriptBin,
}:
writeShellScriptBin "kitty-set-app-icon" ''
  SCHEME="''${1:-dark}"
  cp ${source.src}/kitty-$SCHEME.icns /Applications/kitty.app/Contents/Resources/kitty.icns
  rm /var/folders/*/*/*/com.apple.dock.iconcache
  killall Dock
''
