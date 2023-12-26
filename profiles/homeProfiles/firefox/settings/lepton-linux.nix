{
  lib,
  isLinux,
  ...
}:
# NOTE: lib.mkIf on stdenv will infinite recursion
lib.optionalAttrs isLinux {
  "userChrome.compatibility.os.linux_non_native_titlebar_button" = true;
  "userChrome.theme.non_native_menu" = true;
}
