{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.handlr-regex
        (pkgs.writeShellScriptBin "xterm" ''
          handlr launch x-scheme-handler/terminal -- "$@"
        '')
        (pkgs.writeShellScriptBin "xdg-open" ''
          handlr open "$@"
        '')
      ];
    };
}
