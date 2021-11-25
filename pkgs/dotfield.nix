{ writeScriptBin }:

{
  ediffTool = (writeScriptBin "ediff-tool"
    (builtins.readFile ../config/emacs/bin/ediff-tool));
}
