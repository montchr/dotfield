{
  dotfield-config,
  writeScriptBin,
}: {
  ediffTool =
    writeScriptBin "ediff-tool"
    # FIXME: avoid IFD
    (builtins.readFile "${dotfield-config}/emacs/bin/ediff-tool");
}
