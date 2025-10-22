{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.typst
        pkgs.typstyle # unofficial formatter
        pkgs.tinymist # language server
      ];
    };
}
