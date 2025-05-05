{ lib, ... }:
let
  l = import ./_lib.nix { inherit lib; };
in
{
  programs.zsh.initContent = l.mkInitProfiler ''
    ## Initialise the builtin profiler -- run `zprof` to read results
    zmodload zsh/zprof
  '';
}
