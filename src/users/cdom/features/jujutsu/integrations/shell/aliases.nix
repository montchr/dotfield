{ self, ... }:
let
  lib' = self.lib;

  abbrs = {
    jjd = "jj describe -m";
    jjdn = "jj describe @+ -m";
    jjdp = "jj describe @- -m";
    jjff = "jj diff";
    jjl = "jj log -n 6 --no-pager";
    jjn = "jj new -m";
    jjs = "jj st --no-pager";
  };
in
{
  dotfield.features.development.home = {
    imports = [
      (lib'.shell.makeShellAliasesModule { inherit abbrs; })
    ];
  };
}
