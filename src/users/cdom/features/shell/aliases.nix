{ self, ... }:
let
  lib' = self.lib;

  aliases = {
    "." = "pwd";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";

    # FIXME: needs workaround for nushell
    e = "$EDITOR";

    l = "eza -bl --git --icons --time-style long-iso --group-directories-first";

    # Runs bat without line numbers and wrapping.
    rat = "bat --style=plain --wrap=never";

    top = "btm";
    tree = "eza --tree";
  };

in

{
  dotfield.home = {
    imports = [
      (lib'.shell.makeShellAliasesModule { inherit aliases; })
    ];
  };
}
