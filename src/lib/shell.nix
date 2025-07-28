let
  makeShellAliasesModule =
    {
      aliases ? { },
      abbrs ? { },
    }:
    {
      home.shellAliases = aliases;
      programs.bash.shellAliases = abbrs;
      programs.nushell.shellAliases = abbrs;
      programs.fish.shellAbbrs = abbrs;
    };
in
{
  flake.lib.shell = { inherit makeShellAliasesModule; };
}
