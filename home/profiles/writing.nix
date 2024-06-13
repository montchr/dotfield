{ flake, pkgs, ... }:
{
  home.packages = [
    flake.packages.aspell-with-dicts

    pkgs.enchant
    pkgs.languagetool
  ];

  # Not the greatest workaround, but...
  # <https://github.com/minad/jinx/discussions/173#discussioncomment-9416580>
  home.sessionVariables.ASPELL_CONF = "dict-dir ${flake.packages.aspell-with-dicts}/lib/aspell";
}
