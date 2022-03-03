{ config, pkgs, suites, ... }:

{
  imports = with suites; (
    darwin-minimal
    ++ developer
  );

  my.username = "runner";
}
