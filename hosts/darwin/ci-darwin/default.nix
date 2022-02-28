{ config, pkgs, suites, ... }:

{
  imports = with suites; (
    darwin-minimal
    ++ developer
  );
}
