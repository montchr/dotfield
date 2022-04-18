{
  config,
  pkgs,
  suites,
  ...
}: {
  imports = with suites; minimal;
}
