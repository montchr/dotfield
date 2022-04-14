{
  config,
  pkgs,
  suites,
  ...
}: {
  imports = with suites;
    minimal;

  my.username = "runner";
}
