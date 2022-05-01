{
  config,
  inputs,
  suites,
  profiles,
  ...
}: {
  imports = with suites;
    personal
    ++ gui
    ++ tangibles
    ++ (with profiles; [
      users.xtallos
    ])
    # ++ profiles.emacs
    ++ [./configuration.nix];
}
