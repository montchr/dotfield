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
    ++ (with profiles; [
      users.xtallos
    ])
    # ++ profiles.emacs
    ++ [./configuration.nix];
}
