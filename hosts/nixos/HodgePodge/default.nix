{
  config,
  inputs,
  suites,
  profiles,
  ...
}: {
  imports =
    (with suites;
      personal
      ++ graphical
      ++ tangible)
    ++ (with profiles; [
      users.xtallos
    ])
    ++ [./configuration.nix];
}
