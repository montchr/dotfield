{
  imports = [
    ./common.nix
    ./aux-mods.nix
    ./basic-oneshot-mods.nix
  ];

  services.keyd.keyboards.default.settings = {
    main = {
      ## auxilliary tap-dance mods
      enter = "overload(control, enter)";
      semicolon = "overload(hyper, semicolon)";
      tab = "overload(meh, tab)";
    };
  };
}
