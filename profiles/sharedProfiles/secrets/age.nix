{pkgs, ...}: {
  home-manager.sharedModules = [
    ({lib, ...}: {
      home.sessionVariables = {
        "AGE_KEY_DIR" = lib.mkDefault "$HOME/.age";
      };
    })
  ];

  environment.systemPackages = [
    pkgs.age-plugin-yubikey
    pkgs.rage
  ];
}
