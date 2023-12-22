{pkgs, ...}: {
  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = [
    pkgs.keyd
    pkgs.wally-cli
  ];

  services.keyd.enable = true;
  services.keyd.keyboards.default = {
    ids = ["*"];
    settings = {
      main = {
        capslock = "overload(control, esc)";
      };
    };
  };
}
