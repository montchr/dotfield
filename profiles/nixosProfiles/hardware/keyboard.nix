{pkgs, ...}: {
  hardware.keyboard.zsa.enable = true;
  environment.systemPackages = with pkgs; [wally-cli];

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
