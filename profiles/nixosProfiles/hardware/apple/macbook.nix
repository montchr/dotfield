{
  imports = [../laptop.nix];

  boot.kernelParams = ["hid_apple.iso_layout=0"];
}
