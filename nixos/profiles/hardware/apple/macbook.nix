{
  imports = [../laptop.nix];

  # <https://wiki.archlinux.org/title/Apple_Keyboard#hid_apple_module_options>
  boot.kernelParams = [
    "hid_apple.fnmode=2" # normally function keys; media keys when Fn held
    "hid_apple.iso_layout=0"
    "hid_apple.swap_opt_cmd=1"
  ];
}
