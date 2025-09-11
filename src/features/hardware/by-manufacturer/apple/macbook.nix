{
  aspects.hardware__apple__macbook = {
    requires = [ "laptop" ];
    nixos = {
      # <https://wiki.archlinux.org/title/Apple_Keyboard#hid_apple_module_options>
      boot.kernelParams = [
        "hid_apple.fnmode=2" # normally function keys; media keys when Fn held
        "hid_apple.iso_layout=0" # ANSI
        "hid_apple.swap_opt_cmd=1" # match pc layout
        "hid_apple.swap_fn_leftctrl=1" # match pc layout
      ];
    };
  };
}
