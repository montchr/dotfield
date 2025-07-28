{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkMerge;
  cfg = config.hardware.keyboard.keyboardio;
in
{
  options.hardware.keyboard.keyboardio = {
    enable = lib.mkEnableOption "Keyboard.io keyboard support";
    chrysalis-gui = {
      enable = lib.mkEnableOption "Chrysalis GUI configurator";
      package = lib.mkPackageOption pkgs "chrysalis" { };
    };
  };

  config = mkIf cfg.enable (
    mkMerge [
      {
        users.groups.plugdev = { };

        services.udev.packages = [
          (pkgs.writeTextFile {
            name = "keyboardio-udev-rules";
            destination = "/etc/udev/rules.d/50-keyboardio.rules";
            text = ''
              # Atreus
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2302", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2303", SYMLINK+="Atreus2", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
              # Model 01
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2300", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="1209", ATTRS{idProduct}=="2301", SYMLINK+="model01", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
              # Model 100
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0005", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
              SUBSYSTEMS=="usb", ATTRS{idVendor}=="3496", ATTRS{idProduct}=="0006", SYMLINK+="model100", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_CANDIDATE}="0", TAG+="uaccess", TAG+="seat"
            '';
          })
        ];
      }
      (mkIf cfg.chrysalis-gui.enable {
        environment.systemPackages = [ pkgs.chrysalis ];
      })
    ]
    ++ config.lib.generateSudoersExtraGroupsModules [ "plugdev" ]
  );
}
