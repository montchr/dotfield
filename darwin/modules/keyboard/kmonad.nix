{
  inputs,
  config,
  pkgs,
  packages,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) system;
  inherit (config.environment) systemPath;
  l = inputs.nixpkgs.lib // builtins;
  o = l.options;
  t = l.types;
  cfg = config.keyboard.kmonad;
  deviceClientPath = let
    path = "/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-DriverKit-VirtualHIDDeviceClient.app/Contents/MacOS";
  in
    if hasKarabinerElements
    then path
    else "${pkgs.Karabiner-DriverKit-VirtualHIDDevice}/${path}";
  deviceManagerPath = "/Applications/.Karabiner-VirtualHIDDevice-Manager.app";
  daemonShimPath = "${deviceManagerPath}/kmonad-daemon-shim";
  logPath = "/Library/Logs/KMonad";
  hasKarabinerElements =
    config.keyboard.karabiner-elements.enable
    || config.services.karabiner-elements.enable
    || (l.elem "karabiner-elements" config.homebrew.casks);
in {
  options.keyboard.kmonad = {
    enable = o.mkEnableOption "kmonad";
    package = o.mkOption {
      type = t.package;
      default = inputs.kmonad.packages.${system}.kmonad;
    };
    # configFile
  };
  config = l.mkIf cfg.enable {
    environment.systemPackages = [cfg.package];
    security.accessibilityPrograms = ["${cfg.package}/bin/kmonad"];
    launchd.daemons."kmonad-default".serviceConfig = {
      EnvironmentVariables.PATH = l.concatStringsSep ":" [
        "${cfg.package}/bin"
        deviceClientPath
        systemPath
      ];
      KeepAlive = true;
      Nice = -20;
      ProgramArguments = [
        daemonShimPath
        "--input"
        ''iokit-name "Apple Internal Keyboard / Trackpad"''
        # FIXME
        # (builtins.toString (builtins.toFile "kmonad-default.kbd" ''
        #   (defcfg
        #     input (iokit-name "Apple Internal Keyboard / Trackpad")
        #     output (kext)
        #     fallthrough true
        #     allow-cmd false
        #   )
        #   ${builtins.readFile ../../default.kbd}
        # ''))
      ];
      StandardOutPath = "${logPath}/default-stdout";
      StandardErrorPath = "${logPath}/default-stderr";
      RunAtLoad = true;
    };
    # system.activationScripts.applications.text = l.mkIf (!hasKarabinerElements) ''
    #   echo "copying kmonad shim to karabiner device manager..."
    #   cp ${packages.kmonad-daemon-shim}/bin/kmonad-daemon-shim "${daemonShimPath}"
    #   chmod og= "${daemonShimPath}"
    #   chown root ${daemonShimPath}"
    # '';
  };
}
# Source: <https://github.com/mtoohey31/infra/blob/e477bbf580358609a6c98721c403703d669fee41/darwin/modules/kmonad.nix>

