{
  pkgs,
  inputs,
  ...
}: let
  inherit (inputs.nix-std.lib.serde) toTOML;
in {
  home.packages = with pkgs; [rust-motd];
  xdg.configFile."rust-motd/config.toml".text = toTOML {
    filesystems.root = "/";
    user_service_status."syncthing" = "syncthing";
    weather.url = "https://wttr.in/Philadelphia,PA?0";
  };
}
