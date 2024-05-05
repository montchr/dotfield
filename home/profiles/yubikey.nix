{ pkgs, ... }:
{
  # FIXME: broken on nixos-unstable 2024-05-05
  # home.packages = [ pkgs.yubikey-manager ];

  programs.gpg.scdaemonSettings = {
    disable-ccid = true;
    # TODO: is this still necessary? only mention in DrDuh guide is for Windows
    reader-port = "Yubico Yubi";
  };

  services.gpg-agent.enableScDaemon = true;
}
