{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ vagrant ];

  # https://nixos.wiki/wiki/Vagrant#Using_NFS_mounts
  # TODO: necessary or?
  services.nfs.server.enable = true;

  # TODO: this rule may or may not be correct or necessary...
  # see https://nixos.wiki/wiki/Talk:Vagrant
  # + (lib.optionalString config.virtualisation.libvirtd.enable ''
  #   ip46tables -I INPUT 1 -i vlrblr+ -p tcp -m tcp --dport 2049 -j ACCEPT
  # '');
}
