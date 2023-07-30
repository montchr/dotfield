{config, ...}: {
  services.rtorrent = {
    enable = true;
    openFirewall = true;
    port = 52367;
    rpcSocket = "/run/rtorrent/rpc.sock";
    downloadDir = "/mnt/local/downloads/torrents";
  };
}
