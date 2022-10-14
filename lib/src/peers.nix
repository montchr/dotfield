{peers, ...}: let
  getHost = hostName: peers.hosts.${hostName} or false;
  getNet = network: peers.networks.${network} or false;
in {inherit getHost getNet;}
