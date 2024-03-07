# FIXME: fails with non-local storage backend
{ config, flake, ... }:
let
  listenPort = 8080;
  listenPort' = builtins.toString listenPort;
  fqdn = "cache.seadome.net";
in
{
  imports = [ flake.inputs.attic.nixosModules.atticd ];

  sops.secrets.attic-server-token = { };

  services.atticd.enable = true;
  services.atticd.credentialsFile = config.sops.secrets.attic-server-token.path;
  services.atticd.settings = {
    allowed-hosts = [ ];
    # FIXME: wrong! takes over domain
    # api-endpoint = "https://${fqdn}/";
    listen = "[::]:${listenPort'}";
    require-proof-of-possession = true;

    storage = {
      type = "s3";
      bucket = "nix-cache";
      endpoint = "https://6d8968c8e4649cc9191b06fd43c61ac1.r2.cloudflarestorage.com";
      # Not relevant for Cloudflare R2, but this setting is required by attic.
      region = "us-east-1";
    };

    # Data chunking
    # <https://docs.attic.rs/admin-guide/deployment/nixos.html>
    # <https://docs.attic.rs/admin-guide/chunking.html>
    #
    # Warning: If you change any of the values here, it will be
    # difficult to reuse existing chunks for newly-uploaded NARs
    # since the cutpoints will be different. As a result, the
    # deduplication ratio will suffer for a while after the change.
    chunking = {
      # The minimum NAR size to trigger chunking
      #
      # If 0, chunking is disabled entirely for newly-uploaded NARs.
      # If 1, all NARs are chunked.
      nar-size-threshold = 64 * 1024; # 64 KiB

      # The preferred minimum size of a chunk, in bytes
      min-size = 16 * 1024; # 16 KiB

      # The preferred average size of a chunk, in bytes
      avg-size = 64 * 1024; # 64 KiB

      # The preferred maximum size of a chunk, in bytes
      max-size = 256 * 1024; # 256 KiB
    };
  };

  services.nginx.virtualHosts."${fqdn}" = {
    enableACME = true;
    addSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${listenPort'}";
  };
}
