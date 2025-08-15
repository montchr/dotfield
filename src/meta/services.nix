{ config, ... }:
let
  inherit (config.dotfield.meta) networks;
in
{
  dotfield.meta.services = {
    keycloak =
      let
        fqdn = "auth.${networks.seadome.domain}";
      in
      {
        inherit fqdn;
        realms.default = rec {
          name = "master";
          urls.base = "https://${fqdn}/realms/${name}";
          urls.oidc = "${urls.base}/protocol/openid-connect";
        };
      };
  };
}
