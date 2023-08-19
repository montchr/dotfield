{super, ...}: let
  inherit (super) networks;
  fqdn = "auth.${networks.seadome.domain}";
in {
  keycloak = {
    inherit fqdn;
    realms.default = rec {
      name = "master";
      urls.base = "https://${fqdn}/realms/${name}";
      urls.oidc = "${urls.base}/protocol/openid-connect";
    };
  };
}
