{ root, ... }:
let
  inherit (root) networks;
in
{
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
}
