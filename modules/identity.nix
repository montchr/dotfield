{ config
, lib
, pkgs
, ...
}:
let
  username = "cdom";
  emailUser = "chris";
  emailDomain = "cdom.io";
  homePrefix = if pkgs.stdenv.isDarwin then "/Users" else "/home";
  homePath = "${homePrefix}/${username}";
in
with lib.types;
with lib.our;
{
  options = {
    name = mkOpt str username;
    home = mkOpt path homePath;
    githubUsername = mkOpt str "montchr";
    email = mkOpt str "${emailUser}@${emailDomain}";

    keys = {
      primary = {
        pgp = mkOpt' str "0x135EEDD0F71934F3" "PGP public key fingerprint";
        ssh = mkOpt str "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== chris@cdom.io";
      };
      additional = {
        ssh = mkOpt (listOf str) [
          # Blink on iOS
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2HrKDL60obU2mEkV1pM1xHQeTHc+czioQDTqu0gP37 blink"
        ];
      };
    };

    xdg = {
      bin = mkOpt path "${homePath}/.local/bin";
      cache = mkOpt path "${homePath}/.cache";
      config = mkOpt path "${homePath}/.config";
      data = mkOpt path "${homePath}/.local/share";
    };

    activationScripts = { };
  };
}

