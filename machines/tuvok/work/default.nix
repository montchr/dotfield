{ flake, ... }:
{
  imports = [
    flake.inputs.microvm.nixosModules.host

    # ./modules/node.nix
    # ./modules/interface-naming.nix

    # ./modules/guests
  ];

  microvm.host.enable = true;

  networking.hosts = {
    "127.0.0.1" = [
      #      "kleinforms.ddev.site"
      "tutv.ddev.site"
    ];
  };

  security.pki.certificates = [
    # mkcert generated rootCA.pem for local development
    ''
      -----BEGIN CERTIFICATE-----
      MIIEcjCCAtqgAwIBAgIRAOPVfMXO90Zjv2kjGko8H3cwDQYJKoZIhvcNAQELBQAw
      UTEeMBwGA1UEChMVbWtjZXJ0IGRldmVsb3BtZW50IENBMRMwEQYDVQQLDApjZG9t
      QHR1dm9rMRowGAYDVQQDDBFta2NlcnQgY2RvbUB0dXZvazAeFw0yNDA2MTUyMjQ3
      MDNaFw0zNDA2MTUyMjQ3MDNaMFExHjAcBgNVBAoTFW1rY2VydCBkZXZlbG9wbWVu
      dCBDQTETMBEGA1UECwwKY2RvbUB0dXZvazEaMBgGA1UEAwwRbWtjZXJ0IGNkb21A
      dHV2b2swggGiMA0GCSqGSIb3DQEBAQUAA4IBjwAwggGKAoIBgQCyZt64n4kGa104
      h4j6/YgIbm2OcbHM5lIG1ZnAT44Ppi7ssFSV4bFwy2RyoHf36crLqv3KD4yK8Kwh
      qCJ+4sNSh5wnAS2Lo5yFpYdsfFfWMmTBKSf4MZtrh2mutUK0qq51FRf2l/twY4nE
      4sbWqIXjmg5AEy58K/zbNtU2tLULE8+xzLnSqOCII8/1+GpBFT3RE1ZsPkFjksIl
      iNLuDMP6tS36zRSb203uXjCnhru6hhxJQivp6q/90YmDpSZOdRzpaSnaRp5mkfMl
      qN1QHO1PyZVlduaLK4XUAqYYvHh/hBTn6VhaoRw7BuuyxY1QFeksV+VqJrR5PWpo
      5OJdtEqBJ5dS9MivVpGiWqIhY6mkgZJF1LfDbLFLYKE3Uchlbkl/dmHmjBhCDwSS
      FRzuHVAyTU9BirhXFVp4iN1yH0b6qeu2kmlzvSvwWIDF+k8IMgw5ZxQBOz2Gr+n8
      KryViEZE0t2SYwfF10IZtOppd83e0f9uHMO4/9YRkRVLCFv6M7MCAwEAAaNFMEMw
      DgYDVR0PAQH/BAQDAgIEMBIGA1UdEwEB/wQIMAYBAf8CAQAwHQYDVR0OBBYEFIwl
      nQ9rM1i1DvukSItaLtJle/caMA0GCSqGSIb3DQEBCwUAA4IBgQAtQQm6ODzMLkY9
      X0fXGi+qHNmuDHgP47mJJo35ndfWxfVyZud+U+dO6p9s4+KiQ5lGQP2VO9yTMo1E
      2Xg6mcQP+RoM4bkDLGqzjnNeC55jjhUmaOTfz2qRkktgWFeCMW96xHq8kIs5XiY2
      GEHeL1VRfEAjju/Pohc/35BJPqx26bA8UQxFRhZaup0/6TNMDzBL47Be6ysE9tMw
      nhYT34YyMeZkv9K2LPQ+1bK3VakHryarToMc0RHqPYpYFpx1MmUDP0+hI780iFOz
      dFAERd4kaYHtOYsVJrKuI1zWGsOONBCzEARnbY+CEkHgS6HZsQJ+AhS4c4lojkMN
      6nRwcJ+hIznoC+taj3A3rgb9SDZRWN/iNMdLEvF8WLB/DDZ2dEYkdsN0V3fdaM74
      YwDBJODnc60bhy9sbAAdb6PYvOFu4R7GSGjuIRJKO4CYCywbK93SQ8l8/p/DvbeQ
      sYgT2xcwzFLdbHjL0S3RU8awHopuNMxTHUr8s9U3zzCJhPRuElk=
      -----END CERTIFICATE-----
    ''
  ];
}
