let
  machines = {
    alleymon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJsVn0I6Q0rL94W2V89efhUiffAeJfDtHYcW6czXcPkh";
    hodge = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ8uGFMeCGkqrGiJZU3oVP7h0Xq9jEdACINpjRHqi96r";
    parrothelles = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINBvBCsqtgEdC4J+d1xzrwPIircRYSKbFHR0FulaNV5z";
    tso-portal = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUnCW5QopDKLISa/kRcH+28n9QUV/nFuYadXqUp/ZVq";
  };

  trustedUsers = [
    #: yubikey / gpg-agent
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== chris@cdom.io"
    #: xtallos
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHd/G1FMmwKBH4vDjT5q8TiFdeLM/PY/FJlbvc2OwF+S"
    #: montchr@alleymon
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGk9fhwXG95cVD9DLsHuXrdJYs8DsUF/AmYWcO1+bPVd alleymon"
    #: xtallos@parrothelles
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAPdEosvv8H1UpHC725ZTBRY0L6ufn8MU2UEmI1JN1VL xtallos@parrothelles"
  ];

  alleymonRsa = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDg+L0ma2MoSDA9c+ydaOn+is1UWaMvTJJSdEbTsJ0qn75JyNaVB3c2nIqbu3kTS18z6WDeGkAuMHVwJLZWiL+JKqQuAgvLm3pSH7Wmtfako5T3+3xcw6DKgteVFQC0819Xh4CaOCirDSHBdUk/dqZEi5HuYHxmLmd39crVqAV60csS1LXFNgmPF4QvLj0NUcXJyHEkfSgRHKWwZ2JywN5pURc6/7Vl7b/j+r2nvsWcP4yqtAYjSgnxO+H7QZWZ040eMFNqgcqP+LclI3l0jEMW/SC8ZlqvS9hiICXJ6YLQCZmbRfnFxO1IxZwu97892pV/9ZsdFEiLq/fT2f8rTVZLYe4xLIfSw7BNlV7K9VDyD6PLQ9blKlioser2UjTxB/FH1NdoD7YBDhcMiYg9CfVBM34Q98mB21OwysHHLF7ukZ4Mk5H64y2cuCWRG+ze4CPLu7gK0zTb32eZh9AmUqljZgPs3FL28BJAWwZii4jzV1Hd7KTZ25Pshrk1bCpfAI8=";
  allMachines = with machines; [
    alleymon
    hodge
    parrothelles
    tso-portal
  ];
in {
  "wireless.env.age".publicKeys = allMachines ++ trustedUsers;

  "aws/aws-cdom-default.pem.age".publicKeys = allMachines ++ trustedUsers;

  "espanso/personal.yml.age".publicKeys = allMachines ++ trustedUsers;
  "espanso/work.yml.age".publicKeys = [machines.alleymon] ++ trustedUsers;
}
