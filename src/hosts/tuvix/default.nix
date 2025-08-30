{ config, ... }:
let
  inherit (config.meta) keys;
in
{
  meta.hosts.tuvix = {
    hardware = {
      mem = 16;
      system = "aarch64-darwin";
      vcpus = 8;
    };
    ipv4.address = "192.168.1.155";
    keys = {
      age = keys.age.tuvix;
      ssh = [
        keys.ssh.tuvix
        keys.ssh.tuvix-rsa
      ];
    };
    network = "home";
    networks.ts = {
      ipv4.address = "100.88.155.140";
      ipv6.address = "fd7a:115c:a1e0:ab12:4843:cd96:6258:9b8c";
    };
    users.cdom = {
      keys = {
        age = "age18yqe2svh9ck0san5uzthh6m774r2450hhz4ustguza2l8nttk30qxvtpf4";
        ssh = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMmT25KQRhB56Ym0w81lzRbcYNqWffihEsq/RZ2QE754 cdom@tuvix"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDczCHJl9sJxhB7uy4tqWGYUy1cV3r6dfj777kXsIVyTgT16rDYN0ySVHL2qPUycNv5Xe1Fsv526vJpJEMX3W28mh7n9Kfp2U6WcMPZTtnOxoXrd9CAv8XjtkMEPzoaRQl37WwAkNT1zD5Qx9pFFS8Q5tipquSECH/iA/xBQ51g6FYIT+VVhVsVjWegQR5YB44e0GnYZdSbcOPP42C1Itxyr4hRHitjWWf+lKoNy0eP5KNVNH1MGGVnpriWohHhXPpOjKO7Fs7RI0TEiNJP8+MuI52c26mc3n1c8yla7/li9GStlxEhaZSPveQ0bnUYgXI5aqPT0oCmsXNVH+Ph1oTCYaqIGVvpidvrN/dJsg0psGfl7l0HSlaYBcebvlM+jorS6EVNBx+pILhXFX+YrDrp5rmMiUFAWdvcxTb1gmeFr27fwZV75G01aZvLQhlt3mTzH0yOpEEZQRlsczL8RH+8DTOqQSyM8j62wxGaNDRSTwka54/Wkn4O9e2ZJ88Xi58= cdom@tuvix"
        ];
      };
    };
    syncthing.id = "RYZCGPD-UTXDTH6-DZ5ZJ4W-QVMBUYS-RYBME76-HXUWNVR-OAJK4VR-F222AAK";
  };
}
