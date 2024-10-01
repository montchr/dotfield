{ self, root, ... }:
let
  inherit (root) keys networks;
  hetznerIp6 = address: {
    inherit address;
    gateway = "fe80::1";
    prefixLength = 64;
  };
in
{
  boschic = {
    age = keys.age.boschic;
    ipv4.address = "192.168.1.214";
    keys = [
      keys.ssh.boschic
      keys.ssh.boschic-rsa
    ];
    network = "home";
    networks.ts = "100.112.94.38";
    users.seadoom = {
      age = keys.age.seadoom-at-boschic;
      keys = [ keys.ssh.seadoom-at-boschic ];
    };
  };
  brakhage.users.blink.keys = [ keys.ssh.blink-at-brakhage ];
  chert = {
    inherit (networks.loopgarden) domain;
    ipv6 = hetznerIp6 "2a01:4f8:c012:6d05";
    keys = [
      # keys.ssh.gabbro
      # keys.ssh.gabbro-rsa
    ];
  };
  gabbro = {
    age = keys.age.gabbro;
    inherit (networks.loopgarden) domain;
    ipv6 = hetznerIp6 "2a01:4f8:c17:a3c3";
    keys = [
      keys.ssh.gabbro
      keys.ssh.gabbro-rsa
    ];
    # users.cdom = {
    #   age = keys.age.cdom-at-hierophant;
    #   keys = [keys.ssh.cdom-at-hierophant];
    # };
  };
  hierophant = {
    age = keys.age.hierophant;
    inherit (networks.seadome) domain;
    ipv6 = hetznerIp6 "2a01:4ff:f0:4717";
    keys = [
      keys.ssh.hierophant
      keys.ssh.hierophant-rsa
    ];
    network = "seadome";
    users.cdom = {
      age = keys.age.cdom-at-hierophant;
      keys = [ keys.ssh.cdom-at-hierophant ];
    };
  };
  hodgepodge = {
    age = keys.age.hodgepodge;
    ipv4.address = "192.168.1.152";
    keys = [ keys.ssh.hodgepodge ];
    network = "home";
    networks.ts = "100.71.240.35";
    users.seadoom.keys = [ keys.ssh.seadoom-at-hodgepodge ];
  };
  platauc = {
    ipv4.address = "78.46.148.56";
    ipv6 = hetznerIp6 "2a01:4f8:c0c:591c";
    network = "seadome";
    # networks.ts = {ipv4.address = "";ipv6.address = "";};

    age = keys.age.platauc;
    keys = [
      keys.ssh.platauc
      keys.ssh.platauc-rsa
    ];

    # users.cdom = {
    #   age = keys.age.cdom-at-platauc;
    #   keys = [keys.ssh.cdom-at-platauc];
    # };

    # Hetzner Cloud // CAX41 // nbg1-dc3 // eu-central
    hardware = {
      mem = 32;
      system = "aarch64-linux";
      vcpus = 16;
    };
  };
  ryosuke = {
    age = keys.age.ryosuke;
    ipv4.address = "192.168.1.217";
    hardware = {
      mem = 32;
      vcpus = 24;
      system = "x86_64-linux";
    };
    keys = [
      keys.ssh.ryosuke
      keys.ssh.ryosuke-rsa
    ];
    network = "home";
    networks.ts.ipv4.address = "100.123.41.68";
    users.cdom = {
      age = keys.age.cdom-at-ryosuke;
      keys = [ keys.ssh.cdom-at-ryosuke ];
    };
  };
  synoxyn = {
    ipv4.address = "192.168.1.197";
    network = "home";
    networks.ts = {
      ipv4.address = "100.106.113.40";
      ipv6.address = "fd7a:115c:a1e0::7601:7129";
    };
    users.cdom = {
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGd0Ao5dHAF1+uOanojoMaE6XBkzsa6ooMNe+rBNY5PI cdom@synoxyn"
      ];
    };
  };
  tuvix = {
    age = keys.age.tuvix;
    hardware = {
      mem = 16;
      system = "aarch64-darwin";
      vcpus = 8;
    };
    ipv4.address = "192.168.1.155";
    keys = [
      keys.ssh.tuvix
      keys.ssh.tuvix-rsa
    ];
    network = "home";
    networks.ts = {
      ipv4.address = "100.88.155.140";
      ipv6.address = "fd7a:115c:a1e0:ab12:4843:cd96:6258:9b8c";
    };
    users.cdom = {
      age = "age18yqe2svh9ck0san5uzthh6m774r2450hhz4ustguza2l8nttk30qxvtpf4";
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMmT25KQRhB56Ym0w81lzRbcYNqWffihEsq/RZ2QE754 cdom@tuvix"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDczCHJl9sJxhB7uy4tqWGYUy1cV3r6dfj777kXsIVyTgT16rDYN0ySVHL2qPUycNv5Xe1Fsv526vJpJEMX3W28mh7n9Kfp2U6WcMPZTtnOxoXrd9CAv8XjtkMEPzoaRQl37WwAkNT1zD5Qx9pFFS8Q5tipquSECH/iA/xBQ51g6FYIT+VVhVsVjWegQR5YB44e0GnYZdSbcOPP42C1Itxyr4hRHitjWWf+lKoNy0eP5KNVNH1MGGVnpriWohHhXPpOjKO7Fs7RI0TEiNJP8+MuI52c26mc3n1c8yla7/li9GStlxEhaZSPveQ0bnUYgXI5aqPT0oCmsXNVH+Ph1oTCYaqIGVvpidvrN/dJsg0psGfl7l0HSlaYBcebvlM+jorS6EVNBx+pILhXFX+YrDrp5rmMiUFAWdvcxTb1gmeFr27fwZV75G01aZvLQhlt3mTzH0yOpEEZQRlsczL8RH+8DTOqQSyM8j62wxGaNDRSTwka54/Wkn4O9e2ZJ88Xi58= cdom@tuvix"
      ];
    };
  };
  tuuvok = {
    hardware = {
      # We share a body...
      inherit (self.tuvix.hardware) mem vcpus;
      # ...different minds, same brain.
      system = "aarch64-linux";
    };
    age = keys.age.tuuvok;
    keys = [
      keys.ssh.tuuvok
      keys.ssh.tuuvok-rsa
    ];
    networks.ts = {
      ipv4 = "100.89.80.26";
      ipv6 = "fd7a:115c:a1e0::1c01:501a";
    };
    users.cdom = {
      age = keys.age.cdom-at-tuuvok;
      keys = [ keys.ssh.cdom-at-tuuvok ];
    };
  };
}
