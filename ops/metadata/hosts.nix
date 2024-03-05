{
  super,
  keys,
  ...
}: let
  inherit (super) networks;
  hetznerIp6 = address: {
    inherit address;
    gateway = "fe80::1";
    prefixLength = 64;
  };
in {
  aerattum = {
    network = "seadome";
    users = {
      blink.keys = [keys.ssh.blink-at-aerattum];
      workingcopy.keys = [keys.ssh.workingcopy-at-aerattum-rsa];
    };
  };
  bootstrap-graphical.network = "seadome";
  boschic = {
    age = keys.age.boschic;
    ipv4.address = "192.168.1.214";
    keys = [keys.ssh.boschic keys.ssh.boschic-rsa];
    network = "home";
    networks.ts = "100.112.94.38";
    users.seadoom = {
      age = keys.age.seadoom-at-boschic;
      keys = [keys.ssh.seadoom-at-boschic];
    };
  };
  brakhage.users.blink.keys = [keys.ssh.blink-at-brakhage];
  chert = {
    domain = networks.loopgarden.domain;
    ipv6 = hetznerIp6 "2a01:4f8:c012:6d05";
    keys = [
      # keys.ssh.gabbro
      # keys.ssh.gabbro-rsa
    ];
  };
  gabbro = {
    age = keys.age.gabbro;
    domain = networks.loopgarden.domain;
    ipv6 = hetznerIp6 "2a01:4f8:c17:a3c3";
    keys = [keys.ssh.gabbro keys.ssh.gabbro-rsa];
    # users.cdom = {
    #   age = keys.age.cdom-at-hierophant;
    #   keys = [keys.ssh.cdom-at-hierophant];
    # };
  };
  hierophant = {
    age = keys.age.hierophant;
    domain = networks.seadome.domain;
    ipv6 = hetznerIp6 "2a01:4ff:f0:4717";
    keys = [keys.ssh.hierophant keys.ssh.hierophant-rsa];
    network = "seadome";
    users.cdom = {
      age = keys.age.cdom-at-hierophant;
      keys = [keys.ssh.cdom-at-hierophant];
    };
  };
  hodgepodge = {
    age = keys.age.hodgepodge;
    ipv4.address = "192.168.1.152";
    keys = [keys.ssh.hodgepodge];
    network = "home";
    networks.ts = "100.71.240.35";
    users.seadoom.keys = [keys.ssh.seadoom-at-hodgepodge];
  };
  moraine = {
    age = keys.age.moraine;
    ipv6 = hetznerIp6 "2a01:4f8:200:5047";
    keys = [keys.ssh.moraine keys.ssh.moraine-rsa];
    network = "tso";
    networks.ts = {
      ipv4.address = "100.101.74.89";
      ipv6.address = "fd7a:115c:a1e0:ab12:4843:cd96:6265:4a59";
    };
    users.anomich = {
      age = keys.age.anomich-at-moraine;
      keys = [keys.ssh.anomich-at-moraine];
    };
  };
  ryosuke = {
    age = keys.age.ryosuke;
    ipv4.address = "192.168.1.217";
    keys = [keys.ssh.ryosuke keys.ssh.ryosuke-rsa];
    network = "home";
    networks.ts = "100.123.41.68";
    users.cdom = {
      age = keys.age.cdom-at-ryosuke;
      keys = [keys.ssh.cdom-at-ryosuke];
    };
  };
  synoxyn = {
    ipv4.address = "192.168.1.197";
    network = "home";
  };
  tuvix = {
    age = keys.age.tuvix;
    ipv4.address = "192.168.1.155";
    keys = [keys.ssh.tuvix keys.ssh.tuvix-rsa];
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
  tuvok = {
    age = keys.age.tuvok;
    keys = [keys.ssh.tuvok keys.ssh.tuvok-rsa];
    users.cdom = {
      age = keys.age.cdom-at-tuvok;
      keys = [keys.ssh.cdom-at-tuvok];
    };
  };
  tso = {
    keys = [keys.ssh.tso keys.ssh.tso-rsa];
    users.seed.keys = [keys.ssh.seed-at-tso];
  };
}
