{
  aerattum = {
    network = "seadome";
    users = {
      blink.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2HrKDL60obU2mEkV1pM1xHQeTHc+czioQDTqu0gP37 blink@aerattum"];
      workingcopy.keys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCrU4ZPmxcBNnMeLLyBkFcjlG2MwaIUp5deSycmXSb7gIC4MZKH0lvoCXsXBYTocGhwna2mg1SfpolLZzxzWAYpx52RoHeyY6ml/Z1dSJbpMgV5KZ2kqKo1hHar2i9wsc/EZQKv3rlngOSECiwg2LxHOIGGTz/779yEJnfnWnta+5Tnpk4zdgp8j8g+QbY7NFHcZg2mjcy++Nf2psqJsDZVE1JmzNsA30jEGaGDRAaAv9ZHcQf6E3GEpRvr3iqO9YTzOcgdzzl8CvAtZUa1G4piQK6CYkC6HgAvm73+kSm+JxssSfFi3xgK0+RLAUTGa25MH3PAqR9V8lrcuLI891sLEQTtQIIALfzTw04e740DqXRifzasCVo8lMmZBX8Mu+FC0KSFL0254OfHuTHDCWE7fc/3069pcpgAaJGIDj2rE3v631WqoPZpkmvefuu4+n5nvKe4ypwA/OH6h52s3CL7DlcREe6lnBraEzbuXxVL+0JP66yEzK4vFGtZWeTsbo9jyQkoJIw4IkuqHvRxElysOHaQqG08GkjiCBONiGIqk0GQ3pmeyjptfnrVyi2pFGTvVVQ06ZC7If3wywkWXCJzJ2nrD9B+gyRvKv557m24Goj2+LCi6IVZsFIh6r4+vOdaMnX39eol/kWMl1n93D8YG3bBS5JH0fEQsMZEpsUd7Q== WorkingCopy@aerattum"];
    };
  };
  bootstrap-graphical.network = "seadome";
  boschic = {
    age = "age1wes6w27jntng7t0fl2scuz8l0s556e4m3x3q8qfczgah90kyfs3shuqvvl";
    ipv4.address = "192.168.1.214";
    keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILieF9RIyF4rnqOyse7+3RMYQGH8IrBkTOuMb3O4oDXU root@boschic" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC55brzXJiHpeY6ivYPshjG664jx87m7qLLv6ur8r5i9eWjQK9MIW4OB4Bfc4LKjNNSgeZ9Exs9L2V5ci3hFsBUUiANUuKNWrplO8CjHmAYndo1fQ+NlOqwTug+NOIjAjc4tZ2QxO1QLxBrRieF7UTBm52C4v3fIingi38UYGQje9JedFdxhuUWQW4a+VBpjYX7r6QjSd3+yO29u97fi9HxK54aVCW41ylsAOp75ty6HYoOi4cuqsZipFm+u4lTxj9XtEyernVX0dqIJLuKSB1WnZNeTe3U41+VeZG8H5eEiChJGbfWaAXRjYLiU15Akz/PVDXmkD19F7oqeqsU0VkaUi2y3vpADlMEi4jXaLsaX0xNkGvUyTLGr1TKBBlEDspr4ptN5EyvAzUFOwSAyEnsfjL9QaLs4kNgiJjzMYXr8w7CHfXgI7tw3ERryOS9FpfCo92KqHMkkQ8psqI/DOvxREY+uCeU4VPuiXLH2Q9w0TJuN3MC6V6cr3qiaMTZwaM6dLRb9FGHTk2XycP+EgJywM3zsc2MPijb4B9PXWlUIi20mnEQTVsGhiN738QwVTAWIlAPk4GnWcJLtrdRaOXUyEe/HAWks6vXeMAduKM1vF8MzgHKz4QJVsbcaPrOxAnS9i70UeA0qNjn3n9d2C9cATJEVS9OD2GoTbkMWWQ5uw== root@boschic"];
    network = "home";
    tailscale = "100.112.94.38";
    users.seadoom = {
      age = "age1up0p4xs0lze04acxrlxqj374ymu0dfkmc6adz2psmlz8g57ddv2qvf6pd4";
      keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICaZWAdHI5JfTzuXCNOTs8ebMjThMzhxS8uI28KB3uTx seadoom@boschic.loop.garden"];
    };
  };
  brakhage.users.blink.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEBWeCycDkYAqyXcgGOiy8cgnKDy8QL6F8lO+Jh7ZpLk blink@brakhage"];
  hierophant = {
    age = "age1nl8de4w4vlke0nls38jykhlygf3aenr6prkvu6ap5m9a6pd80q8sr9sk00";
    ipv4.address = "100.68.129.15";
    keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP5ffhsQSZ3DsVddNzfsahN84SFnDWn9erSXiKbVioWy hierophant.loop.garden"];
    network = "seadome";
    users.hierophant = {
      age = "age18ngk35h7qycc9kzjc4fy788nvnvlce8gvjahkvkkjxlahd5r54vqhmq9y2";
      keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDwOUQFOaTPMtYG4VWrgHF772sf4MhmK5Rvq4vlUFFXH hierophant@loop.garden"];
    };
  };
  hodgepodge = {
    age = "age1saydygdkg49p6g34ylds3m666x0nsr2f8clwcggsn9essqdkfqjsq9a25w";
    ipv4.address = "192.168.1.152";
    keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRS3UkaJeMQm58v+ggR5e0hVeUbFZkhyQJwEC0LK5YS"];
    network = "home";
    tailscale = "100.71.240.35";
    users.seadoom.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG+iDtB1+DXl89xmlHz6irAYfI2dm4ubinsH3apMeFeo seadoom@hodgepodge.loop.garden"];
  };
  moraine = {
    ipv6 = {
      address = "2a01:4f8:200:5047";
      # TODO: gateway+prefix should be a "default" somewhere...
      gateway = "fe80::1";
      prefixLength = 64;
    };
    keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK1hnnVzNQ1JJ1TOTOWuxztbCV6EZ4F8xZBrEfOwROVX root@moraine"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCu56Rvtpdr8oPtKT+dru8t/GrqwVtngVIAGGjSRMmg4GBbi8OVx5TkKCNFypJKwx1oCUnIb5f11HVdHukpe/TAjxKlF1kQlXJpaCLszQD5Gdf9SBH8gKrNtoqlsnalaU7faWnbfGoPmMPrd6hNVByslAzrC1OHFiI1vqAmnwh5XdU7Y4r8brScPsG4NoRQ9P3bTNMs4Sk1s0UxMAgziHltG/jlcupQxHx5Zdm2of3wXAWdvrRT3k69aPd6PHs+rQOqZkzuY9vRloKxFSINyTnXnJDlCZis0NzoSoeAdfuWj5xfk795NztCJaazkRSp/4Mg7/hkoWfGvVve5WV+kwOEKpLOWpYHNHUgyABLb7QxZ+Yx/9UGZT91kWVX1KxpSEG7S/Azs327flI+lw3b9kBwYyPIV7A2UztfvCmJmKKKyn1rppUDbICCCQb6ADmwSSvqYsRwDXU0QGyns3sUpNjq0F73QJOIGTTE47+lbEVwYYt3JyXSzl1RaUe7Bw5+mpD1z71EkFuO2Y3BtDaqIbqvjY6rKKT5h+8fw0u+WHcSmZIczncZGLp18qR1PSc4nAlFh0inGMbUQbuqIMYsqmmGkWs71/7rHtvBGBEHq90KYfXXGT3bhzv/Xj0gtUr1XXBEHIV+KAM/4mVBdOowCJiVuRzLyQ7cUYJl+INwi3XZIw== root@moraine"
    ];
    network = "cubegarden";
    users.anomich.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINUGLeqyzAYXvrFCTSqumXiJ7xWOCclO0eRf0vlP2eya anomich@moraine"];
  };
  ryosuke = {
    age = "age14rlwkyskyfz65vrvu2n4v3vslqvuqc8uk7vjsre9n52zpnhke30svsjvak";
    ipv4.address = "192.168.1.217";
    keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBPhDD8yaBeZNP0Qv2b4DAx2QUlu+YX41zSod8k8dS8b root@ryosuke" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCfa+BY8CbQBAYXZGRz+x7FxG2QxQCOqnRgg6D9jsUpZjI2So0KIX0h1xxdCiGRr0ZKgKqn+dVln/eAnbPkcIg7QfJjYdMNDgZef89lU+EbDXp4n4kfH1tyRZq273rR6pu6PthKyD6GxCnfMPZmTe9kR3ndDWEz++pS0ChMFRQsUy+zTnTxj/b8O6eTHRRQDYoCvETc1kKfoH/O852/U2hXcQPYR0qpH1TF1nzJhC5sStLWiAws2g2I+53e7XAluBCyqQBPjPlENwVTPH+riTTvUNEg5p7KmZodK3grD1FylvxA/E1iR6PQJT++PkczQyguI51SjMHng9rSFHDn24jT3/g8u2tHuBfKZV8cV6JvX07tUF8Fn1Mmo6Pas0WjJmvx7aFBt2LI/jJ0CYMqcsND84Z6qZQ3f18mQG/pS3N4yNmsVW6XDvMQ+sIU1tJgGlZrbT6ITJDvD4MEyC7UeT1Oy9yS1+IyJfSX48RqHxdeoWXARm58m/rzemx7m7q2UQXhzLlL1jdZwcDsXXa5AOuNWCsmZi6gIr7mVEaVIrXl7S08NGtU8QRmZFE0XA+bbCJP6iL0Gjos6HZvaqRk221coX1idMP8+uFSQhpzDYexF8CHM5j+anO6J/dpUjsy0cgvITnF1gkbv1YkYGSeIC2rrbQPhUJ99Pp6jfN9/5UxFw== root@ryosuke"];
    network = "home";
    tailscale = "100.123.41.68";
    users.cdom = {
      age = "age19une89quzx4jt36hndrdp5ghr43845dndctfta206f7ts5ucsyjqt0l3pm";
      keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL+h4ovqnQQKAr9S4nMGJ4NmRq1klpC9APdDMRiHOI9d cdom@ryosuke"];
    };
  };
  synoxyn = {
    ipv4.address = "192.168.1.197";
    network = "home";
  };
  tuvix = {
    age = "";
    ipv4.address = "192.168.1.155";
    keys = [];
    network = "home";
    users.cdom = {
      age = "age18yqe2svh9ck0san5uzthh6m774r2450hhz4ustguza2l8nttk30qxvtpf4";
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMmT25KQRhB56Ym0w81lzRbcYNqWffihEsq/RZ2QE754 cdom@tuvix"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDczCHJl9sJxhB7uy4tqWGYUy1cV3r6dfj777kXsIVyTgT16rDYN0ySVHL2qPUycNv5Xe1Fsv526vJpJEMX3W28mh7n9Kfp2U6WcMPZTtnOxoXrd9CAv8XjtkMEPzoaRQl37WwAkNT1zD5Qx9pFFS8Q5tipquSECH/iA/xBQ51g6FYIT+VVhVsVjWegQR5YB44e0GnYZdSbcOPP42C1Itxyr4hRHitjWWf+lKoNy0eP5KNVNH1MGGVnpriWohHhXPpOjKO7Fs7RI0TEiNJP8+MuI52c26mc3n1c8yla7/li9GStlxEhaZSPveQ0bnUYgXI5aqPT0oCmsXNVH+Ph1oTCYaqIGVvpidvrN/dJsg0psGfl7l0HSlaYBcebvlM+jorS6EVNBx+pILhXFX+YrDrp5rmMiUFAWdvcxTb1gmeFr27fwZV75G01aZvLQhlt3mTzH0yOpEEZQRlsczL8RH+8DTOqQSyM8j62wxGaNDRSTwka54/Wkn4O9e2ZJ88Xi58= cdom@tuvix"
      ];
    };
  };
}
