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
  tsone = {
    age = "age1rza57uagvpnmsu6kras64ajpl48f3wcugyydvvf3g5acj0mulyasw4u665";
    ipv4 = {
      address = "94.130.220.154";
      gateway = "94.130.220.129";
      prefixLength = 26;
    };
    ipv6 = {
      addresses = ["2a01:4f8:13b:17ac::1"];
      gateway = "fe80::1";
      prefixLength = 64;
      subnet = "2a01:4f8:13b:17ac";
    };
    keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGu6JFX4J6H3NgeQyuvCA2z2xh01um119P+tz5ZPWwO root@tsone" "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXREGzvnOtPeSGlRz+Bktw5ls6qyLoKPiEvVOcdPTFUpb/0m3qjI5eNraLfdG4tQ081DRllIgTaQ41gwYxpWKQVmlEOSRjsWrG/XVtVmXpcGPkvGCqNGjgugLLeC+qhEC+BbruhMh5LVb5pDe3ATzJ4BftOK8P8AD4cu7n+oID/MxMm8ZjDJqGccSPhtteHKTot29ofhL2Ra9CYgitQEHHO3/+nl+8C0jErNFINveAeidgDbtEsFYR93DPXQDp454Pqh9S3NdT4DbbwhhsMuVla3eFcFEb+WrjPcSrE6wf2MXYu1agL9Pic89uYMV4kypArAfJW6Su0uxWJkZWboKb71Yan/wwCekEtJyTcvL4443YPb8+gdpGSXAj41ZuWyebEHOyQ5XQNPfqgmQ+TjEb9extYif32HPu47MEoAxAtDOahsTiGABqc2CqBbzVCZfXea/bDBjMpuCfRENmsGDDRO72nqKk3Q+8g2nzkzueftkWggjxTkM1Q78TfN96EzP2WvcBqeaQlMDJSKrG/1WT5o9UENCJ0iAbWLyZ5pnryJSoU0hqEVsKvd6J1W8ljrgXRm5tIs/IAIASbE9QR6bnyCOa54dsfMogleQ3+a3JWk0yGBt/Sp0QKt7aQI/rh2RvzMrJtF4UWoQcVh2fdA8dgz2JgLdZlii57RyEg1wEAw== root@tsone"];
    network = "cubegarden";
    users.cdom.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBzpSe7IG82Fl/djgmhclz1QPtduts0JOm9ZEOt075le cdom@tsone"];
  };
  tuvix = {
    age = "";
    ipv4.address = "192.168.1.155";
    keys = [];
    network = "home";
    users.cdom = {
      age = "age18yqe2svh9ck0san5uzthh6m774r2450hhz4ustguza2l8nttk30qxvtpf4";
      keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMmT25KQRhB56Ym0w81lzRbcYNqWffihEsq/RZ2QE754 cdom@tuvix"];
    };
  };
}