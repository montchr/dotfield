{
  dotfield.modules.workstation.nixos = {
    programs.nh = {
      enable = true;
      # <https://github.com/viperML/nh/issues/88>
      flake = "/etc/nixos";
    };
  };
}
