{inputs,...}:{
  aspects.hardware__lenovo__thinkpad-x1-13th-gen.requires = ["laptop"];

aspects.hardware__lenovo__thinkpad-x1-13th-gen.nixos = {
      imports = [inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-13th-gen];

};
}
