{
  aspects.hardware__apple__macbook-intel = {
    requires = [
      "laptop"
      "hardware__apple__macbook"
    ];

    nixos = {
      hardware.facetimehd.enable = true;
      services.mbpfan.enable = true;
    };
  };
}
