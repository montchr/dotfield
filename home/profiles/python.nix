{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    (python3.withPackages (ps:
      with ps; [
        black
        grip
        nose
        pandas
        pip
        poetry
        pylint
        pytest
        setuptools
        types-toml
      ]))

    # lsp server
    pyright
  ];
}
