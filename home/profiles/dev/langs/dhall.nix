{pkgs, ...}: {
  home.packages = with pkgs; [
    dhall

    ##: libs
    dhallPackages.Prelude

    ##: utils
    dhall-lsp-server

    ##: language support
    dhall-bash
    dhall-json
    dhall-nix
    haskellPackages.dhall-toml

    # > Note that the `dhall-json` package also provides a `dhall-to-yaml`
    # > executable. Currently, the behavior of the `dhall-to-yaml` and
    # > `dhall-to-yaml-ng` executables should not differ much, but eventually
    # > the `dhall-to-yaml-ng` executable should be able to support greater
    # > functionality due to using the `HsYAML` package as an intermediate step
    # > in the transformation process.
    # https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-yaml
    haskellPackages.dhall-yaml
  ];
}
