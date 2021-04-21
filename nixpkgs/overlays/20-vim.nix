self: pkgs: {
  vim_configurable = pkgs.vim_configurable.override {
    python = pkgs.python3;
    guiSupport = "no";
    darwinSupport = true;
  };
}
