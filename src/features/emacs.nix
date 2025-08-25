{
  dotfield.aspects.development.home =
    { pkgs, ... }:
    {
      programs.emacs = {
        enable = true;
        package =
          if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs30-macport else pkgs.emacs-unstable-pgtk;
        ceamx.enable = true;
      };
    };
}
