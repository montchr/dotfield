{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs;
in
{
  options.programs.emacs.defaultEditor = {
    enable = lib.mkEnableOption "emacsclient as default editor";

    package = lib.mkOption {
      # via <https://github.com/nix-community/home-manager/blob/80546b220e95a575c66c213af1b09fe255299438/modules/services/emacs.nix#L186C1-L191C11>
      default = pkgs.writeShellScriptBin "editor" ''
        exec ${lib.getBin cfg.finalPackage}/bin/emacsclient "''${@:---create-frame}"
      '';
      defaultText = ''
        pkgs.writeShellScriptBin "editor" '''
          exec ''${lib.getBin cfg.finalPackage}/bin/emacsclient "''${@:---create-frame}"
        '''
      '';
      type = lib.types.package;
      description = ''
        The editor package to use as default `$EDITOR`.

        The default is a shell script wrapper for the `emacsclient` provided by
        `config.programs.emacs.finalPackage`.
      '';
    };
  };

  config = lib.mkIf (cfg.enable && cfg.defaultEditor.enable) (
    let
      sessionVariables = {
        EDITOR = lib.getExe cfg.defaultEditor.package;
      };
    in
    {
      home.packages = [ cfg.package ];

      home = {
        inherit sessionVariables;
      };
      programs.bash = {
        inherit sessionVariables;
      };
      programs.zsh = {
        inherit sessionVariables;
      };

      # <https://mimetype.io/all-types>
      xdg.mimeApps.defaultApplications =
        let
          # TODO: there are definitely more filetypes...
          mimetypes = [
            "application/atom+xml"
            "application/davmount+xml"
            "application/ecmascript"
            "application/json"
            "application/pgp-encrypted"
            "application/pgp-signature"
            "application/rdf+xml"
            "application/x-latex"
            "application/x-sh"
            "application/x-shellscript"
            "application/x-tex"
            "application/x-tex-tfm"
            "application/x-texinfo"
            "application/xml"
            "application/xml-dtd"
            "application/yaml"
            "test/mimetype"
            "text/css"
            "text/html"
            "text/csv"
            "text/javascript"
            "text/json"
            "text/markdown"
            "text/plain"
            "text/tab-separated-values"
            "text/vnd.curl"
            "text/x-markdown"
            "text/x-python"
            "text/x-vcard"
            "text/org"
            "text/text"
          ];
        in
        lib.genAttrs mimetypes (_: lib.singleton "emacsclient.desktop");
    }
  );
}
