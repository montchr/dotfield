{
  aspects.development.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.programs.jujutsu;
    in
    {
      programs.jujutsu.settings.aliases = {
        run-all-tools =
          let
            impl = pkgs.writeNushellApplication {
              name = "jj-run-all-tools";
              runtimeInputs = [ cfg.package ];
              text = builtins.readFile ./jj-run-all-tools.nu;
            };
          in
          {
            doc = "Run tools configured in [run.tools]";
            definition = [
              "util"
              "exec"
              "--"
              (lib.getExe impl)
            ];
          };
      };
    };
}
