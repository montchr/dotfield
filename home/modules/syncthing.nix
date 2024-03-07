{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  cfg = config.services.syncthing;
in
{
  meta.maintainers = [ maintainers.rycee ];

  # Use our local fork of these modules while still pending upstream changes.
  disabledModules = [ "services/syncthing.nix" ];

  options = {
    services.syncthing = {
      enable = mkEnableOption "Syncthing continuous file synchronization";

      extraOptions = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = [ "--gui-apikey=apiKey" ];
        description = ''
          Extra command-line arguments to pass to <command>syncthing</command>.
        '';
      };

      tray = mkOption {
        type =
          with types;
          either bool (submodule {
            options = {
              enable = mkOption {
                type = types.bool;
                default = false;
                description = "Whether to enable a syncthing tray service.";
              };

              command = mkOption {
                type = types.str;
                default = "syncthingtray";
                defaultText = literalExpression "syncthingtray";
                example = literalExpression "qsyncthingtray";
                description = "Syncthing tray command to use.";
              };

              package = mkOption {
                type = types.package;
                default = pkgs.syncthingtray-minimal;
                defaultText = literalExpression "pkgs.syncthingtray-minimal";
                example = literalExpression "pkgs.qsyncthingtray";
                description = "Syncthing tray package to use.";
              };
            };
          });
        default = {
          enable = false;
        };
        description = "Syncthing tray service configuration.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable { home.packages = [ (getOutput "man" pkgs.syncthing) ]; })

    (mkIf (cfg.enable && isLinux) {
      systemd.user.services = {
        syncthing = {
          Unit = {
            Description = "Syncthing - Open Source Continuous File Synchronization";
            Documentation = "man:syncthing(1)";
            After = [ "network.target" ];
          };

          Service = {
            ExecStart =
              "${pkgs.syncthing}/bin/syncthing -no-browser -no-restart -logflags=0"
              + optionalString (cfg.extraOptions != [ ]) (" " + escapeShellArgs cfg.extraOptions);
            Restart = "on-failure";
            SuccessExitStatus = [
              3
              4
            ];
            RestartForceExitStatus = [
              3
              4
            ];

            # Sandboxing.
            LockPersonality = true;
            MemoryDenyWriteExecute = true;
            NoNewPrivileges = true;
            PrivateUsers = true;
            RestrictNamespaces = true;
            SystemCallArchitectures = "native";
            SystemCallFilter = "@system-service";
          };

          Install = {
            WantedBy = [ "default.target" ];
          };
        };
      };
    })

    (mkIf (cfg.enable && isDarwin) {
      launchd.agents.syncthing =
        let
          logDir =
            if config.xdg.enable then config.xdg.cacheHome else "${config.home.homeDirectory}/Library/Logs";
        in
        {
          enable = true;
          config = {
            ProgramArguments = [
              "${pkgs.syncthing}/bin/syncthing"
              "--no-browser"
              "--no-restart"
              "--logflags=0"
            ] ++ (optionals (cfg.extraOptions != [ ]) (escapeShellArgs cfg.extraOptions));
            RunAtLoad = true;
            EnvironmentVariables = {
              HOME = config.home.homeDirectory;
              STNORESTART = "1";
            };
            KeepAlive.SuccessfulExit = false;
            LowPriorityIO = true;
            ProcessType = "Background";
            StandardOutPath = "${logDir}/syncthing.out.log";
            StandardErrorPath = "${logDir}/syncthing.err.log";
          };
        };
    })

    (mkIf (isAttrs cfg.tray && cfg.tray.enable) {
      assertions = [
        {
          # TODO: test
          assertion = isLinux;
          message = ''
            The 'services.syncthing.tray' option is not available on darwin.
          '';
        }
      ];
      systemd.user.services = {
        ${cfg.tray.package.pname} = {
          Unit = {
            Description = cfg.tray.package.pname;
            Requires = [ "tray.target" ];
            After = [
              "graphical-session-pre.target"
              "tray.target"
            ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${cfg.tray.package}/bin/${cfg.tray.command}";
          };

          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
        };
      };
    })

    # deprecated
    (mkIf (isBool cfg.tray && cfg.tray) {
      systemd.user.services = {
        "syncthingtray" = {
          Unit = {
            Description = "syncthingtray";
            Requires = [ "tray.target" ];
            After = [
              "graphical-session-pre.target"
              "tray.target"
            ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            ExecStart = "${pkgs.syncthingtray-minimal}/bin/syncthingtray";
          };

          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
        };
      };
      assertions = [
        {
          # TODO: test
          assertion = isLinux;
          message = ''
            The 'services.syncthing.tray' option is not available on darwin.
          '';
        }
      ];
      warnings = [
        "Specifying 'services.syncthing.tray' as a boolean is deprecated, set 'services.syncthing.tray.enable' instead. See https://github.com/nix-community/home-manager/pull/1257."
      ];
    })
  ];
}
# Local Variables:
# format-all-formatters: (("Nix" nixpkgs-fmt))
# End:
