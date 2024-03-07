{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    concatStringsSep
    escapeShellArg
    escapeShellArgs
    generators
    getBin
    getVersion
    hiPrio
    literalExpression
    maintainers
    mkEnableOption
    mkIf
    mkMerge
    mkOption
    optional
    optionalAttrs
    optionalString
    types
    versionAtLeast
    ;

  cfg = config.services.emacs;
  emacsCfg = config.programs.emacs;
  emacsBinPath = "${cfg.package}/bin";
  emacsVersion = getVersion cfg.package;

  clientWMClass = if versionAtLeast emacsVersion "28" then "Emacsd" else "Emacs";

  # Adapted from upstream emacs.desktop
  clientDesktopItem = pkgs.writeTextDir "share/applications/emacsclient.desktop" (
    generators.toINI { } {
      "Desktop Entry" = {
        Type = "Application";
        Exec = "${emacsBinPath}/emacsclient ${concatStringsSep " " cfg.client.arguments} %F";
        Terminal = false;
        Name = "Emacs Client";
        Icon = "emacs";
        Comment = "Edit text";
        GenericName = "Text Editor";
        MimeType = "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;";
        Categories = "Development;TextEditor;";
        Keywords = "Text;Editor;";
        StartupWMClass = clientWMClass;
      };
    }
  );

  # Match the default socket path for the Emacs version so emacsclient continues
  # to work without wrapping it.
  socketDir = "%t/emacs";
  socketPath = "${socketDir}/server";

  # We wrap the start command in a login shell so Emacs starts with the user's
  # environment, most importantly $PATH and $NIX_PROFILES. It may be
  # worth investigating a more targeted approach for user services to
  # import the user environment.
  daemonCmdParts = [
    "${emacsBinPath}/emacs"
    (
      "--fg-daemon"
      # In case the user sets 'server-directory' or 'server-name' in
      # their Emacs config, we want to specify the socket path explicitly
      # so launching 'emacs.service' manually doesn't break emacsclient
      # when using socket activation.
      + optionalString cfg.socketActivation.enable "=${escapeShellArg socketPath}"
    )
    "${escapeShellArgs cfg.extraOptions}"
  ];
  execCmdParts = [
    pkgs.runtimeShell
    "-l"
    "-c"
  ];
in
{
  # Disable the upstream module.
  disabledModules = [ "services/emacs.nix" ];

  meta.maintainers = [
    maintainers.tadfisher
    maintainers.montchr
  ];

  options.services.emacs = {
    enable = mkEnableOption "the Emacs daemon";

    package = mkOption {
      type = types.package;
      default = if emacsCfg.enable then emacsCfg.finalPackage else pkgs.emacs;
      defaultText = literalExpression ''
        if config.programs.emacs.enable then config.programs.emacs.finalPackage
        else pkgs.emacs
      '';
      description = "The Emacs package to use.";
    };

    extraOptions = mkOption {
      type = with types; listOf str;
      default = [ ];
      example = [
        "-f"
        "exwm-enable"
      ];
      description = ''
        Extra command-line arguments to pass to <command>emacs</command>.
      '';
    };

    client = {
      enable = mkEnableOption "generation of Emacs client desktop file";
      arguments = mkOption {
        type = with types; listOf str;
        default = [ "-c" ];
        description = ''
          Command-line arguments to pass to <command>emacsclient</command>.
        '';
      };
    };

    # Attrset for forward-compatibility; there may be a need to customize the
    # socket path, though allowing for such is not easy to do as systemd socket
    # units don't perform variable expansion for 'ListenStream'.
    socketActivation = {
      enable = mkEnableOption "systemd socket activation for the Emacs service";
    };

    startWithUserSession = mkOption {
      type = with types; either bool (enum [ "graphical" ]);
      default = !cfg.socketActivation.enable;
      defaultText = literalExpression "!config.services.emacs.socketActivation.enable";
      example = "graphical";
      description = ''
        Whether to launch Emacs service with the systemd user session. If it is
        <literal>true</literal>, Emacs service is started by
        <literal>default.target</literal>. If it is
        <literal>"graphical"</literal>, Emacs service is started by
        <literal>graphical-session.target</literal>.
      '';
    };

    defaultEditor = mkOption rec {
      type = types.bool;
      default = false;
      example = !default;
      description = ''
        Whether to configure <command>emacsclient</command> as the default
        editor using the <envar>EDITOR</envar> environment variable.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      launchd.agents.emacs = {
        enable = true;
        config.KeepAlive.SuccessfulExit = true;
        config.ProgramArguments = execCmdParts ++ daemonCmdParts;
        config.RunAtLoad = true;
      };
    }
    {
      systemd.user.services.emacs =
        {
          Unit =
            {
              Description = "Emacs text editor";
              Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";

              After = optional (cfg.startWithUserSession == "graphical") "graphical-session.target";
              PartOf = optional (cfg.startWithUserSession == "graphical") "graphical-session.target";

              # Avoid killing the Emacs session, which may be full of
              # unsaved buffers.
              X-RestartIfChanged = false;
            }
            // optionalAttrs (cfg.socketActivation.enable) {
              # Emacs deletes its socket when shutting down, which systemd doesn't
              # handle, resulting in a server without a socket.
              # See https://github.com/nix-community/home-manager/issues/2018
              RefuseManualStart = true;
            };

          Service =
            {
              Type = "notify";

              ExecStart = ''
                ${concatStringsSep " " execCmdParts} \
                  "${concatStringsSep " " daemonCmdParts}"
              '';

              # Emacs will exit with status 15 after having received SIGTERM, which
              # is the default "KillSignal" value systemd uses to stop services.
              SuccessExitStatus = 15;

              Restart = "on-failure";
            }
            // optionalAttrs (cfg.socketActivation.enable) {
              # Use read-only directory permissions to prevent emacs from
              # deleting systemd's socket file before exiting.
              ExecStartPost = "${pkgs.coreutils}/bin/chmod --changes -w ${socketDir}";
              ExecStopPost = "${pkgs.coreutils}/bin/chmod --changes +w ${socketDir}";
            };
        }
        // optionalAttrs (cfg.startWithUserSession != false) {
          Install = {
            WantedBy = [
              (if cfg.startWithUserSession == true then "default.target" else "graphical-session.target")
            ];
          };
        };
    }

    {
      home.sessionVariables = mkIf cfg.defaultEditor {
        EDITOR = getBin (
          pkgs.writeShellScript "editor" ''
            exec ${getBin cfg.package}/bin/emacsclient "''${@:---create-frame}"
          ''
        );
      };
    }

    (mkIf cfg.client.enable {
      assertions = [
        (lib.hm.assertions.assertPlatform "services.emacs.client.enable" pkgs lib.platforms.linux)
      ];

      home.packages = optional cfg.client.enable (hiPrio clientDesktopItem);
    })

    (mkIf cfg.socketActivation.enable {
      assertions = [
        (lib.hm.assertions.assertPlatform "services.emacs.socketActivation" pkgs lib.platforms.linux)
      ];

      systemd.user.sockets.emacs = {
        Unit = {
          Description = "Emacs text editor";
          Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
        };

        Socket = {
          ListenStream = socketPath;
          FileDescriptorName = "server";
          SocketMode = "0600";
          DirectoryMode = "0700";
        };

        Install = {
          WantedBy = [ "sockets.target" ];
        };
      };
    })
  ]);
}
