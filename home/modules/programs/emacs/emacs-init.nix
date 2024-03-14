# Source: <https://git.sr.ht/~rycee/nur-expressions/tree/master/item/hm-modules>
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    flatten
    isBool
    mapAttrsToList
    mkOption
    optional
    types
    ;

  cfg = config.programs.emacs.init;

  packageFunctionType = lib.mkOptionType {
    name = "packageFunction";
    description = "function from epkgs to package";
    check = lib.isFunction;
    merge = lib.mergeOneOption;
  };

  usePackageType = types.submodule (
    { name, config, ... }:
    {
      options = {
        enable = lib.mkEnableOption "Emacs package ${name}";

        package = lib.mkOption {
          type = types.either (types.str // { description = "name of package"; }) packageFunctionType;
          default = name;
          description = ''
            The package to use for this module. Either the package name
            within the Emacs package set or a function taking the Emacs
            package set and returning a package.
          '';
        };

        defer = lib.mkOption {
          type = types.either types.bool types.ints.positive;
          default = false;
          description = ''
            The <option>:defer</option> setting.
          '';
        };

        defines = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:defines</option>.
          '';
        };

        demand = lib.mkOption {
          type = types.bool;
          default = false;
          description = ''
            The <option>:demand</option> setting.
          '';
        };

        diminish = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:diminish</option>.
          '';
        };

        chords = lib.mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "jj" = "ace-jump-char-mode";
            "jk" = "ace-jump-word-mode";
          };
          description = ''
            The entries to use for <option>:chords</option>.
          '';
        };

        functions = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:functions</option>.
          '';
        };

        mode = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:mode</option>.
          '';
        };

        after = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:after</option>.
          '';
        };

        bind = lib.mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "M-<up>" = "drag-stuff-up";
            "M-<down>" = "drag-stuff-down";
          };
          description = ''
            The entries to use for <option>:bind</option>.
          '';
        };

        bindLocal = lib.mkOption {
          type = types.attrsOf (types.attrsOf types.str);
          default = { };
          example = {
            helm-command-map = {
              "C-c h" = "helm-execute-persistent-action";
            };
          };
          description = ''
            The entries to use for local keymaps in <option>:bind</option>.
          '';
        };

        bindKeyMap = lib.mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "C-c p" = "projectile-command-map";
          };
          description = ''
            The entries to use for <option>:bind-keymap</option>.
          '';
        };

        command = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:commands</option>.
          '';
        };

        config = lib.mkOption {
          type = types.lines;
          default = "";
          description = ''
            Code to place in the <option>:config</option> section.
          '';
        };

        extraConfig = lib.mkOption {
          type = types.lines;
          default = "";
          description = ''
            Additional lines to place in the use-package configuration.
          '';
        };

        hook = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:hook</option>.
          '';
        };

        earlyInit = lib.mkOption {
          type = types.lines;
          default = "";
          description = ''
            Lines to add to <option>programs.emacs.init.earlyInit</option> when
            this package is enabled.
            </para><para>
            Note, the package is not automatically loaded so you will have to
            <literal>require</literal> the necessary features yourself.
          '';
        };

        init = lib.mkOption {
          type = types.lines;
          default = "";
          description = ''
            The entries to use for <option>:init</option>.
          '';
        };

        extraPackages = lib.mkOption {
          type = types.listOf types.package;
          default = [ ];
          description = ''
            Extra packages to add to <option>home.packages</option>.
          '';
        };

        assembly = lib.mkOption {
          type = types.lines;
          readOnly = true;
          internal = true;
          description = "The final use-package code.";
        };
      };

      config = lib.mkIf config.enable {
        assembly =
          let
            quoted = v: ''"${lib.escape [ ''"'' ] v}"'';
            mkBindHelper =
              cmd: prefix: bs:
              lib.optionals (bs != { }) (
                [ ":${cmd} (${prefix}" ] ++ lib.mapAttrsToList (n: v: "  (${quoted n} . ${v})") bs ++ [ ")" ]
              );

            mkAfter = vs: optional (vs != [ ]) ":after (${toString vs})";
            mkCommand = vs: optional (vs != [ ]) ":commands (${toString vs})";
            mkDefines = vs: optional (vs != [ ]) ":defines (${toString vs})";
            mkDiminish = vs: optional (vs != [ ]) ":diminish (${toString vs})";
            mkMode = map (v: ":mode ${v}");
            mkFunctions = vs: optional (vs != [ ]) ":functions (${toString vs})";
            mkBind = mkBindHelper "bind" "";
            mkBindLocal =
              bs:
              let
                mkMap = n: v: mkBindHelper "bind" ":map ${n}" v;
              in
              flatten (mapAttrsToList mkMap bs);
            mkBindKeyMap = mkBindHelper "bind-keymap" "";
            mkChords = mkBindHelper "chords" "";
            mkHook = map (v: ":hook ${v}");
            mkDefer = v: if isBool v then optional v ":defer t" else [ ":defer ${toString v}" ];
            mkDemand = v: optional v ":demand t";
          in
          lib.concatStringsSep "\n  " (
            [ "(use-package ${name}" ]
            ++ mkAfter config.after
            ++ mkBind config.bind
            ++ mkBindKeyMap config.bindKeyMap
            ++ mkBindLocal config.bindLocal
            ++ mkChords config.chords
            ++ mkCommand config.command
            ++ mkDefer config.defer
            ++ mkDefines config.defines
            ++ mkFunctions config.functions
            ++ mkDemand config.demand
            ++ mkDiminish config.diminish
            ++ mkHook config.hook
            ++ mkMode config.mode
            ++ lib.optionals (config.init != "") [
              ":init"
              config.init
            ]
            ++ lib.optionals (config.config != "") [
              ":config"
              config.config
            ]
            ++ optional (config.extraConfig != "") config.extraConfig
          )
          + ")";
      };
    }
  );

  # TODO: unused binding?
  # usePackageStr = name: pkgConfStr: ''
  #   (use-package ${name}
  #     ${pkgConfStr})
  # '';

  mkRecommendedOption =
    type: extraDescription:
    mkOption {
      type = types.bool;
      default = false;
      example = true;
      description =
        ''
          Whether to enable recommended ${type} settings.
        ''
        + lib.optionalString (extraDescription != "") ''
          </para><para>
          ${extraDescription}
        '';
    };

  # Recommended GC settings.
  gcSettings = ''
    (defun hm/reduce-gc ()
      "Reduce the frequency of garbage collection."
      (setq gc-cons-threshold most-positive-fixnum
            gc-cons-percentage 0.6))

    (defun hm/restore-gc ()
      "Restore the frequency of garbage collection."
      (setq gc-cons-threshold 16777216
            gc-cons-percentage 0.1))

    ;; Make GC more rare during init, while minibuffer is active, and
    ;; when shutting down. In the latter two cases we try doing the
    ;; reduction early in the hook.
    (hm/reduce-gc)
    (add-hook 'minibuffer-setup-hook #'hm/reduce-gc -50)
    (add-hook 'kill-emacs-hook #'hm/reduce-gc -50)

    ;; But make it more regular after startup and after closing minibuffer.
    (add-hook 'emacs-startup-hook #'hm/restore-gc)
    (add-hook 'minibuffer-exit-hook #'hm/restore-gc)

    ;; Avoid unnecessary regexp matching while loading .el files.
    (defvar hm/file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)

    (defun hm/restore-file-name-handler-alist ()
      "Restores the file-name-handler-alist variable."
      (setq file-name-handler-alist hm/file-name-handler-alist)
      (makunbound 'hm/file-name-handler-alist))

    (add-hook 'emacs-startup-hook #'hm/restore-file-name-handler-alist)
  '';

  # Whether the configuration makes use of `:diminish`.
  hasDiminish = lib.any (p: p.diminish != [ ]) (lib.attrValues cfg.usePackage);

  # Whether the configuration makes use of `:bind`.
  hasBind = lib.any (p: p.bind != { } || p.bindLocal != { } || p.bindKeyMap != { }) (
    lib.attrValues cfg.usePackage
  );

  # Whether the configuration makes use of `:chords`.
  hasChords = lib.any (p: p.chords != { }) (lib.attrValues cfg.usePackage);

  usePackageSetup =
    ''
      (eval-when-compile
        (require 'use-package)
        ;; To help fixing issues during startup.
        (setq use-package-verbose ${if cfg.usePackageVerbose then "t" else "nil"}))

    ''
    + lib.optionalString hasDiminish ''
      ;; For :diminish in (use-package).
      (require 'diminish)
    ''
    + lib.optionalString hasBind ''
      ;; For :bind in (use-package).
      (require 'bind-key)

      ;; Fixes "Symbol’s function definition is void: use-package-autoload-keymap".
      (autoload #'use-package-autoload-keymap "use-package-bind-key")
    ''
    + lib.optionalString hasChords ''
      ;; For :chords in (use-package).
      (use-package use-package-chords
        :config (key-chord-mode 1))
    '';

  earlyInitFile = ''
    ;;; hm-early-init.el --- Emacs configuration à la Home Manager -*- lexical-binding: t; -*-
    ;;
    ;;; Commentary:
    ;;
    ;; The early init component of the Home Manager Emacs configuration.
    ;;
    ;;; Code:

    ${cfg.earlyInit}

    (provide 'hm-early-init)
    ;; hm-early-init.el ends here
  '';

  initFile =
    ''
      ;;; hm-init.el --- Emacs configuration à la Home Manager -*- lexical-binding: t; -*-
      ;;
      ;;; Commentary:
      ;;
      ;; A configuration generated from a Nix based configuration by
      ;; Home Manager.
      ;;
      ;;; Code:

      ${lib.optionalString cfg.startupTimer ''
        (defun hm/print-startup-stats ()
          "Prints some basic startup statistics."
          (let ((elapsed (float-time (time-subtract after-init-time
                                                    before-init-time))))
            (message "Startup took %.2fs with %d GCs" elapsed gcs-done)))
        (add-hook 'emacs-startup-hook #'hm/print-startup-stats)
      ''}

      ${cfg.prelude}

      ${usePackageSetup}
    ''
    + lib.concatStringsSep "\n\n" (
      map (lib.getAttr "assembly") (lib.filter (lib.getAttr "enable") (lib.attrValues cfg.usePackage))
    )
    + ''

      ${cfg.postlude}

      (provide 'hm-init)
      ;; hm-init.el ends here
    '';
in
{
  imports = [ ./emacs-init-defaults.nix ];

  options.programs.emacs.init = {
    enable = lib.mkEnableOption "Emacs configuration";

    recommendedGcSettings = mkRecommendedOption "garbage collection" ''
      This will reduce garbage collection frequency during startup and
      while the minibuffer is active.
    '';

    startupTimer = lib.mkEnableOption "Emacs startup duration timer";

    earlyInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in <filename>early-init.el</filename>.
      '';
    };

    prelude = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in the beginning of
        <filename>init.el</filename>.
      '';
    };

    postlude = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in the end of
        <filename>init.el</filename>.
      '';
    };

    packageQuickstart = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to enable package-quickstart. This will make sure that
        <literal>package.el</literal> is activated and all autoloads are
        available.
        </para><para>
        If disabled you can save quite a few milliseconds on the startup time,
        but you will most likely have to tweak the <literal>command</literal>
        option of various packages.
        </para><para>
        As an example, running <literal>(emacs-init-time)</literal> on an Emacs
        configuration with this option enabled reported ~300ms. Disabling the
        option dropped the init time to ~200ms.
      '';
    };

    usePackageVerbose = lib.mkEnableOption "verbose use-package mode";

    usePackage = mkOption {
      type = types.attrsOf usePackageType;
      default = { };
      example = lib.literalExpression ''
        {
          dhall-mode = {
            mode = [ '''"\\.dhall\\'"''' ];
          };
        }
      '';
      description = ''
        Attribute set of use-package configurations.
      '';
    };
  };

  config = lib.mkIf (config.programs.emacs.enable && cfg.enable) {
    # Collect the extra packages that should be included in the user profile.
    # These are typically tools called by Emacs packages.
    home.packages = lib.concatMap (v: v.extraPackages) (
      lib.filter (lib.getAttr "enable") (builtins.attrValues cfg.usePackage)
    );

    programs.emacs.init.earlyInit =
      let
        standardEarlyInit = lib.mkBefore ''
          ${lib.optionalString cfg.recommendedGcSettings gcSettings}

          ${
            if cfg.packageQuickstart then
              ''
                (setq package-quickstart t
                      package-quickstart-file "hm-package-quickstart.el")
              ''
            else
              ''
                (setq package-enable-at-startup nil)
              ''
          }

          ;; Avoid expensive frame resizing. Inspired by Doom Emacs.
          (setq frame-inhibit-implied-resize t)
        '';

        # Collect the early initialization strings for each package.
        packageEarlyInits = map (p: p.earlyInit) (
          lib.filter (p: p.earlyInit != "") (builtins.attrValues cfg.usePackage)
        );
      in
      lib.mkMerge ([ standardEarlyInit ] ++ packageEarlyInits);

    programs.emacs.extraPackages =
      epkgs:
      let
        getPkg =
          v:
          if lib.isFunction v then
            [ (v epkgs) ]
          else
            optional (lib.isString v && lib.hasAttr v epkgs) epkgs.${v};

        packages = lib.concatMap (v: getPkg v.package) (
          lib.filter (lib.getAttr "enable") (builtins.attrValues cfg.usePackage)
        );
      in
      [
        (epkgs.trivialBuild {
          pname = "hm-early-init";
          src = pkgs.writeText "hm-early-init.el" earlyInitFile;
          version = "0.1.0";
          packageRequires = packages;
          preferLocalBuild = true;
          allowSubstitutes = false;
        })

        (epkgs.trivialBuild {
          pname = "hm-init";
          src = pkgs.writeText "hm-init.el" initFile;
          version = "0.1.0";
          packageRequires =
            [ epkgs.use-package ]
            ++ packages
            ++ optional hasBind epkgs.bind-key
            ++ optional hasDiminish epkgs.diminish
            ++ optional hasChords epkgs.use-package-chords;
          preferLocalBuild = true;
          allowSubstitutes = false;
          preBuild = ''
            # Do a bit of basic formatting of the generated init file.
            emacs -Q --batch \
              --eval '(find-file "hm-init.el")' \
              --eval '(let ((indent-tabs-mode nil) (lisp-indent-offset 2)) (indent-region (point-min) (point-max)))' \
              --eval '(write-file "hm-init.el")'

            ${lib.optionalString cfg.packageQuickstart ''
              # Generate a package quickstart file to make autoloads and such available.
              emacs -Q --batch \
                --eval "(require 'package)" \
                --eval "(setq package-quickstart-file \"hm-package-quickstart.el\")" \
                --eval "(package-quickstart-refresh)"

              # We know what we're doing?
              sed -i '/no-byte-compile: t/d' hm-package-quickstart.el
            ''}
          '';
        })
      ];

    xdg.configFile = {
      "emacs/dotfield-early-init.el".text = ''
        (require 'hm-early-init)
        (provide 'dotfield-early-init)
      '';

      "emacs/dotfield-init.el".text = ''
        (require 'hm-init)
        (provide 'dotfield-init)
      '';
    };
  };
}
