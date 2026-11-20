# SPDX-FileCopyrightText: 2024-2025 Temple University <kleinweb@temple.edu>
# SPDX-License-Identifier: GPL-3.0-or-later

# The package on its own only produces a patched binary tree in the Nix
# store.  The `NSGClient` binary hardcodes absolute `/opt/Citrix/...`
# paths and needs three pieces of system integration that a package
# cannot provide:
#
#   1. CAP_NET_RAW on the binary  — it opens raw sockets for the VPN data path.
#   2. The `nsgverctl` system service — privileged route/nftables setup.
#   3. The `/opt/Citrix` tree on disk — because the paths are not relocatable.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.citrix-secure-access;

  optTree = "${cfg.package}/opt/Citrix";

  readOnlyEntries = [
    "resx"
    "service"
    "libAnalyticsInterface.so"
    "libsentry.so"
    "crashpad_handler"
    "pubkey.asc"
    "citrix_va.conf"
    "rt_csa.conf"
    "globalConfiguration.conf"
    "userConfiguration.conf"
    "bin/nsgclient.desktop"
    "bin/nsgtsb.sh"
    "bin/startnsgclient.sh"
  ];

  mkCopy = entry: "C /opt/Citrix/NSGClient/${entry} 0644 root root - ${optTree}/NSGClient/${entry}";
  mkSymlink = entry: "L+ /opt/Citrix/NSGClient/${entry} - - - - ${optTree}/NSGClient/${entry}";

  staticTreeRules = [
    "d /opt/Citrix 0755 root root -"
    "d /opt/Citrix/NSGClient 0755 root root -"
    "d /opt/Citrix/NSGClient/bin 0755 root root -"
    # The EPA (Endpoint Analysis) library is dlopen()ed by absolute path.
    "L+ /opt/Citrix/EPA - - - - ${optTree}/EPA"
    # `nsgverctl` authenticates each packet by reading the sender's
    # `/proc/<pid>/exe` and comparing it against this exact path, so the client
    # must *be* this file rather than a symlink to it -- `exe` resolves through
    # symlinks and would report the store path, failing the check ("Path match
    # fail. Packet received from unauthentic NSGClient") and leaving the
    # command silently unanswered.  A copy carries CAP_NET_RAW, which cannot be
    # set on the read-only store path.
    # `r` first: earlier generations left a symlink here, and `C+` will not
    # replace a symlink with a regular file.
    "r /opt/Citrix/NSGClient/bin/NSGClient"
    "C+ /opt/Citrix/NSGClient/bin/NSGClient 0755 root root - ${cfg.package}/libexec/NSGClient.unpatched"
  ]
  ++ map mkSymlink readOnlyEntries;

  # ----------------------------------------------------------------------
  # TODO(learning): define `runtimeStateRules` — the writable parts of the tree.
  #
  # Three paths CANNOT be store symlinks, because the running client (or the
  # root `nsgverctl` service) writes to them:
  #
  #   /opt/Citrix/NSGClient/.socketpath     AF_UNIX datagram socket that
  #                                         nsgverctl binds. Must NOT be
  #                                         pre-created -- see the rule below.
  #   /opt/Citrix/NSGClient/nft_commands.txt  nftables ruleset scratch file
  #                                           written by nsgverctl. 0644.
  #   /opt/Citrix/NSGClient/tmp             scratch *directory* the service
  #                                         expects. 0755.
  #
  # And one genuine design decision — `globalConfiguration.json`:
  #   - The upstream installer MERGES the previous file into the new one, which
  #     implies admins are expected to tune it and have edits survive upgrades.
  #   - Option A: `C /opt/.../globalConfiguration.json 0644 root root - SOURCE`
  #     copies the package's copy ONCE; later edits persist, but a package
  #     upgrade will NOT refresh it (state drifts from the Nix store).
  #   - Option B: `L+ ...` symlink — fully reproducible and always matches the
  #     package, but the client cannot persist any change to it.
  #
  # systemd-tmpfiles line types you need:
  #   d  PATH MODE UID GID AGE        create a directory
  #   f  PATH MODE UID GID - -        create an empty file if absent
  #   C  PATH MODE UID GID - SOURCE   copy SOURCE to PATH if absent
  #   L+ PATH - - - - TARGET          create a symlink, replacing what's there
  #
  # `${optTree}/NSGClient/globalConfiguration.json` is the package's copy.
  # Implement the list below (~5 lines): the three writable paths, plus the
  # globalConfiguration.json rule for whichever option fits this deployment.
  runtimeStateRules = [
    # nsgverctl bind(2)s this path itself; bind fails EADDRINUSE if anything
    # already exists there, so ensure it is absent rather than pre-creating it.
    "r /opt/Citrix/NSGClient/.socketpath"
    "f /opt/Citrix/NSGClient/nft_commands.txt 0644 root root - -"
    "d /opt/Citrix/NSGClient/tmp 0755 root root -"
    (mkCopy "globalConfiguration.json")
  ];

  # The deployed client is the stock binary, so nothing wraps it -- but it
  # still needs a GIO TLS backend or every gateway connection fails with "TLS
  # support is not available".  `exec` keeps `/proc/<pid>/exe` pointing at the
  # FHS path, which `nsgverctl` requires (see the tmpfiles rule above).
  launcher = pkgs.writeShellScriptBin "NSGClient" ''
    export GIO_EXTRA_MODULES="${pkgs.glib-networking}/lib/gio/modules''${GIO_EXTRA_MODULES:+:$GIO_EXTRA_MODULES}"
    # The client `dlopen`s libcurl by bare soname at runtime.  nix-ld resolves
    # `DT_NEEDED` entries only, so a runtime dlopen searches the ordinary path
    # and finds nothing -- sends then fail, and the client's error path logs an
    # int through a `%s` and segfaults.
    export LD_LIBRARY_PATH="${lib.makeLibraryPath cfg.package.runtimeLibraries}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    exec /opt/Citrix/NSGClient/bin/NSGClient "$@"
  '';

in
{
  options.services.citrix-secure-access = {
    enable = lib.mkEnableOption "the Citrix Secure Access VPN client";

    package = lib.mkOption {
      type = lib.types.package;
      defaultText = lib.literalMD "`packages.citrix-secure-access` from the beams flake";
      description = "The citrix-secure-access package to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    # `launcher` first: it must win the `NSGClient` name over the package's
    # own store-path wrapper, which `nsgverctl` would reject.
    environment.systemPackages = [
      launcher
      cfg.package
    ];

    # The deployed client is the stock, unpatched binary (see the tmpfiles rule
    # above), so it resolves its libraries the FHS way.  Contribute them to the
    # nix-ld pool rather than enabling nix-ld here -- that stays the user's
    # choice, and without it this client cannot run.
    programs.nix-ld.libraries = cfg.package.runtimeLibraries;

    # No CAP_NET_RAW on the client.  A file capability puts the loader into
    # secure-execution mode, which drops LD_LIBRARY_PATH -- and the stock,
    # unpatched binary carries no RPATH, so it then cannot find its libraries
    # at all.  The data path does not need it: with no capability set,
    # tunnelled TCP connections complete and negotiate TLS.

    # Privileged daemon: route/nftables/DNS plumbing for the tunnel.
    systemd.services.nsgverctl = {
      description = "Citrix NSG Version Control Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      # The daemon shells out to route/firewall tooling at runtime.
      path = with pkgs; [
        # `sha256sum`, which the daemon runs (by bare name) to checksum the
        # client binary when validating the sender of each packet.
        coreutils
        iproute2
        nftables
        procps
        # `systemctl`, which the daemon calls when restarting the client.
        systemd
      ];
      serviceConfig = {
        # tmpfiles ordering against this unit is not guaranteed, so clear a
        # stale socket here too. The leading `-` tolerates an absent path.
        ExecStartPre = "-${pkgs.coreutils}/bin/rm -f /opt/Citrix/NSGClient/.socketpath";
        ExecStart = "${optTree}/NSGClient/service/nsgverctl";
        Restart = "always";
        KillMode = "process";
      };
    };

    # The tunnel interface (`Citrix_VA`) must be left alone by NetworkManager,
    # and the client needs its dedicated routing-table id registered.
    networking.networkmanager.enable = lib.mkDefault true;
    environment.etc."NetworkManager/conf.d/citrix_va.conf".source =
      "${optTree}/NSGClient/citrix_va.conf";
    environment.etc."iproute2/rt_tables.d/rt_csa.conf".source = "${optTree}/NSGClient/rt_csa.conf";

    # Materialise the hardcoded /opt/Citrix tree.
    systemd.tmpfiles.rules = staticTreeRules ++ runtimeStateRules;
  };
}
