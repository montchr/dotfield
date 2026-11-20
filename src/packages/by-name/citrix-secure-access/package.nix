# SPDX-FileCopyrightText: 2024-2026 Temple University <kleinweb@temple.edu>
# SPDX-License-Identifier: GPL-3.0-or-later

# The `NSGClient` binary hardcodes absolute `/opt/Citrix/...` paths for its
# resources, config files and the IPC socket-path file, so it only *runs*
# correctly when the companion NixOS module (`./module.nix`) has materialised
# that tree. Building the package alone is not enough.
{
  lib,
  stdenv,
  fetchurl,
  requireFile,
  dpkg,
  autoPatchelfHook,
  icoutils,
  wrapGAppsHook3,
  makeWrapper,

  # Runtime libraries — the union of the `DT_NEEDED` entries of NSGClient,
  # service/nsgverctl and EPA/libepalib.so (see `patchelf --print-needed`).
  curl,
  dconf,
  glib,
  glib-networking,
  gpgme,
  gsettings-desktop-schemas,
  gtk3,
  libarchive,
  libayatana-appindicator,
  libnl,
  libnotify,
  libproxy,
  libuuid,
  libx11,
  # libxml2 2.14 bumped its soname to libxml2.so.16; NSGClient is linked
  # against the old libxml2.so.2, so we need the pinned 2.13 series.
  libxml2_13,
  libxscrnsaver,
  networkmanager,
  openssl,
  procps,
  pugixml,
  systemd,
  webkitgtk_4_1,
}:

let
  # NSGClient and nsgverctl are linked against `libgpgme.so.11` (gpgme 1.x).
  # nixpkgs has moved to gpgme 2.x, which bumped the soname to `.so.45` and
  # left no pinned 1.x attribute behind. Override back to the last 1.x release.
  gpgme1 = gpgme.overrideAttrs (_old: rec {
    version = "1.24.3";
    src = fetchurl {
      url = "mirror://gnupg/gpgme/gpgme-${version}.tar.bz2";
      sha256 = "1pahikkdrv6d1b22ssh0vwrhy23ps9b8k4fxkxjchy5is5dpzhdz";
    };
  });
in
stdenv.mkDerivation (finalAttrs: {
  pname = "citrix-secure-access";
  version = "25.8.2";

  src = requireFile {
    name = "nsginstaller64.deb";
    sha256 = "1cq95i3i3bd67aknwxz4bdkqfj288dp0fvsa80w7g166jma0klsn";
    message = ''
      Citrix Secure Access is distributed under the Citrix EULA and cannot be
      downloaded automatically. Obtain the "Citrix Secure Access client for
      Ubuntu" .deb (named `nsginstaller64.deb`) from:

        https://www.citrix.com/downloads/citrix-secure-access/plug-ins/Citrix-Gateway-VPN-EPA-Clients-Ubuntu.html

      then add it to the Nix store with:

        nix-prefetch-url file://$PWD/nsginstaller64.deb
    '';
  };

  unpackCmd = "dpkg-deb -x $curSrc source";
  sourceRoot = "source";

  dontConfigure = true;
  dontBuild = true;

  # wrapGAppsHook3 wraps $out/bin during postFixup, which would re-wrap the
  # wrapper built below and drop its PATH prefix. Take the hook's arguments
  # via `gappsWrapperArgs` and apply them in a single wrapper instead.
  dontWrapGApps = true;

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    icoutils
    makeWrapper
    wrapGAppsHook3
  ];

  buildInputs = [
    curl
    dconf
    glib
    glib-networking
    gpgme1
    gsettings-desktop-schemas
    gtk3
    libarchive
    libayatana-appindicator
    libnl
    libnotify
    libproxy
    libuuid
    (lib.getLib libxml2_13) # libxml2's default output is `bin` (no .so)
    (lib.getLib networkmanager) # provides libnm.so.0
    openssl
    # nixpkgs builds pugixml as a static archive by default; the client needs
    # the shared libpugixml.so.1.
    (pugixml.override { shared = true; })
    stdenv.cc.cc # libstdc++ / libgcc_s
    webkitgtk_4_1
    libx11
    libxscrnsaver # libXss.so.1
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/libexec $out/share/applications
    cp -r opt $out/opt


    # The tray icons ship only as `.ico`, which GTK4
    # `Texture::from_filename()` cannot decode, so GTK4 bars (anything
    # not going through gdk-pixbuf) render an invisible tray slot.
    for ico in opt/Citrix/NSGClient/resx/images/*.ico; do
      name="$(basename "$ico" .ico)"
      icotool --extract --output="$TMPDIR" "$ico"
      for png in "$TMPDIR/$name"_*.png; do
        # icotool names extracts `<name>_<index>_<w>x<h>x<depth>.png`.
        dim="''${png##*_}"
        dim="''${dim%x*}"
        install -Dm644 "$png" \
          "$out/share/icons/hicolor/$dim/apps/$name.png"
      done

      # Also alongside the original, since the client reports `resx/images`
      # as its `IconThemePath` and a tray may look there rather than in the
      # system theme. Keeps `resx` a self-contained tree the module can
      # symlink wholesale.
      install -Dm644 "$TMPDIR/$name"_*_48x48x*.png \
        "$out/opt/Citrix/NSGClient/resx/images/$name.png"

      rm -f "$TMPDIR/$name"_*.png
    done

    # Desktop entry: strip the hardcoded /opt path so it launches `NSGClient`
    # from PATH (which, on a NixOS host with the module, resolves to the
    # capability wrapper in /run/wrappers/bin first).
    install -Dm644 opt/Citrix/NSGClient/bin/nsgclient.desktop \
      $out/share/applications/citrix-secure-access.desktop
    substituteInPlace $out/share/applications/citrix-secure-access.desktop \
      --replace-fail "/opt/Citrix/NSGClient/bin/NSGClient" "NSGClient" \
      --replace-fail \
        "Icon=/opt/Citrix/NSGClient/resx/images/icon_vpn.ico" \
        "Icon=icon_vpn"

    runHook postInstall
  '';

  # `gappsWrapperArgs` is populated by wrapGAppsHook3 during preFixup, so the
  # wrapper has to be built here rather than in installPhase -- otherwise the
  # array is still empty and the GTK/pixbuf/schema settings are lost.
  #
  # `$out/bin/NSGClient` is a convenience entry point. The NixOS module
  # re-wraps it through `security.wrappers` to grant CAP_NET_RAW; the cap
  # propagates through this wrapper via ambient capabilities.
  # Must run after fixupPhase: autoPatchelfHook rewrites every ELF under $out,
  # which would change the hash this copy exists to preserve.
  preservePristinePhase = ''
    # `nsgverctl` validates the client by sha256-ing whatever sits at
    # /opt/Citrix/NSGClient/bin/NSGClient and comparing it against a hash of
    # the stock binary baked into the service.  Re-extract the original from
    # the .deb so the module has an untouched copy to deploy; it runs under
    # nix-ld using the libraries the module contributes to that pool.
    dpkg-deb --fsys-tarfile $src \
      | tar -xO ./opt/Citrix/NSGClient/bin/NSGClient \
      > $out/libexec/NSGClient.unpatched
    chmod 0755 $out/libexec/NSGClient.unpatched
  '';

  postPhases = [ "preservePristinePhase" ];

  postFixup = ''
    # Deliberately NOT named `NSGClient`: `nsgverctl` rejects any client whose
    # `/proc/<pid>/exe` is not /opt/Citrix/NSGClient/bin/NSGClient, so this
    # wrapper cannot establish a tunnel.  It stays for debugging the patched
    # binary; the module ships the launcher that actually works.
    makeWrapper $out/opt/Citrix/NSGClient/bin/NSGClient $out/bin/NSGClient-patched \
      --inherit-argv0 \
      --set-default XDG_DATA_DIRS /usr/local/share/:/usr/share/ \
      "''${gappsWrapperArgs[@]}" \
      --prefix PATH : "${
        lib.makeBinPath [
          dpkg
          procps
          # `resolvectl`, which the client calls to flush the DNS cache after
          # applying the gateway's split-DNS configuration.
          systemd
        ]
      }"
  '';

  # The module deploys the stock, unpatched client (see `libexec/` above), which
  # resolves its libraries the FHS way.  Expose them so the module can hand them
  # to nix-ld without restating -- and drifting from -- this list.
  passthru.runtimeLibraries = finalAttrs.buildInputs;

  meta = {
    description = "Citrix Secure Access client (NetScaler Gateway VPN) for Linux";
    homepage = "https://www.citrix.com/downloads/citrix-gateway/";
    license = lib.licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
    mainProgram = "NSGClient";
  };
})
