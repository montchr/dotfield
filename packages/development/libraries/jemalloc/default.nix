# SPDX-FileCopyrightText Copyright (c) 2003-2022 Eelco Dolstra and the Nixpkgs/NixOS contributors
# SPDX-License-Identifier: MIT
#
##: Source:
#
# https://github.com/NixOS/nixpkgs/pull/193589
# https://github.com/divanorama/nixpkgs/blob/fix_jemalloc_aarch64_darwin/pkgs/development/libraries/jemalloc/default.nix
{
  lib,
  stdenv,
  fetchurl,
  fetchpatch,
  # By default, jemalloc puts a je_ prefix onto all its symbols on OSX, which
  # then stops downstream builds (mariadb in particular) from detecting it. This
  # option should remove the prefix and give us a working jemalloc.
  # Causes segfaults with some software (ex. rustc), but defaults to true for backward
  # compatibility.
  stripPrefix ? stdenv.hostPlatform.isDarwin,
  disableInitExecTls ? false,
}:
stdenv.mkDerivation rec {
  pname = "jemalloc";
  version = "5.3.0";

  src = fetchurl {
    url = "https://github.com/jemalloc/jemalloc/releases/download/${version}/${pname}-${version}.tar.bz2";
    hash = "sha256-LbgtHnEZ3z5xt2QCGbbf6EeJvAU3mDw7esT3GJrs/qo=";
  };

  patches = [
    # fix tests under --with-jemalloc-prefix=, see https://github.com/jemalloc/jemalloc/pull/2340
    (fetchpatch {
      url = "https://github.com/jemalloc/jemalloc/commit/d00ecee6a8dfa90afcb1bbc0858985c17bef6559.patch";
      hash = "sha256:11wp6byrhxi8ymqgknb40xx5xcwyj60ydajqh2029qc924ivi61p";
    })
  ];

  # see the comment on stripPrefix
  configureFlags =
    []
    ++ lib.optional stripPrefix "--with-jemalloc-prefix="
    ++ lib.optional disableInitExecTls "--disable-initial-exec-tls"
    # jemalloc is unable to correctly detect transparent hugepage support on
    # ARM (https://github.com/jemalloc/jemalloc/issues/526), and the default
    # kernel ARMv6/7 kernel does not enable it, so we explicitly disable support
    ++ lib.optionals (stdenv.isAarch32 && lib.versionOlder version "5") [
      "--disable-thp"
      "je_cv_thp=no"
    ]
    # AArch64 has configurable page size up to 64k. The default configuration
    # for jemalloc only supports 4k page sizes.
    ++ lib.optional stdenv.isAarch64 "--with-lg-page=16";

  NIX_CFLAGS_COMPILE = lib.optionalString stdenv.isDarwin "-Wno-error=array-bounds";

  doCheck = true;

  enableParallelBuilding = true;

  meta = with lib; {
    homepage = "http://jemalloc.net";
    description = "General purpose malloc(3) implementation";
    longDescription = ''
      malloc(3)-compatible memory allocator that emphasizes fragmentation
      avoidance and scalable concurrency support.
    '';
    license = licenses.bsd2;
    platforms = platforms.all;
  };
}
