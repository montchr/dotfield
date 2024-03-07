{
  lib,
  stdenv,
  fetchurl,
  rpmextract,
  autoreconfHook,
  file,
  libjpeg,
  cups,
}:
let
  version = "1.0.0";
  release = "201212w";
  srcUrl = "https://download.ebz.epson.net/dsc/op/stable/SRPMS/epson-inkjet-printer-${release}-${version}-1lsb3.2.src.rpm";
in
stdenv.mkDerivation {
  pname = "epson_201212w";
  inherit version;

  src = fetchurl {
    # NOTE: Don't forget to update the webarchive link too!
    urls = [
      srcUrl
      "https://web.archive.org/web/${srcUrl}"
      # "https://download.ebz.epson.net/dsc/op/stable/RPMS/x86_64/epson-inkjet-printer-${release}-${version}-1lsb3.2.x86_64.rpm"
      # TODO: why is this url structure different?
      # "https://download.ebz.epson.net/dsc/op/stable/RPMS/x86_64/epson-inkjet-printer-${release}-${version}-1lsb3.2.x86_64.rpm"
      # "https://download.ebz.epson.net/dsc/op/stable/SRPMS/epson-inkjet-printer-201207w-${version}-1lsb3.2.src.rpm"
    ];
    sha256 = "sha256-gi1lnOHBUYOiV7Coi5Cz597UMHH7e5jLtdQzvm4pjgI=";
  };

  nativeBuildInputs = [
    rpmextract
    autoreconfHook
    file
  ];

  buildInputs = [
    libjpeg
    cups
  ];

  unpackPhase = ''
    rpmextract $src
    tar -zxf epson-inkjet-printer-${release}-${version}.tar.gz
    tar -zxf epson-inkjet-printer-filter-${version}.tar.gz
    for ppd in epson-inkjet-printer-${release}-${version}/ppds/*; do
      substituteInPlace $ppd --replace "/opt/epson-inkjet-printer-${release}" "$out"
      substituteInPlace $ppd --replace "/cups/lib" "/lib/cups"
    done
    cd epson-inkjet-printer-filter-${version}
  '';

  preConfigure = ''
    chmod +x configure
    export LDFLAGS="$LDFLAGS -Wl,--no-as-needed"
  '';

  postInstall = ''
    cd ../epson-inkjet-printer-${release}-${version}
    cp -a lib64 resource watermark $out
    mkdir -p $out/share/cups/model/epson-inkjet-printer-${release}
    cp -a ppds $out/share/cups/model/epson-inkjet-printer-${release}/
    cp -a Manual.txt $out/doc/
    cp -a README $out/doc/README.driver
  '';

  meta = with lib; {
    homepage = "https://www.openprinting.org/driver/epson-201212w";
    description = "Epson printer driver (WF-3010, WF-3520, WF-3530, WF-3540)";
    longDescription = ''
      This software is a filter program used with the Common UNIX Printing
      System (CUPS) under Linux. It supplies high quality printing with
      Seiko Epson Color Ink Jet Printers.

      List of printers supported by this package:
        Epson WF-3010 Series
        Epson WF-3520 Series
        Epson WF-3530 Series
        Epson WF-3540 Series

      To use the driver adjust your configuration.nix file:
        services.printing = {
          enable = true;
          drivers = [ pkgs.epson_201212w ];
        };
    '';

    license = with licenses; [
      lgpl21
      epson
    ];
    maintainers = [ maintainers.montchr ];
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
