###: EPSON WF-3520
# Manufacturer: 0x04b8
# Device: 0x0899
#
##: Printing:
#
# <https://www.openprinting.org/printer/Epson/Epson-WF-3520_Series>
#
# No additional configuration or packages required.
# Not supported by Gutenprint.
#
# Current settings (with non-default: one-sided and reverse):
# copies=1 device-uri=dnssd://EPSON%20WF-3520%20Series._ipp._tcp.local/?uuid=7c47f900-67ff-11d4-9a7f-b0e89208cb87 finishings=3 job-cancel-after=10800 job-hold-until=no-hold job-priority=50 job-sheets=confidential,none marker-change-time=0 number-up=1 outputorder=reverse print-color-mode=color printer-commands=none printer-info='EPSON WF-3520 Series' printer-is-accepting-jobs=true printer-is-shared=false printer-is-temporary=false printer-location='Labortatory 1' printer-make-and-model='Epson WF-3520 Series - epson-inkjet-printer 1.0.0-1lsb3.2 (Seiko Epson Corporation LSB 3.2)' printer-state=3 printer-state-change-time=1708968150 printer-state-reasons=none printer-type=2273308 printer-uri-supported=ipp://localhost/printers/LABORTTY sides=one-sided
#
##: Scanning:
#
# <http://www.sane-project.org/man/sane-epson2.5.html>
#
# - "network interface supported via DFSG non-free iscan-network-nt package"
# - "overseas version of the PX-605F"
#
# Requires proprietary scanning driver from EPSON:
#
# <http://support.epson.net/linux/en/iscan_c.php?version=2.30.4>
#
# This software is known as "iscan" which is part of the "Image Scan!" bundle.
# The SANE reference site and Nixpkgs refer to this bundle as "epkowa", though I
# am not sure where the name comes from. Regardless, `epkowa` contains `iscan`
# and the `iscan-network-nt` plugin mentioned by the SANE reference site.
#
# Additional configuration is also required to tell epkowa how to connect to the
# scanner over the network. It took a while for me to figure this part out. See
# the `epkowa.conf` text file derivation in `hardware.sane.extraBackends`. The
# NixOS module does not provide a way to write such a file directly.
#
# The EPSON manual has a section about this step, which is pretty specific actually:
# <http://download.ebz.epson.net/man/linux/iscan_e.html#sec8-3f>
# I am not sure whether specifying the port 1865 is essential, but the manual
# suggests providing it would be a good idea (the wording is unclear to me).
#
# And this comment provides a direct example in the context of `extraBackends`:
# <https://discourse.nixos.org/t/l355-epson-wifi-scanner/5543/9>
#
# Otherwise, the scanner is incompatible with Apple AirPlay, and thus
# incompatible with `sane-airscan`:
#
# <https://support.apple.com/en-us/HT201311>
# <https://github.com/alexpevzner/sane-airscan/blob/master/README.md#compatibility>
#
#     pkgs.utsushi only supports usb/scsi
#
# Network scanning confirmed not functioning with:
# - sane-backends (default)
# - sane-airscan
# - utsushi (usb/scsi only)
#
# I have not tested USB scanning but I'm sure it's much more straightforward.
{ lib, pkgs, ... }:
let
  ip = "192.168.1.192";
in
{
  imports = [ ./common.nix ];

  nixpkgs.config.allowlistedLicenses = [ lib.licenses.epson ];

  hardware.sane.extraBackends = [
    pkgs.epkowa

    (pkgs.writeTextFile {
      name = "epkowa.conf";
      # TODO: verify how much of this is necessary?
      text = ''
        net ${ip} 1865
      '';
      destination = "/etc/sane.d/epkowa.conf";
    })
  ];

  # FIXME: remove? what happens if the port is provided? is the extra conf file
  # still needed?
  hardware.sane.netConf = ''
    ${ip}
  '';
}
