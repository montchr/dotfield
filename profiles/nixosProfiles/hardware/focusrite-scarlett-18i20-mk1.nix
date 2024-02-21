##: Focusrite Scarlett 18i20 [1st Gen]
#
# Generation can be identified by serial number prefix;
# in this case, my device's serial number begins with "S".
#
# Source: <https://support.focusrite.com/hc/en-gb/articles/208295789-Which-generation-of-Scarlett-do-I-have>
# Archived source: <https://archive.ph/6T3gx>
#
##: Usage
#
# Unfortunately, it seems that ALSA will reset the device's monitor output
# channel to a muted state each time the device is disconnected.
#
# TODO: verify whether that is true for each boot
# FIXME: force monitor output unmuted by default?
#
# To toggle mute for the monitor channel:
#
# In `alsamixer`, switch to the device controls and look for "MM", which
# indicates mute is on. Toggle with "m". "00" state indicates an open channel.
# Although levels can be changed on this channel, this does not seem to have any
# effect on output volume, so just keep the level low.
#
# - <2024-02-20 Tue 19:33> alsa-scarlett-gui is a package providing support for
#   software-control of many
#   Focusrite devices' advanced but proprietary controls. Currently, the package
#   does not yet support Gen 1 devices, but the author indicates there is likely a
#   path forward and they only need some time to work on it. <https://github.com/geoffreybennett/alsa-scarlett-gui/issues/33>
#
# - <2024-02-20 Tue 19:27> Arch Wiki note in the pro audio article suggested
#   that some devices might not like USB 3 ports. This doesn't appear to be a
#   problem for this device.
#
# - <2024-02-20 Tue 19:21> The device works with no noticeable issues for casual
#   listening in GNOME. However, there is only very limited support in the GNOME
#   sound GUI (unsurprisingly).
#
#
##: Troubleshooting
#
### No sound
#
# - Make sure physical mute button is not activated
# - Check "USB Active" LED
# - Check alsamixer channel mute status
{
  imports = [../audio-pro.nix];

  # TODO:
  # musnix.soundcardPciId = "";
}
