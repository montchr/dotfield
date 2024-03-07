# FIXME: frequent crashes with GNOME:
# Jan 22 14:24:15 hodgepodge kernel: nouveau 0000:01:00.0: fifo: SCHED_ERROR 0a [CTXSW_TIMEOUT]
# Jan 22 14:24:15 hodgepodge kernel: nouveau 0000:01:00.0: fifo: runlist 0: scheduled for recovery
# Jan 22 14:24:15 hodgepodge kernel: nouveau 0000:01:00.0: fifo: channel 5: killed
# Jan 22 14:24:15 hodgepodge kernel: nouveau 0000:01:00.0: fifo: engine 0: scheduled for recovery
# Jan 22 14:24:15 hodgepodge .gnome-shell-wr[23980]: meta_wayland_buffer_process_damage: assertion 'buffer->resource' failed
#
# <https://gitlab.freedesktop.org/xorg/driver/xf86-video-nouveau/-/issues/339>
#
##: NVIDIA GeForce GT 750M Mac Edition (GK107M) (rev a1)
#
# Last supported NVIDIA driver is the 470.xx series.
# <https://www.nvidia.com/en-us/drivers/unix/legacy-gpu/>
#
# But the 470.xx drivers will reach EOL on <2024-07-20>, and GNOME 46 will drop
# support for these drivers.
#
# So I don't use those drivers, despite crashes with current drivers.
{ pkgs, ... }:
{
  # FIXME: unfortunately, this does not lead to great results
  #        -- UI is still far too small
  #
  # diag = w: h: sqrt(w^2 + h^2);
  # diagPx = diag 2880 1800;      => 3396.23320754
  # diagIn = 15;
  # ppi = diagPx / diagIn;        => 226.415547169
  #
  # services.xserver.dpi = 226;
}
