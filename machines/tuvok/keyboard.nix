{
  # By default, kanata captures all keyboard events, so most of the
  # configuration can be shared across every keyboard we use, by way of the
  # "default" configuration.  To prevent surprises, we specify devices
  # explicitly.
  services.kanata.keyboards."default".devices = [
    "/dev/input/by-path/platform-24eb30000.input-event-kbd"
    # Dell Smart Card Reader Keyboard KB813t (circa 2015)
    "/dev/input/by-id/usb-Dell_Dell_Smart_Card_Reader_Keyboard-event-kbd"
  ];
}
