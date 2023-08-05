{...}: {
  services.matrix-synapse.settings.listeners = [
    # FIXME: when this is active, error due to no resources
    #   stderr) error: The option `services.matrix-synapse.settings.listeners."[definition 1-entry 2]".resources' is used but not defined.
    {
      port = 8009;
      type = "metrics";
      bind_addresses = ["::1" "127.0.0.1"];
    }
  ];
}
