# Loop Garden Matrix Homeserver

## Tasks

- [ ] FIXME configure email via gabbro

## Email Configuration

- <https://matrix-org.github.io/synapse/latest/usage/configuration/config_documentation.html?highlight=smtp#email>

The Synapse configuration format does not provide a way to supply paths to most
secret values. The result of this is the need to include secrets by appending
separate configuration files together. That is just fine if the configuration
value lives at the top level -- only that single value needs to be included
separately. However, assumedly due to the limitations of YAML, it's not possible
to set a single _nested_ property without also re-defining the entirety of that
tree inside the encrypted configuration.

So, with that in mind, here are the non-secret parts of configuration that are
hidden away inside the encrypted files.

```yaml
email:
smtp_host: mail.loop.garden
smtp_pass: *********
require_transport_security: true
enable_true: true
notif_from: "%(app)s <notifications@matrix.loop.garden>"
app_name: "loop.garden"
invite_client_location: "https://chat.loop.garden"
```

## Sources

- <https://nixos.org/manual/nixos/stable/index.html#module-services-matrix-synapse>
