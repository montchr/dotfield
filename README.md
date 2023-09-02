# Dotfield

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

> The map is open and connectable in all of its dimensions; it is detachable,
> reversible, susceptible to constant modification. … The map has to do with
> performance, whereas the tracing always involves an alleged “competence”.

My worlds and systems, a nutrient-rich collection of typos feeding the hungry
heads of a talking planet, "oriented toward an experimentation in contact with
the real".

## Disclaimerisms

These are my personal configurations and are not intended for use as a template,
but you are welcome to do so if you like! I hope visitors manage to find
something helpful/inspiring/interesting, but please keep in mind that _I have no
idea what I'm doing_.

Dotfield does not embody "best practices" or "the right way to Nix". The project
exists as an evolving and unstable result of one amateur's take on identifying
and implementing flexible and understandishable patterns or novelties across
other sources this amateur has encountered.

If you have questions or feedback, feel free to reach out in the issues or discussions!

## Bootstrapping

This section is incomplete, fragmented, and I don't remember writing it.
However, it seems to contain important reference notes for stuff I always forget.

### NixOS

#### Setup

```sh
nix-env -f '<nixpkgs>' -iA nixos-install-tools git bat fd ripgrep tealdeer vim
alias nix="nix --extra-experimental-features 'nix-command flakes'"
export NEW_HOSTNAME=<your-hostname>
export GIT_BRANCH="add-${NEW_HOSTNAME}"
```

#### Partitioning and formatting

<details>
<summary><b>Option 1: Disko</b></summary>

```sh
curl "https://raw.githubusercontent.com/montchr/dotfield/${GIT_BRANCH}/machines/${NEW_HOSTNAME}/disk-config.nix" -o /tmp/disk-config.nix
nix run github:nix-community/disko -- --mode disko /tmp/disk-config.nix
# to verify:
mount | grep /mnt
```

</details>

<details>
<summary><b>Option 2: Manually</b></summary>

TODO: copy rough commands from moraine provisioning script

</details>

#### Configurate

```sh
git clone https://github.com/montchr/dotfield.git -b "${GIT_BRANCH}" /mnt/etc/nixos
# absolute paths from `/mnt` would break once booted into the system
cd /mnt/etc && ln -s nixos dotfield && cd nixos
nixos-generate-config --no-filesystems --root /mnt
```

Integrate any missing configuration from the generator locally, push to remote, and pull on the host.

#### Install

```sh
nixos-install --flake ".#${NEW_HOSTNAME}"
```

After rebooting, edit `~/.ssh/known_hosts` on your local machine to remove the initial entries since the host keys have been reset after installation.

#### Record Public Keys

```sh
export KEYS_DIR="$PRJ_ROOT/cells/ops/data/keys"
export NEW_HOSTNAME=<...>
export NEW_HOSTIP=<...>

ssh root@$NEW_HOSTIP -t 'cat /etc/ssh/ssh_host_ed25519_key.pub' \
> "$KEYS_DIR/ssh/$NEW_HOSTNAME.pub"

ssh root@$NEW_HOSTIP -t 'cat /etc/ssh/ssh_host_rsa_key.pub' \
> "$KEYS_DIR/ssh/$NEW_HOSTNAME-rsa.pub"

nix run nixpkgs#ssh-to-age -- -i "$KEYS_DIR/ssh/$NEW_HOSTNAME.pub" \
| tr --delete '\n' \
> "$KEYS_DIR/age/$NEW_HOSTNAME.txt"

git add $KEYS_DIR
```

#### Update Secret Recipients

Add the host to `//cells/secrets/cfg/sops.nix`, then:

```sh
direnv reload
grep "$NEW_HOSTNAME" $PRJ_ROOT/.sops.yaml --before-context=10
sops updatekeys secrets/global.secrets.yaml
```

### Generic Linux

TODO

### macOS/Darwin

Something along these lines:

- `sudo xcode-select --install`
- Install Homebrew
- `brew install git bash zsh ripgrep fd tealdeer bat coreutils`
- Add Homebrew `PATH` entries to `~/.zprofile` as directed
- Install Nix
- Generate SSH keys for your user and add them to GitHub/Sourcehut.
- `mkdir -p ~/.config`
- `git clone git@github.com:montchr/dotfield.git ~/.config/dotfield`
- Create a basic config for the new host in `./darwin/machines/<hostname>/default.nix`
- Add the new host to `flake.darwinConfigurations.<hostname>` in `./darwin/configurations.nix`
- `nix build .#darwinConfigurations.<hostname>.system --verbose`
- `./result/sw/bin/darwin-rebuild switch --flake .#<hostname>` (assuming that
  the desired hostname has not yet been set -- otherwise, `... --flake .` should suffice)

### Secrets

After the initial generation with secrets disabled (due to a
catch-22/bootstrapping problem), you should then be able to do the following
with a smartcard attached.

```sh
export KEYID="0x135EEDD0F71934F3"
gpg --recv $KEYID
gpg --list-secret-keys
gpg-agent-restart

mkdir -p $XDG_CONFIG_HOME/sops/age
# Required for editing sops files
pass show age--secret-key >> $XDG_CONFIG_HOME/sops/age/keys
```

## Structure

I still don't know what I'm doing.
Maybe one day I will have enough of an idea that I can write it down with any amount of coherence.
Or maybe I'm doing it all backwards…?

## Grafts

Generally in order of [frecency][frecency], along with an optional description
of reasons for inclusion.

More recently, I've aimed to reference sources with comments and SPDX headings
in relevant files.

### NixOS/nix-darwin/home-manager

- <https://github.com/lovesegfault/nix-config> :: nixos
- <https://github.com/oddlama/nix-config> :: nixos, sso, good docs, microvms
- <https://git.sr.ht/~misterio/nix-config/> :: nixos, desktops, similar goals, simplicity and clarity, aesthetics
- <https://github.com/Mic92/dotfiles> :: nixos, flake-parts, extensive, fleets, networking, structure, secrets management
- <https://github.com/srid/nixos-config> :: nixos, nix-darwin, vms, simple, nixos-shell
- <https://github.com/viperML/dotfiles> :: nixos, flake-parts, structure
- <https://github.com/TLATER/dotfiles> :: home-manager, structure
- <https://github.com/d12frosted/environment> :: nixos, nix-darwin, world-building, emacs, docs
- <https://github.com/colemickens/nixcfg> :: nixos, extensive, fun
- <https://github.com/cole-h/nixos-config/> :: nixos, media server
- <https://github.com/kclejeune/system> :: nixos, home-manager
- <https://github.com/Xe/nixos-configs> :: networking, extensive
- <https://github.com/sei40kr/dotfiles>
- <https://github.com/hlissner/dotfiles> :: nixos, libs, original, structure, homes w/o home-manager
- <https://github.com/malob/nixpkgs> :: nix-darwin, docs
- <https://github.com/ahmedelgabri/dotfiles>
- <https://github.com/cmacrae/config> :: nix-darwin, nixos, emacs

[frecency]: https://en.wikipedia.org/wiki/Frecency

## Systems

### `tuvix` [MacBook Air M2]

Work computer running macOS. It's fresh. It's sleek. But it's still a Mac...

### `ryosuke` [Teenage Engineering Computer-1]

Ryosuke is a "ghost of the circuit", a denizen of Kairo, LoBE.

[PCPartPicker Part List](https://pcpartpicker.com/list/pXZ9nt)

| Type             | Item                                                                                                                                                                                                 |
| :--------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **CPU**          | [AMD Ryzen 9 5900X 3.7 GHz 12-Core Processor](https://pcpartpicker.com/product/KwLwrH/amd-ryzen-9-5900x-37-ghz-12-core-processor-100-100000061wof)                                                   |
| **CPU Cooler**   | [Noctua NH-L9a-AM4 33.84 CFM CPU Cooler](https://pcpartpicker.com/product/DZfhP6/noctua-nh-l9a-am4-338-cfm-cpu-cooler-nh-l9a-am4)                                                                    |
| **Motherboard**  | [Gigabyte X570SI AORUS PRO AX Mini ITX AM4 Motherboard](https://pcpartpicker.com/product/s792FT/gigabyte-x570si-aorus-pro-ax-mini-itx-am4-motherboard-x570si-aorus-pro-ax)                           |
| **Memory**       | [Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4-3600 CL18 Memory](https://pcpartpicker.com/product/Yg3mP6/corsair-vengeance-lpx-32-gb-2-x-16-gb-ddr4-3600-memory-cmk32gx4m2d3600c18)                   |
| **Storage**      | [Samsung 970 Evo Plus 1 TB M.2-2280 NVME Solid State Drive](https://pcpartpicker.com/product/Zxw7YJ/samsung-970-evo-plus-1-tb-m2-2280-nvme-solid-state-drive-mz-v7s1t0bam)                           |
| **Video Card**   | [PowerColor Radeon RX 6500 XT 4 GB ITX Video Card](https://pcpartpicker.com/product/DxjBD3/powercolor-radeon-rx-6500-xt-4-gb-itx-video-card-axrx-6500xt-4gbd6-dh)                                    |
| **Case**         | [teenage engineering Computer-1 Mini ITX Desktop Case](https://pcpartpicker.com/product/sdRYcf/teenage-engineering-computer-1-mini-itx-desktop-case-te030as001)                                      |
| **Power Supply** | [Corsair SF 600 W 80+ Platinum Certified Fully Modular SFX Power Supply](https://pcpartpicker.com/product/BtsmP6/corsair-sf-600w-80-platinum-certified-fully-modular-sfx-power-supply-cp-9020182-na) |
| **Case Fan**     | [Noctua A8 PWM chromax.black.swap 32.67 CFM 80 mm Fan](https://pcpartpicker.com/product/Jdwkcf/noctua-nf-a8-pwm-chromaxblackswap-3267-cfm-80-mm-fan-nf-a8-pwm-chromaxblackswap)                      |

The Ryzen 9 5900X processor and mini-ITX Teenage Engineering Computer-1 case are the stars here.

This is my primary computer. I carry it up and down three flights of stairs
every day. I've also used it in the office as a "laptop".

### `moraine`

Hetzner AX52 (+ ECC) (+ 2x16TB HDD)

Media server. Work in progress.

#### Name Origin

[Moraine - Official Outer Wilds Wiki](https://outerwilds.fandom.com/wiki/Moraine)

> **Moraine** is a [Hearthian](https://outerwilds.fandom.com/wiki/Hearthian 'Hearthian') who enjoys using the [Signalscope](https://outerwilds.fandom.com/wiki/Signalscope 'Signalscope'), especially to listen to the [travelers](https://outerwilds.fandom.com/wiki/Travelers 'Travelers') music from across the [Solar system](https://outerwilds.fandom.com/wiki/Solar_system 'Solar system'). They are found on a platform atop the tallest tree in [The Village](https://outerwilds.fandom.com/wiki/The_Village 'The Village'), so they will have a better view of the planets.

### `boschic`

A towering beast lurking in the shadows of my living room.

Originally built in 2015, recently revamped.

[PCPartPicker Part List](https://pcpartpicker.com/list/LKQQRv)

| Type                         | Item                                                                                                                                                                                    |
| :--------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **CPU**                      | [AMD Ryzen 5 5600X 3.7 GHz 6-Core Processor](https://pcpartpicker.com/product/g94BD3/amd-ryzen-5-5600x-37-ghz-6-core-processor-100-100000065box)                                        |
| **CPU Cooler**               | [Noctua NH-D15 82.5 CFM CPU Cooler](https://pcpartpicker.com/product/4vzv6h/noctua-nh-d15-825-cfm-cpu-cooler-nh-d15)                                                                    |
| **Motherboard**              | [Asus ROG STRIX B450-F GAMING II ATX AM4 Motherboard](https://pcpartpicker.com/product/xYvqqs/asus-rog-strix-b450-f-gaming-ii-atx-am4-motherboard-rog-strix-b450-f-gaming-ii)           |
| **Memory**                   | [Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4-3600 CL18 Memory](https://pcpartpicker.com/product/Yg3mP6/corsair-vengeance-lpx-32-gb-2-x-16-gb-ddr4-3600-memory-cmk32gx4m2d3600c18)      |
| **Storage**                  | [Crucial MX100 256 GB 2.5" Solid State Drive](https://pcpartpicker.com/product/63V48d/crucial-internal-hard-drive-ct256mx100ssd1)                                                       |
| **Storage**                  | [Samsung 970 Evo Plus 2 TB M.2-2280 NVME Solid State Drive](https://pcpartpicker.com/product/Fv8j4D/samsung-970-evo-plus-2-tb-m2-2280-nvme-solid-state-drive-mz-v7s2t0bam)              |
| **Storage**                  | [Seagate BarraCuda 1 TB 3.5" 7200RPM Internal Hard Drive](https://pcpartpicker.com/product/dCxfrH/seagate-internal-hard-drive-st1000dm003)                                              |
| **Video Card**               | [NVIDIA GeForce RTX 3080 Ti 12 GB Founders Edition Video Card](https://pcpartpicker.com/product/c2kWGX/nvidia-geforce-rtx-3080-ti-12-gb-founders-edition-video-card-900-1g133-2518-000) |
| **Case**                     | [Phanteks Enthoo Pro ATX Full Tower Case](https://pcpartpicker.com/product/mn3RsY/phanteks-case-phes614pbk)                                                                             |
| **Power Supply**             | [Corsair AX 760 W 80+ Platinum Certified Fully Modular ATX Power Supply](https://pcpartpicker.com/product/Yhbp99/corsair-power-supply-ax760)                                            |
| **Wireless Network Adapter** | [TP-Link Archer T5E 802.11a/b/g/n/ac PCIe x1 Wi-Fi Adapter](https://pcpartpicker.com/product/XdcRsY/tp-link-archer-t5e-pcie-x1-80211abgnac-wi-fi-adapter-archer-t5e)                    |

### `HodgePodge` aka the "Sacred Chao"

An early-2014 15-inch MacBook Pro who has seen quite the life. Mostly unused for
the past several years due to the availability of more portable work laptops. It
is now living out its life in a declarative retirement home. `nixos-rebuild` is
impossibly slow, even with the binary cache and `ryosuke` as build host.

### Incubation

#### `sommoch`

Dead but dreaming.

Laid to indefinite rest when one of the cats, seeking human attention, chewed
through the LCD screen. We did not speak for a week.

Still works, but unusable without external display.
Suffers from congenital Butterfly Keyboard Syndrome.
Runs macOS.

## Notes

### macOS GUI Applications via Nix

**UPDATE: `emacs29-macport` works amazingly!**
Thanks to `@tnytown` for their hard work in
https://github.com/NixOS/nixpkgs/issues/127902
and https://github.com/NixOS/nixpkgs/pull/252244.

#### Emacs

The package `emacs29-macport` works well.
It's based on [Mitsuharu Yamamoto’s excellent macOS port][emacs-macport] of GNU Emacs.
The Nix package used to have issues with GUI crashes,
but AFAIK those are no longer an issue.

- Install `emacs29-macport` via `environment.systemPackages` (from nix-darwin)
- Open it from a terminal shell session via `/usr/bin/open -a '/Applications/Nix Apps/Emacs.app'`

Still, nix-darwin `launchd` service never seems to work, breaking `emacsclient` functionality.
I have not yet confirmed whether this is still an issue with `emacs29-macport`.
The following quote from the manual seems to indicate that such
functionality remains a mystery:

> The Mac port doesn't support multi-tty with GUI. The developer has
> no idea how to detach Emacs as a GUI application from Window Server or
> Dock without separating a GUI process (not thread) from the main Emacs
> (Lisp evaluator) process. TTY-only multi-tty is supposed to work.

[mituharu / emacs-mac / README-mac — Bitbucket](https://bitbucket.org/mituharu/emacs-mac/src/master/README-mac)

[emacs-macport]: https://bitbucket.org/mituharu/emacs-mac/src/master/

#### yabai

Just use the official package released via `brew`, unless you prefer pain.
