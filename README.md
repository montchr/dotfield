# Dotfield

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

> The map is open and connectable in all of its dimensions; it is detachable,
> reversible, susceptible to constant modification. … The map has to do with
> performance, whereas the tracing always involves an alleged “competence”.

My worlds and systems, a nutrient-rich collection of typos feeding the hungry
heads of a talking planet, "oriented toward an experimentation in contact with
the real".

```
all records of prior decision motivation regularly wiped
upon stepping into the sun, birdsongs irradiating
scattered dream fragments dragged down hallways,
the central motif of some one of many ghosts' nightly roams
mindlessly down to hell.

a declarative and functional tape loop magnetically
engraved in lead paint by an adamantine lathe,
melting off the walls,
maddening speed of river tides creeping ever closer to
this house i dare to call my own,
a crucial delusion suppressed for safe passage.

do not resist the birds, they know you all too well.
time to call the sound sweep again;
they say i'm ready for my fifteen-minute closeup
```

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
export KEYS_DIR="$PRJ_ROOT/ops/keys"
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

Add the host to `//.sops.yaml`, then:

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

Currently (as of 2024-01-22), Ryosuke is serving as a living room HTPC. I've
also used it in the office as a "laptop".

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

#### Audio/Video Input/Output

Workstation desk has the following devices:

- Audio in/out: Focusrite Scarlett 18i20 [Gen 1] Audio Interface/Mixer
- Video in, Audio (mic) in: Logitech Brio 501 Webcam

```console
seadoom@boschic ~ % dmesg | grep -i -B 3 focusrite
[    1.567008] usb 5-1: New USB device found, idVendor=1235, idProduct=800c, bcdDevice= 4.4c
[    1.567011] usb 5-1: New USB device strings: Mfr=1, Product=2, SerialNumber=0
[    1.567012] usb 5-1: Product: Scarlett 18i20 USB
[    1.567013] usb 5-1: Manufacturer: Focusrite

seadoom@boschic ~ % pw-dump | grep node.name | grep alsa
        "node.name": "alsa_input.usb-046d_Brio_501_2235LZ52HK58-02.analog-stereo",
        "node.name": "alsa_output.pci-0000_0c_00.4.iec958-stereo",
        "node.name": "alsa_input.pci-0000_0c_00.4.analog-stereo",
        "node.name": "alsa_output.pci-0000_0a_00.1.hdmi-stereo",
        "node.name": "alsa_output.usb-Focusrite_Scarlett_18i20_USB-00.multichannel-output",
        "node.name": "alsa_input.usb-Focusrite_Scarlett_18i20_USB-00.multichannel-input",

seadoom@boschic ~ % lspci | grep -i audio
0a:00.1 Audio device: NVIDIA Corporation GA102 High Definition Audio Controller (rev a1)
0c:00.4 Audio device: Advanced Micro Devices, Inc. [AMD] Starship/Matisse HD Audio Controller

seadoom@boschic ~ % lsusb | grep -i scarlett
Bus 005 Device 008: ID 1235:800c Focusrite-Novation Scarlett 18i20
```

The 18i20 is connected to KRK Rokit 5 studio monitors via the interface's
L/R monitor output channels. Unfortunately, ALSA resets the monitor output level
to muted status each time the device is reconnected.

See `/var/lib/alsa/asound.state` for the state of ALSA settings as of last boot.
According to
<https://wiki.archlinux.org/title/Advanced_Linux_Sound_Architecture#ALSA_and_systemd>,
updated values will be written on shutdown.

Possible culprits for the mute status:

(note that this one is not the 18i20, which is USB):

```
State.Generic {
...
	control.18 {
		iface MIXER
		name 'Auto-Mute Mode'
		value Enabled
		comment {
			access 'read write'
			type ENUMERATED
			count 1
			item.0 Disabled
			item.1 Enabled
		}
	}
...
}
```

And for the 18i20 itself:

```
State.USB {
...
	control.9 {
		iface MIXER
		name 'Master 1 (Monitor) Playback Switch'
		value.0 false
		value.1 false
		comment {
			access 'read write'
			type BOOLEAN
			count 2
		}
	}
...
}
```

Source should be PCM 1

### `HodgePodge` aka the "Sacred Chao"

An early-2014 15-inch MacBook Pro who has seen quite the life. ~Mostly unused
for the past several years due to the availability of more portable work
laptops. It is now living out its life in a declarative retirement home.~

**Update [2024-01-22]:** This laptop has recently become my primary computer.

`nixos-rebuild` is impossibly slow, even with the binary cache and `ryosuke` as
build host.

#### Specifications

MacBookPro11,3 (Late 2013) (Dual-Graphics Retina Display)

| Type                   | Item                                                                  |
| :--------------------- | :-------------------------------------------------------------------- |
| **CPU**                | Intel(R) Core(TM) i7-4850HQ CPU @ 2.30GHz (4-core)                    |
| **Network Controller** | Broadcom BCM4360 802.11ac Dual Band Wireless Network Adapter (rev 03) |
| **Video**              | NVIDIA Corporation GK107M [GeForce GT 750M Mac Edition] (rev a1)      |
| **Camera**             | Broadcom 720p FaceTime HD Camera                                      |
| **Audio**              | Intel 8 Series / C220 Series HDAC                                     |
| **Audio**              | NVIDIA GK107 HDMI Audio Controller                                    |
| **SATA**               | Samsung S4LN053X01 AHCI SSD Controller (Apple slot)                   |

**Tip:** to determine Apple model within Linux, run `grep -h . /sys/devices/virtual/dmi/id/board_v*`.

##### "Dual-Graphics"

Technically, there is also an integrated Intel graphics card in addition to the
discrete NVIDIA card. However, Apple decided to force the integrated Intel GPU
to be disabled unless running macOS. There are ways around that (apparently
rEFInd can help), but I don't think it's worth the extra effort.

This issue was originally reported on the grub-devel mailing list, but it
doesn't look like the patch was ever merged (but I don't yet understand how
these mailing list workflows operate so it's pretty unclear to me what the
actual "status" is).

<https://lists.gnu.org/archive/html/grub-devel/2013-12/msg00442.html>

For more info:

- <https://gist.github.com/stefanocoding/c6dbf4489f330021bd9335d655c9fbbf>
- <https://github.com/0xbb/apple_set_os.efi>

##### Display flickering

[Flickering with gnome 3.22.2-1 + gdm on macbook pro / Applications & Desktop Environments / Arch Linux Forums](https://bbs.archlinux.org/viewtopic.php?id=219442)

Start the machine with the power adapter unplugged (either boot or wake).

> Whenever I've had the flicker today I've fixed it by: Unplug power cable, shut
> the lid, allow it to suspend (wait for the apple logo light to go out), open
> the lid, re-plug power cable. This process has removed the flicker the two
> times I've had it today.

Note that this is **not** specific to GNOME, but affects KDE Plasma and SDDM too
(according to one user's report).

The discussion resulted in a chain of bug reports leading here:

<https://gitlab.freedesktop.org/drm/amd/-/issues/759>

The discussion and bug report also pertain to AMD graphics, which doesn't apply
to the MacBookPro11,3 model. And yet the power adapter workaround still seems to
help resolve the issue with this machine's NVIDIA graphics...

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

**UPDATE: `emacs29-macport` works amazingly!** Thanks to `@tnytown` for their
hard work in https://github.com/NixOS/nixpkgs/issues/127902 and
https://github.com/NixOS/nixpkgs/pull/252244.

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

## Shitlist

Mostly opinions and rants, which will likely be removed in time.

### GNOME Desktop

It's been the default desktop environment for my graphical NixOS configurations,
but only because it "just works". In actuality, while it "works", it **just**
works. Its primary method of configuration is based on the state of
user-selected options in GUIs, hiding the configuration behind opaque UX

There is, I'm sure, a specific term for this particular application design
fallacy, perhaps best summarized by WordPress' infamous "decisions not options"
aphorism and embodied in Apple software and hardware. While it looks pretty and
looks like it has "good UX", like macOS (and Apple software in general), it has
so much useless clutter and ends up breaking in weird ways.

#### Definitions + Resources

`gsettings` is a command-line interface for `dconf` schema introspection. It's
useful for getting/setting the current value of a setting and for scripting
necessarily-stateful settings like theme appearance variants.

`dconf` is the underlying backend, a database store.

"dconf Editor" is a GUI application for quick and direct interaction with
available settings, similar to Firefox's `about:config` page.

#### Configuring GNOME via `home-manager`

[gvolpe/dconf2nix: :feet: Convert Dconf files (e.g. Gnome Shell) to Nix, as expected by Home Manager](https://github.com/gvolpe/dconf2nix)
in combination with
[home-manager/modules/misc/dconf.nix at master · nix-community/home-manager](https://github.com/nix-community/home-manager/blob/master/modules/misc/dconf.nix)

#### Seahorse / `gnome-keyring-daemon`

Unreliable and clunky.

##### Default keyring lockouts

On multiple occasions, I've gotten locked out of the default keyring,
which is supposed to be the same as the login password. Deleting a couple files
resets it:

```sh
rm ~/.local/share/keyrings/{login.keyring,user.keystore}
```

<https://github.com/NixOS/nixpkgs/issues/174099>

##### Undismissible modal prompt

When I first activated the Nextcloud Client application on boschic, I was
prompted to unlock the `default` keyring. But because I was locked out, I needed
to dismiss the prompt. The prompt immediately reappeared.

Because the GNOME prompts are "accessible", there is no way to escape focus
other than pressing cancel. That means that a process can spam request a prompt
indefinitely, with no way to kill the process from within the GNOME session. I
had to resort to C-M-F1 to switch to the virtual console / getty.

On that note, considering that I've disabled getty@tty1 when autologin is
enabled (due to a NixOS issue), switching tty might not work in those scenarios...

#### Firefox

I am considering migrating off Firefox. as Mozilla seems to be enshittifying
itself, favoring business interests and flashy posturing as competition to
Chrome and other browsers. I am losing the energy to keep up with it.

Firefox on Android crashes constantly and I literally can't use any version of
it anymore, so I've already ditched it on my phone in favor of Vanadium
(Chromium-based). Therefore Firefox Sync no longer matters to me as a feature.

The only clear benefit to using Firefox is its (decreasingly) deep
customizability but it has always felt like a bad way to spend my time.

I really do want a browser to "just work", but with the option to configure
frustrating things like... keyboard shortcuts... which Firefox does not support.
The question is: do any browsers? I am not yet sure.

##### Customizing keyboard shortcuts

TL;DR Nope, forget it.

<https://support.mozilla.org/bm/questions/1381773>

there is no way to customize internal keyboard shortcuts. this should be
considered a bug, not a feature request. but it's considered an "idea" in a
community feedback forum, with no indication as to whether it matters to Mozilla
or not.

i wonder whether the community forum is intended to act as a buffer zone /
feedback echo chamber to keep users out of the internal bug trackers. Mozilla
has to actually consider and prioritize user feedback from this forum, otherwise
why does it exist? Is there any evidence of such a process?
