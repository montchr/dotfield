# Dotfield

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

> The map is open and connectable in all of its dimensions; it is detachable,
> reversible, susceptible to constant modification. … The map has to do with
> performance, whereas the tracing always involves an alleged “competence”.

My worlds and systems, a nutrient-rich collection of typos feeding the hungry
heads of a talking planet, "oriented toward an experimentation in contact with
the real".

### Disclaimerisms

These are my personal configurations and are not intended for use as a template,
but you are welcome to do so if you like! I hope visitors manage to find
something helpful/inspiring/interesting, but please keep in mind that *I have no
idea what I'm doing*.

Dotfield does not embody "best practices" or "the right way to Nix". The project
exists as an evolving and unstable result of one amateur's take on identifying
and implementing flexible and understandishable patterns or novelties across
other sources this amateur has encountered.

### Attn: Fellow Travellers

Feel free to copy/modify/fork under the terms of [the license](./COPYING), but
nothing here is guaranteed to work. In fact, some of it *doesn't* work or might
be total nonsense, but I will generally indicate that with `FIXME` or `TODO`
comments scattered throughout.

If you have questions or feedback, please ask away! If you use anything within
as a starting point for your own configurations and find that my implementation
doesn't work for you, feel free to ask for help. Even if I can't help, I would
be interested to hear about what you've encountered -- perhaps such an issue
would be something I'd have run into at a future point.

## Bootstrapping

This section is incomplete and fragmented, but contains important reference
notes for stuff I always forget.

```sh
export KEYID="0x135EEDD0F71934F3"
gpg --recv $KEYID

mkdir -p $XDG_CONFIG_HOME/sops/age
# Required for editing sops files
pass show age--secret-key >> $XDG_CONFIG_HOME/sops/age/keys
```


## Structure

More details forthcoming...


## Vertebrae

- [hercules-ci/flake-parts][flake-parts] :: a framework for flake modules
- [divnix/digga][digga] :: helpful lib functions and examples

[flake-parts]: https://github.com/hercules-ci/flake-parts
[digga]: https://github.com/divnix/digga

## Grafts

Generally in order of [frecency][frecency], along with an optional description
of reasons for inclusion.

More recently, I've aimed to reference sources with comments and SPDX headings
in relevant files.

### NixOS/nix-darwin/home-manager

- https://github.com/Mic92/dotfiles :: nixos, flake-parts, extensive, fleets, networking, structure, secrets management
- https://github.com/viperML/dotfiles :: nixos, flake-parts, structure
- https://git.sr.ht/~misterio/nix-config/ :: nixos, desktops, similar goals, simplicity and clarity, aesthetics
- https://github.com/srid/nixos-config :: nixos, flake-parts
- https://github.com/TLATER/dotfiles :: home-manager, structure
- https://github.com/hlissner/dotfiles :: nixos, libs, original, structure, homes w/o home-manager
- https://github.com/colemickens/nixcfg :: nixos, extensive, fun
- https://github.com/cole-h/nixos-config/ :: nixos, media server
- https://github.com/kclejeune/system :: nixos, home-manager
- https://github.com/Xe/nixos-configs :: networking, extensive
- https://github.com/sei40kr/dotfiles
- https://github.com/malob/nixpkgs :: nix-darwin, docs
- https://github.com/ahmedelgabri/dotfiles
- https://github.com/cmacrae/config :: nix-darwin, nixos, emacs
- https://github.com/d12frosted/environment :: nixos, nix-darwin, provisioning, world-building, emacs, docs

### Emacs

- [elken's doom configs][elken-doom] :: doom, similar goals, corfu module
- [tecosaur's doom configs][tecosaur-doom] :: doom, aesthetics
- [d12frosted's emacs configs][d12frosted-emacs]


[frecency]: https://en.wikipedia.org/wiki/Frecency
[tecosaur-doom]: https://tecosaur.github.io/emacs-config/config.html
[elken-doom]: https://github.com/elken/doom
[d12frosted-emacs]: https://github.com/d12frosted/environment/tree/master/emacs


## Systems

### `ryosuke` [Teenage Engineering Computer-1]

Ryosuke is a "ghost of the circuit", a denizen of Kairo, LoBE.

[PCPartPicker Part List](https://pcpartpicker.com/list/pXZ9nt)

Type|Item
:----|:----
**CPU** | [AMD Ryzen 9 5900X 3.7 GHz 12-Core Processor](https://pcpartpicker.com/product/KwLwrH/amd-ryzen-9-5900x-37-ghz-12-core-processor-100-100000061wof)
**CPU Cooler** | [Noctua NH-L9a-AM4 33.84 CFM CPU Cooler](https://pcpartpicker.com/product/DZfhP6/noctua-nh-l9a-am4-338-cfm-cpu-cooler-nh-l9a-am4)
**Motherboard** | [Gigabyte X570SI AORUS PRO AX Mini ITX AM4 Motherboard](https://pcpartpicker.com/product/s792FT/gigabyte-x570si-aorus-pro-ax-mini-itx-am4-motherboard-x570si-aorus-pro-ax)
**Memory** | [Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4-3600 CL18 Memory](https://pcpartpicker.com/product/Yg3mP6/corsair-vengeance-lpx-32-gb-2-x-16-gb-ddr4-3600-memory-cmk32gx4m2d3600c18)
**Storage** | [Samsung 970 Evo Plus 1 TB M.2-2280 NVME Solid State Drive](https://pcpartpicker.com/product/Zxw7YJ/samsung-970-evo-plus-1-tb-m2-2280-nvme-solid-state-drive-mz-v7s1t0bam)
**Video Card** | [PowerColor Radeon RX 6500 XT 4 GB ITX Video Card](https://pcpartpicker.com/product/DxjBD3/powercolor-radeon-rx-6500-xt-4-gb-itx-video-card-axrx-6500xt-4gbd6-dh)
**Case** | [teenage engineering Computer-1 Mini ITX Desktop Case](https://pcpartpicker.com/product/sdRYcf/teenage-engineering-computer-1-mini-itx-desktop-case-te030as001)
**Power Supply** | [Corsair SF 600 W 80+ Platinum Certified Fully Modular SFX Power Supply](https://pcpartpicker.com/product/BtsmP6/corsair-sf-600w-80-platinum-certified-fully-modular-sfx-power-supply-cp-9020182-na)
**Case Fan** | [Noctua A8 PWM chromax.black.swap 32.67 CFM 80 mm Fan](https://pcpartpicker.com/product/Jdwkcf/noctua-nf-a8-pwm-chromaxblackswap-3267-cfm-80-mm-fan-nf-a8-pwm-chromaxblackswap)

The Ryzen 9 5900X processor and mini-ITX Teenage Engineering Computer-1 case are the stars here.

This is my primary computer. I carry it up and down three flights of stairs
every day. I've also used it in the office as a "laptop".

### `boschic`

A beast lurking in the shadows of my living room.

Originally built in 2015, recently revamped.

[PCPartPicker Part List](https://pcpartpicker.com/list/LKQQRv)

Type|Item
:----|:----
**CPU** | [AMD Ryzen 5 5600X 3.7 GHz 6-Core Processor](https://pcpartpicker.com/product/g94BD3/amd-ryzen-5-5600x-37-ghz-6-core-processor-100-100000065box)
**CPU Cooler** | [Noctua NH-D15 82.5 CFM CPU Cooler](https://pcpartpicker.com/product/4vzv6h/noctua-nh-d15-825-cfm-cpu-cooler-nh-d15)
**Motherboard** | [Asus ROG STRIX B450-F GAMING II ATX AM4 Motherboard](https://pcpartpicker.com/product/xYvqqs/asus-rog-strix-b450-f-gaming-ii-atx-am4-motherboard-rog-strix-b450-f-gaming-ii)
**Memory** | [Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4-3600 CL18 Memory](https://pcpartpicker.com/product/Yg3mP6/corsair-vengeance-lpx-32-gb-2-x-16-gb-ddr4-3600-memory-cmk32gx4m2d3600c18)
**Storage** | [Crucial MX100 256 GB 2.5" Solid State Drive](https://pcpartpicker.com/product/63V48d/crucial-internal-hard-drive-ct256mx100ssd1)
**Storage** | [Samsung 970 Evo Plus 2 TB M.2-2280 NVME Solid State Drive](https://pcpartpicker.com/product/Fv8j4D/samsung-970-evo-plus-2-tb-m2-2280-nvme-solid-state-drive-mz-v7s2t0bam)
**Storage** | [Seagate BarraCuda 1 TB 3.5" 7200RPM Internal Hard Drive](https://pcpartpicker.com/product/dCxfrH/seagate-internal-hard-drive-st1000dm003)
**Video Card** | [NVIDIA GeForce RTX 3080 Ti 12 GB Founders Edition Video Card](https://pcpartpicker.com/product/c2kWGX/nvidia-geforce-rtx-3080-ti-12-gb-founders-edition-video-card-900-1g133-2518-000)
**Case** | [Phanteks Enthoo Pro ATX Full Tower Case](https://pcpartpicker.com/product/mn3RsY/phanteks-case-phes614pbk)
**Power Supply** | [Corsair AX 760 W 80+ Platinum Certified Fully Modular ATX Power Supply](https://pcpartpicker.com/product/Yhbp99/corsair-power-supply-ax760)
**Wireless Network Adapter** | [TP-Link Archer T5E 802.11a/b/g/n/ac PCIe x1 Wi-Fi Adapter](https://pcpartpicker.com/product/XdcRsY/tp-link-archer-t5e-pcie-x1-80211abgnac-wi-fi-adapter-archer-t5e)

### `HodgePodge` aka the "Sacred Chao"

An early-2014 15-inch MacBook Pro who has seen quite the life. Mostly unused for
the past several years due to the availability of more portable work laptops. It
is now living out its life in a declarative retirement home. `nixos-rebuild` is
impossibly slow, even with the binary cache and `ryosuke` as build host.


### Incubation


#### "`tsone`" (working title)

Hetzner SX134.

For backups, remote builds, mass storage, pretty much everything that needs to
be accessible remotely.

##### Planned

- ArchiveBox
- BorgBackup archive storage from other hosts
- Some sort of fast "cloud" storage for everyday use (notes and document sync, recent photos, etc.)

#### `sommoch` [damaged+misplaced]

Dead but dreaming.

Laid to indefinite rest when one of the cats, seeking human attention, chewed
through the LCD screen. We did not speak for a week.

Still works, but unusable without external display.

Suffers from congenital Butterfly Keyboard Syndrome. Runs macOS.

Currently lost somewhere in the meat ether.

## Errata

### kitty terminal custom icons

kitty's FAQ page shows a small collection of high-quality alternative icons designed by some kitty fans.

<https://sw.kovidgoyal.net/kitty/faq/#i-do-not-like-the-kitty-icon>

While I personally don't _dislike_ the kitty icon, these alternatives are great.

- **Currently: <https://github.com/DinkDonk/kitty-icon>**
- <https://github.com/k0nserv/kitty-icon>
- <https://github.com/hristost/kitty-alternative-icon>

#### NixOS

Not yet.

#### macOS

There's a couple hoops to jump through in order to specify a custom application icon.

And, unfortunately, because I currently install `kitty.app` with Homebrew due to
frequent build failures with `nixpkgs#kitty`, any customisations will be
reverted whenever the app updates.

I've added a script called `kitty-set-app-icon` to re-copy the desired icon back
to `kitty.app` post-update. This script is available via the
`kitty-helpers.setAppIcon` package.

Credit goes to [this blog post](https://www.sethvargo.com/replace-icons-osx/) for outlining a simple alternative to the usual drag-and-drop approach.

## License

Copyright (C) 2020-2022 Chris Montgomery

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You may read the license in full within the [COPYING](./COPYING) file included
in the project root. You may also find it at <https://www.gnu.org/licenses/>
