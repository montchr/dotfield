# Dotfield

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

My worlds and systems, a nutrient-rich typo terraforming the hungry heads of a
talking planet.

## Disclaimer

These configurations are generally very rough, disorganised, and frustrating to
work with. I plan to do some clean up soon.

If you have questions or feedback, please ask/post away! I hope visitors manage
to find something helpful/inspiring/interesting, but please keep in mind that
**I have no idea what I'm doing**. I sometimes make changes that I sound
confident about, but may end up reversing them days or hours later. If something
doesn't work for you, I may be able to help, but please don't assume that just
because it lives in my configuration means that it "works" or "is a best
practice" or "is in any way secure".

Which brings me to the license:

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

## Structure

### Profiles/Suites

Overall, I've found that the profile "composition" approach is very difficult to
scale. I plan on moving back towards a simpler module-toggle structure,
converting "suites" into "profiles". From my understanding, that's what profiles
always have been in NixOS terminology. I believe "suites" is an unncessary and
unwieldly abstraction and hope to see it removed from the DevOS example
eventually, or at least "downgraded" to just another example. But only after
finding a better suggestion.

## Systems

### `ryosuke` Computer-1

This one is fresh off the workbench. After much frustration and screaming during
assembly, I've built a small form-factor PC in a Teenage Engineering Computer-1
case.

Details to follow.

I intend for this to be my primary and semi-portable personal computer and
perhaps transitioning `boschic` to a new Face as home server.

### `boschic`

A three-faced beast lurking in the shadows of my living room.

Originally built in 2015, recently revamped for ~computing power~ ~playing Myst in VR~
~Elden Ring~ fun.

<table>
<tbody>
<tr>
<th class="org-left">CPU</th>
<td class="org-left">AMD Ryzen 5 5600X 3.7 GHz 6-Core Processor</td>
</tr>

<tr>
<th class="org-left">CPU Cooler</th>
<td class="org-left">Noctua NH-D15 82.5 CFM CPU Cooler</td>
</tr>

<tr>
<th class="org-left">Motherboard</th>
<td class="org-left">Asus ROG STRIX B450-F GAMING II ATX AM4 Motherboard</td>
</tr>

<tr>
<th class="org-left">Memory</th>
<td class="org-left">Corsair Vengeance LPX 32 GB (2 x 16 GB) DDR4-3600 CL18 Memory</td>
</tr>

<tr>
<th class="org-left">Storage</th>
<td class="org-left">Seagate BarraCuda 1 TB 3.5&ldquo; 7200RPM Internal Hard Drive</td>
</tr>

<tr>
<th class="org-left">Storage</th>
<td class="org-left">Crucial MX100 256 GB 2.5&ldquo; Solid State Drive</td>
</tr>

<tr>
<th class="org-left">Storage</th>
<td class="org-left">Samsung 970 Evo Plus 2 TB M.2-2280 NVME Solid State Drive</td>
</tr>

<tr>
<th class="org-left">GPU</th>
<td class="org-left">NVIDIA GeForce RTX 3080 Ti 12 GB Founders Edition Video Card</td>
</tr>

<tr>
<th class="org-left">Case</th>
<td class="org-left">Phanteks Enthoo Pro ATX Full Tower Case</td>
</tr>

<tr>
<th class="org-left">PSU</th>
<td class="org-left">Corsair AX 760 W 80+ Platinum Certified Fully Modular ATX Power Supply</td>
</tr>

<tr>
<th class="org-left">Wireless Adapter</th>
<td class="org-left">TP-Link Archer T5E 802.11a/b/g/n/ac PCIe x1 Wi-Fi Adapter</td>
</tr>
</tbody>
</table>

#### Face One: Gaming + VR

I don't want to open a gateway to [my own VR Hell on NixOS][vrhell], so this
Face should only be summoned after invoking the Ten Windows.

[vrhell]: https://xeiaso.net/blog/nixos-vr-hell-2021-12-02

#### Face Two: Home Theater PC

Boschic is connected to a ViewSonic "4K" projector pointed at a ceiling-mounted
100in. screen.

While the Plex Media Player experience on NixOS is... manageable... I suspect that the situation may be deteriorating:

Plex has stated they will be dropping support for the "Plex Media Player"
application. And it shows. This would be fine with me, because its UI is
terrible, especially in 4K resolution across the room. Its UI animations are
janky and jittery as hell. And yet... it plays even 4K video perfectly.

The ~new~ resurrected Plex HTPC application is beautiful. The design is
well-thought-out for my own sort of use case. However, it requires Flatpak
(yuck), and it totally fails to play back even low-resolution videos on my
projector without introducing unwatchable stuttering. I've noticed some log
errors relating to WebGL. It seems to be related to Wayland/XWayland. I thought
that the proprietary NVIDIA drivers fixed the issue, but then it came back. It's
completely unusable with Nouveau.

So, uh, I don't know.

My roommates usually invoke the Ten Windows before playing video anyway. I'm
sure this says something about how I have not done a great job making the
NixOS-GNOME HTPC experience easily approachable on a guest user account (that's
`zortflower`!), but I've ran into so many issues trying to get it working on my
own account that I totally understand.

#### Face Three: Daily Driver

Currently my primary computer, which has caused some conflict and pain (I
literally sit on the couch all day). This shouldn't be the case for much longer
though.

### `HodgePodge` aka the "Sacred Chao"

An early-2014 15-inch MacBook Pro who has seen quite the life. Mostly unused for
the past several years due to the availability of more portable work laptops. It
is now living out its life in a declarative retirement home.

Its excessive clunkiness is excacerbated by the sharp edges exposed on its
sturdy sticker-laden plastic case over the years. The situation is more
manageable now thanks to the globs of Sugru preventing any further bodily harm.


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

## Identities

From https://github.com/drduh/YubiKey-Guide#using-keys:

```sh
export KEYID="0x135EEDD0F71934F3"
gpg --recv $KEYID
```


## Vertebrae

- https://github.com/divnix/digga :: a good friend

## Grafts

Generally in order of recency.

- https://github.com/Xe/nixos-configs
- https://github.com/sei40kr/dotfiles
- https://github.com/malob/nixpkgs
- https://github.com/kclejeune/system
- https://github.com/ahmedelgabri/dotfiles
- https://github.com/cmacrae/config
- https://github.com/hlissner/dotfiles
- https://github.com/d12frosted/environment
- https://github.com/hardselius/dotfiles
- https://github.com/alrra/dotfiles
- https://github.com/jasonheecs/ubuntu-server-setup

## Errata

### kitty terminal custom icons

kitty's FAQ page shows a small collection of high-quality alternative icons designed by some kitty fans.

<https://sw.kovidgoyal.net/kitty/faq/#i-do-not-like-the-kitty-icon>

While I personally don't _dislike_ the kitty icon, these alternatives are great.

- **Currently: <https://github.com/DinkDonk/kitty-icon>**
- <https://github.com/k0nserv/kitty-icon>
- <https://github.com/hristost/kitty-alternative-icon>

#### NixOS

TKTKTK

#### macOS

There's a couple hoops to jump through in order to specify a custom application icon.

And, unfortunately, because I currently install `kitty.app` with Homebrew due to
frequent build failures with `nixpkgs#kitty`, any customisations will be
reverted whenever the app updates.

I've added a script called `kitty-set-app-icon` to re-copy the desired icon back
to `kitty.app` post-update. This script is available via the
`kitty-helpers.setAppIcon` package.

Credit goes to [this blog post](https://www.sethvargo.com/replace-icons-osx/) for outlining a simple alternative to the usual drag-and-drop approach.
