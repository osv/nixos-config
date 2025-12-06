# NixOS Configuration Modules

## Apps

| Option | Description |
|--------|-------------|
| `nerv.opt.apps._1password` | 1Password password manager |
| `nerv.opt.apps.ai` | AI tools (whisper, claude-desktop, code-cursor, claude-code) |
| `nerv.opt.apps.ardour` | Digital audio workstation |
| `nerv.opt.apps.blender` | 3D creation suite |
| `nerv.opt.apps.bottles` | Windows application runner |
| `nerv.opt.apps.cadence` | JACK audio production tools |
| `nerv.opt.apps.chromium` | Chromium web browser |
| `nerv.opt.apps.dbclients` | Database client applications |
| `nerv.opt.apps.discord` | Discord chat application |
| `nerv.opt.apps.dolphin` | KDE file manager |
| `nerv.opt.apps.doukutsu-rs` | Cave Story game engine |
| `nerv.opt.apps.element` | Matrix chat client |
| `nerv.opt.apps.emacs` | Emacs editor with Doom config |
| `nerv.opt.apps.etcher` | USB/SD card image writer |
| `nerv.opt.apps.firefox` | Firefox browser with privacy config |
| `nerv.opt.apps.frappe-books` | Accounting software |
| `nerv.opt.apps.freetube` | Privacy-focused YouTube client |
| `nerv.opt.apps.gimp` | Image manipulation program |
| `nerv.opt.apps.gparted` | Partition editor |
| `nerv.opt.apps.hey` | Benchmarking tool |
| `nerv.opt.apps.inkscape` | Vector graphics editor |
| `nerv.opt.apps.logseq` | Knowledge management tool |
| `nerv.opt.apps.looking-glass-client` | VM display client |
| `nerv.opt.apps.lutris` | Gaming platform |
| `nerv.opt.apps.mpv` | Media player with uosc UI |
| `nerv.opt.apps.obs` | Broadcasting/recording software |
| `nerv.opt.apps.office` | Office suite applications |
| `nerv.opt.apps.pcsx2` | PlayStation 2 emulator |
| `nerv.opt.apps.pitivi` | Video editor |
| `nerv.opt.apps.pocketcasts` | Podcast player |
| `nerv.opt.apps.prismlauncher` | Minecraft launcher |
| `nerv.opt.apps.protontricks` | Proton game fixes helper |
| `nerv.opt.apps.qimgv` | Qt5 image viewer |
| `nerv.opt.apps.rpcs3` | PlayStation 3 emulator |
| `nerv.opt.apps.shotcut` | Video editor |
| `nerv.opt.apps.simplescreenrecorder` | Screen recorder for Linux |
| `nerv.opt.apps.steamtinkerlaunch` | Steam game tweaking tool |
| `nerv.opt.apps.telegram` | Telegram messenger |
| `nerv.opt.apps.torrent` | BitTorrent client |
| `nerv.opt.apps.twitter` | Twitter client |
| `nerv.opt.apps.ubports-installer` | Ubuntu Touch installer |
| `nerv.opt.apps.viber` | Viber messenger |
| `nerv.opt.apps.virtualbox` | Virtualization software |
| `nerv.opt.apps.vlc` | Media player |
| `nerv.opt.apps.vscode` | Visual Studio Code editor |
| `nerv.opt.apps.winetricks` | Wine helper script |
| `nerv.opt.apps.yt-music` | YouTube Music desktop app |
| `nerv.opt.apps.yuzu` | Nintendo Switch emulator |
| `nerv.opt.apps.zathura` | Lightweight PDF/document viewer |
| `nerv.opt.apps.zoom` | Video conferencing |

## CLI Apps

| Option | Description |
|--------|-------------|
| `nerv.opt.cli-apps.bpftrace` | Linux tracing tool |
| `nerv.opt.cli-apps.prisma` | Database toolkit |
| `nerv.opt.cli-apps.proton` | Proton compatibility layer |
| `nerv.opt.cli-apps.tmux` | Terminal multiplexer with custom config |
| `nerv.opt.cli-apps.wine` | Windows compatibility layer |
| `nerv.opt.cli-apps.wshowkeys` | Wayland key display tool |

## Archetypes

| Option | Description |
|--------|-------------|
| `nerv.archetypes.gaming` | Gaming-focused system profile |
| `nerv.archetypes.minimal` | Minimal system configuration |
| `nerv.archetypes.server` | Server system profile |
| `nerv.archetypes.workstation` | Workstation profile with development tools |

## Desktop

| Option | Description |
|--------|-------------|
| `nerv.opt.desktop.addons.boomer` | Screen zoom tool |
| `nerv.opt.desktop.addons.electron-support` | Electron app wayland/scaling fixes |
| `nerv.opt.desktop.addons.file` | File manager integration |
| `nerv.opt.desktop.addons.firefox-nordic-theme` | Nordic theme for Firefox |
| `nerv.opt.desktop.addons.gtk` | GTK theming and configuration |
| `nerv.opt.desktop.addons.kanshi` | Wayland display configuration |
| `nerv.opt.desktop.addons.keyring` | Desktop keyring services |
| `nerv.opt.desktop.addons.mako` | Wayland notification daemon |
| `nerv.opt.desktop.addons.mime` | MIME type associations |
| `nerv.opt.desktop.addons.notify` | Desktop notifications |
| `nerv.opt.desktop.addons.picom` | X11 compositor |
| `nerv.opt.desktop.addons.plasma-hack` | KDE Plasma customizations |
| `nerv.opt.desktop.addons.rofi` | Application launcher |
| `nerv.opt.desktop.addons.screenlock` | Screen lock with betterlockscreen + xidlehook |
| `nerv.opt.desktop.addons.swappy` | Screenshot annotation tool |
| `nerv.opt.desktop.addons.term` | Terminal configuration |
| `nerv.opt.desktop.addons.term.kitty` | Kitty terminal emulator with Dracula theme |
| `nerv.opt.desktop.addons.term.urxvt` | URxvt with dynamic theming |
| `nerv.opt.desktop.addons.term.wezterm` | WezTerm with font/theme switching hotkeys |
| `nerv.opt.desktop.addons.wallpapers` | Wallpaper management |
| `nerv.opt.desktop.addons.waybar` | Wayland status bar |
| `nerv.opt.desktop.addons.wofi` | Wayland application launcher |
| `nerv.opt.desktop.addons.xdg-portal` | XDG desktop portals |
| `nerv.opt.desktop.addons.xresources` | X resources configuration |
| `nerv.opt.desktop.displayManager.gdm` | GNOME display manager |
| `nerv.opt.desktop.displayManager.sddm` | SDDM display manager |
| `nerv.opt.desktop.fluxbox` | Fluxbox window manager with Mod4 (Windows key) bindings |
| `nerv.opt.desktop.gnome` | GNOME desktop environment |
| `nerv.opt.desktop.sway` | Sway window manager |
| `nerv.opt.desktop.xmonad` | XMonad window manager with polybar |

## Gaming

| Option | Description |
|--------|-------------|
| `nerv.opt.gaming.optimization` | Gaming performance optimizations |
| `nerv.opt.gaming.steam` | Steam gaming platform |

## Hardware

| Option | Description |
|--------|-------------|
| `nerv.opt.hardware.audio` | Audio subsystem configuration |
| `nerv.opt.hardware.fingerprint` | Fingerprint reader support |
| `nerv.opt.hardware.networking` | Network configuration and tools |

## Security

| Option | Description |
|--------|-------------|
| `nerv.opt.security.acme` | ACME/Let's Encrypt certificates |
| `nerv.opt.security.dns` | DNS security configuration |
| `nerv.opt.security.doas` | Doas privilege escalation |
| `nerv.opt.security.gpg` | GnuPG configuration |
| `nerv.opt.security.keyring` | System keyring services |
| `nerv.opt.security.pass` | Password store |
| `nerv.opt.security.sops` | Secrets management |
| `nerv.opt.security.vpn` | VPN configuration |

## Services

| Option | Description |
|--------|-------------|
| `nerv.opt.services.avahi` | mDNS/DNS-SD service discovery |
| `nerv.opt.services.cowsay-mastodon-poster` | Mastodon posting service |
| `nerv.opt.services.dex` | Desktop entry execution |
| `nerv.opt.services.openssh` | OpenSSH server |
| `nerv.opt.services.printing` | CUPS printing services |
| `nerv.opt.services.samba` | SMB/CIFS file sharing |
| `nerv.opt.services.tailscale` | Tailscale VPN mesh |
| `nerv.opt.services.writefreely` | Federated blogging platform |

## Suites

| Option | Description |
|--------|-------------|
| `nerv.suites.art` | Digital art applications |
| `nerv.suites.business` | Business/office applications |
| `nerv.suites.common` | Common system utilities |
| `nerv.suites.common-slim` | Minimal common utilities |
| `nerv.suites.desktop` | Desktop environment utilities |
| `nerv.suites.development` | Development tools and languages |
| `nerv.suites.emulation` | Console emulators |
| `nerv.suites.games` | Gaming applications |
| `nerv.suites.media` | Media playback tools |
| `nerv.suites.music` | Music production software |
| `nerv.suites.social` | Social media clients |
| `nerv.suites.video` | Video editing tools |

## System

| Option | Description |
|--------|-------------|
| `nerv.opt.system.boot` | Boot configuration |
| `nerv.opt.system.fonts` | System fonts configuration |
| `nerv.opt.system.locale` | Locale and language settings |
| `nerv.opt.system.performance` | Performance tuning |
| `nerv.opt.system.time` | Time zone and NTP settings |
| `nerv.opt.system.xkb` | Keyboard layout configuration |
| `nerv.opt.system.zfs` | ZFS filesystem with auto-snapshot |

## Tools

| Option | Description |
|--------|-------------|
| `nerv.opt.tools.appimage-run` | AppImage execution support |
| `nerv.opt.tools.bottom` | System monitor (btm) |
| `nerv.opt.tools.comma` | Nix command runner |
| `nerv.opt.tools.direnv` | Directory-based environments |
| `nerv.opt.tools.fup-repl` | Flake update REPL |
| `nerv.opt.tools.git` | Git with delta diff viewer |
| `nerv.opt.tools.go` | Go programming language |
| `nerv.opt.tools.http` | HTTP client tools |
| `nerv.opt.tools.k8s` | Kubernetes tools |
| `nerv.opt.tools.misc` | Miscellaneous utilities |
| `nerv.opt.tools.net` | Network utilities |
| `nerv.opt.tools.nix-ld` | Dynamic library loader for Nix |
| `nerv.opt.tools.qmk` | QMK keyboard firmware tools |
| `nerv.opt.tools.steganography` | Steganography with 7z encryption and image hiding |

## Programming

| Option | Description |
|--------|-------------|
| `nerv.opt.programming.node` | Node.js development |
| `nerv.opt.programming.python` | Python development with uv package manager |
| `nerv.opt.programming.ruby` | Ruby development |

## Virtualisation

| Option | Description |
|--------|-------------|
| `nerv.opt.virtualisation.docker` | Docker container runtime |
| `nerv.opt.virtualisation.kvm` | KVM virtualization |

## Other Modules

| Option | Description |
|--------|-------------|
| `nerv.opt.user` | User account configuration |
| `nerv.home` | Home-manager integration |
| `nerv.opt.nix` | Nix daemon and settings |
| `nerv.opt.persist` | Impermanence configuration |
| `nerv.opt.common` | Common system checks and manuals |