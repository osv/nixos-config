# Creating recovery ISO

 1. Ensure you have enaught space in /
 2. install-iso: `nix build .#nixosConfigurations.install-iso.config.system.build.isoImage`
3. Run:
```
  # Install QEMU if not already installed
  nix-shell -p qemu

  # Run the ISO with QEMU
  qemu-system-x86_64 \
    -enable-kvm \
    -m 4G \
    -cpu host \
    -smp 2 \
    -cdrom ./result/iso/nixos-graphical-installer.iso \
    -boot d
```
