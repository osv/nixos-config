# Boomer Package

X11 zoom tool by tsoding. Built with Nim using `buildNimPackage` + `lockFile`.

## Source

- Repo: https://github.com/tsoding/boomer
- Patch: `aa-flashpoint.patch` — anti-aliasing for flashlight mode (from PR #118)

## Updating

### Update source

1. Get latest commit from https://github.com/tsoding/boomer
2. Update `rev` and `hash` in `default.nix`
3. To get new hash: `nix-prefetch-url --unpack "https://github.com/tsoding/boomer/archive/<commit>.tar.gz" | xargs nix hash convert --hash-algo sha256 --to sri`

### Regenerate lock.json

If Nim dependencies change (check `boomer.nimble` for `requires`), regenerate lock file:

```sh
nix build .#boomer.src -o /tmp/boomer-src
nix run nixpkgs#nim_lk -- /tmp/boomer-src > packages/boomer/lock.json
```

The `nim_lk` tool resolves nimble dependencies (currently `x11` and `opengl`) and produces a JSON lockfile with pinned revisions and hashes.

### Verify build

```sh
git add packages/boomer/
nix build .#boomer
ldd result/bin/boomer  # check glibc version
```

## History

Previously boomer came from a stale flake input (`nilp0inter/boomer`) which used old `nimPackages.buildNimPackage`. That API was removed from nixpkgs. Current approach uses top-level `buildNimPackage` with `lockFile`, as documented in nixpkgs `doc/languages-frameworks/nim.section.md`.
