# Rumoca Nix Dev Shell

This directory contains the Nix flake used for the Rumoca development shell:

- `infra/nix/flake.nix`

## Use with direnv from repository root

1. Install `direnv` and `nix-direnv`.
2. Create a top-level `.envrc` in the repository root with:

```bash
use flake ./infra/nix
```

3. Allow it:

```bash
direnv allow
```

## Optional check

From repository root, you can verify it directly with:

```bash
nix develop ./infra/nix
```
