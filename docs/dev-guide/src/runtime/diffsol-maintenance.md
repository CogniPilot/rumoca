# Diffsol maintenance fork

Rumoca pins Diffsol 0.13.2 from the
[CogniPilot maintenance fork](https://github.com/CogniPilot/diffsol/tree/d08fec4f592480fcc3d49e4026267242fd83ceca)
through the root Cargo patch and lockfile. The fork preserves every solver
source file from the former vendored dependency; solver behavior, API,
configured tolerances, and native/Wasm feature selection are unchanged.
Later solver corrections are fork commits recorded in its patch inventory.

The fork's [numerical patch inventory](https://github.com/CogniPilot/diffsol/blob/d08fec4f592480fcc3d49e4026267242fd83ceca/RUMOCA_PATCH.md)
records the original upstream revision, published-crate checksum, licensing,
numerical corrections, and existing regression tests. The
[maintenance guide](https://github.com/CogniPilot/diffsol/blob/d08fec4f592480fcc3d49e4026267242fd83ceca/RUMOCA_MAINTENANCE.md)
explains its version and validation scope. New dependency corrections belong
in the fork and require a new immutable revision in Cargo.toml and Cargo.lock.

A workspace Cargo patch does not apply to registry consumers. Publishing
Rumoca crates still requires an upstream release containing these fixes or
an explicitly maintained published dependency. No full MSL/OMC qualification
or upstream-release claim follows from relocating identical source.
