# Projection demand qualification ledger

This ledger distinguishes the small ownership regression from the original
Euler38 memory and state gates. No local branch is described as a published or
merged fix.

| Source | Gate | Actual result |
|---|---|---|
| `18a7e6bdb6b64557e7736566718687f6db75b3cd` | `projection_demand_` before (2 selected) | 1 passed, 1 failed, 0 ignored |
| `87c16773ef1be4c03f8c61f78e0aed387b2058f8` | `projection_demand_` after (4 selected) | Pending |
| same 87c | `prepared_projection_jacobian::` after (10 selected) | Pending; includes the four above |
| d65 | original Euler38 2 GiB memory gate (1 selected) | 0 passed, 1 failed; actual allocation abort |
| d65 | independent original Euler38 state gate | Timeout, exit 124; no samples or completed Rust summary |
| 87c | rebound original Euler38 memory gate (1 required) | Unrun |
| 87c | rebound independent original Euler38 state gate (1 required) | Unrun |

The before producer unnecessarily calls a native projection preparation callback
for a complete construction-issued causal value schedule. The fixture's callback
refuses that preparation, and construction returns `InvalidRow` with the message
`required projection preparation failed`. The independently coupled,
uncertified block's required preparation error still propagates (the passing
control). The guardian exited 101 after 69.292 seconds without a resource stop.
This proves the preparation-demand defect. It does not establish a repaired
Euler38 memory or numerical outcome.

The candidate derives the backend application demand from all issued
uncertified refresh and remainder block inventories, plus the initialization
settled-read cones already issued from its structural patterns. Source-owner,
reverse/forward, seed-domain, and required backend-error checks remain. Its
initialization fixture supplies the actual matching JVP so its cone is derived
from the same owner. See the [consumer map](euler425-projection-demand-candidate.md)
and [current core/timeout evidence](euler425-d65-current-core-before.md).

Raw before evidence under
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-projection-demand-before-root18a7e-20261011`:

| File | SHA256 |
|---|---|
| `recipe.json` | `ac19401680459fa5e7e70b1c6f77a16eee5e6547387d6fe9a9605771276ec163` |
| `projection-demand-before-2.log` | `c6b682148928b85e3236df3a1125b47b12c78c2f382ff84740d1e963f40cf9ba` |
| `projection-demand-before-2.guardian.json` | `1495ec4811766e2214a3fc99e23abe5cd2e86a9ff0314b6b6d7892a439fdd358` |

The pending ROOT after attempt is `pr427-projection-demand-after-root87c-20261011`,
recipe SHA256 `2da78604c8c764011cedddccbbbf709081816ea583fd14fd39114e985a37d5d1`.
Its actual result must be added from terminal raw logs/count receipts, rather
than inferred from source review.

The independent original-model after recipes bind an owned clean 87c snapshot:

| Attempt | Recipe SHA256 | Required count |
|---|---|---|
| `pr427-original-euler38-memory-after-root87c-20261011` | `b290a622b93286af7997caca0a28131473bd1c9b6c7215c28263b8e3ba4d8908` | 1 |
| `pr427-original-euler38-states-after-root87c-20261011` | `a95d05c073881aba0e3f9e3f5a5b48b45e7868796671b13ae1873464d1ab08b2` | 1 |

Both retain the original fixture SHA256
`dccdafed12d656db2c6758b6e0e5f7ca5d85accb89847d6715c4c6f5f5c4b89d`
and the two-extents-only derived38 SHA256
`1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d`.
The test-owned source helper consumes and independently pins those same bytes;
archival copies do not replace its input.

Each uses the counted runner's last successful Rust summary (one passed, zero
failed/ignored), the existing focused count wrapper, and the unchanged 600-second,
4 GiB aggregate RSS, 8 GiB cache, 8 GiB scratch, 16 GiB available-memory guards.
The memory child additionally preserves its 2 GiB address-space limit and Rayon1.
The state test preserves its default stack and all 11 requested samples, checking
all 5,776 state values and actual Cranelift receipts without refusals/declines.
These two independent outcomes are both required before claiming issue #425
fixed; a passing solver control alone is insufficient.
