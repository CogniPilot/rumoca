# Original Euler38 memory and state after receipt

The original full Euler38 memory gate passed on source
`87c16773ef1be4c03f8c61f78e0aed387b2058f8`: one passed, zero failed, zero
ignored. The counted runner accepted the last anchored Rust summary. This
establishes completion under the unchanged 2 GiB child address-space limit,
Rayon1, and default stack. The independent state gate also passed one test,
checking all 5,776 state values at each of the 11 samples and actual Cranelift
receipts without refusals or declines. Both use the same frozen 87c source.
Fresh GitHub CI and landing remain prerequisites for external closure; no issue
closure was performed.

The [qualification ledger](euler425-projection-demand-before-after.md) retains
the earlier d65 allocation abort and independent state timeout. The
[current-core evidence](euler425-d65-current-core-before.md) identifies native
projection compilation as the earlier allocation-failure owner; the focused
before/after demand tests are separate evidence, not substitutes for this model.

## Command and checked input

ROOT executed the following gate in its frozen 87c checkout through the bound
counted runner and guardian, inside `nix develop .#full`:

```sh
python3 "$HOME/scratch/rumoca/slam-integration-cycle-20261010/run-focused-cargo-test.py" 1 \
  cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  process_allocator_address_space::issue_425_original_38_bench_fits_a_two_gib_address_space \
  -- --exact --nocapture
```

The recipe binds the actual Nix Python executable and all 3,972 tracked regular
source files. It uses ROOT's owned `workshop427-ci/target`, `$HOME/.cargo`,
four Cargo/Rayon/test workers, incremental compilation off, and dev/test debug
information off. The test's benchmark child overrides Rayon to one worker.

The test-owned source helper independently pins the unchanged retained original
30×30 fixture and its 38×38 variation. Only `nx` and `ny` change from 30 to 38;
the model name remains `E30x30`. The archival `.mo` files bind the input bytes and
do not replace that helper. No source, dimension, stack, or validator relaxation
was used.

The existing test invokes the built CLI with `sh`'s
`ulimit -v 2097152`, then `sim bench --model E30x30 --t-end 0.1 --dt 0.01
--iterations 1 --warmups 0 --solver rk-like --json`. It requires successful exit,
matching model/solver/settings, 11 reported points, final time 0.1, and finite
nonnegative compile/preparation/hot-run timings. The default native feature path
is retained. Actual Cranelift sample receipts and all 5,776 state values belong
to the separate mandatory state gate, not these benchmark report assertions.

## Actual bounds and outcome

The Rust summary is `1 passed; 0 failed; 0 ignored; 0 measured; 1432 filtered out`,
finished in **96.57 seconds**. The guardian exited zero without a resource stop
after **358.806 seconds**, including Cargo compilation. Its peak aggregate RSS
was **2,931,604 KiB**, including compiler processes; this is not a measurement of
the separately capped benchmark child's RSS. Peak owned cache was
**7,371,771,904 bytes**.

The unchanged outer limits were 600 seconds, 4 GiB aggregate RSS, 8 GiB owned
cache, 8 GiB scratch free floor, and 16 GiB available host memory. The minimum
observed scratch free space was 73,603,485,696 bytes and minimum available host
memory was 45,183,744 KiB. This is one completion proof, not a run-rate or
performance comparison.

## Reproducible bindings

Raw attempt directory:
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-original-euler38-memory-after-root87c-shared-20261011`.
All values below are SHA256. The recipe's historical `UNRUN` scope label was
frozen before execution; the log, guardian and accepted count receipt establish
the actual outcome without rewriting that recipe.

| Binding | SHA256 |
|---|---|
| `recipe.json` | `6c065b8b9a495e8b59fed51814b478a16306464ba1c9c49aac61aa50c8efcb0d` |
| `inputs.json` | `382291f516f3bc2d5850d5a26f904a4eb85899e4283d8f56484b81ed0e3f9917` |
| `original30.mo` | `dccdafed12d656db2c6758b6e0e5f7ca5d85accb89847d6715c4c6f5f5c4b89d` |
| `original38.mo` | `1bd65c874537a23acd43cde3d5d1879d84bf1b1c2affa685df1646d84e8c6a5d` |
| `original-euler38-memory-1.log` | `48660326db03775c3ed0b4a5cd6ea82f064321d35553e452ed98aa7535015e47` |
| `original-euler38-memory-1.guardian.json` | `092c28321f5129dc8790d368079aea5bb0c48e85cec6a0089ea13b31e313bf8e` |
| `original-euler38-memory-1.test-count.json` | `c2b000c67a07411c430b3f611ac7768237614baa54e200ada67f96e1c61ef528` |
| `run-owned-cargo-counted-parallel-safe-20261011.mjs` | `82d4adde12445c2e15e1c432be0d55324e67aebd1b5b6bffb88740015e164d2a` |
| `run-focused-cargo-test.py` | `7347479ec13adb192b6b5210f9b5b33f8e2c3a1b7da17a954d439553b054822b` |
| `guardian-owned-swap.mjs` | `2106ebc8ac92fe4f54ed2789ae3d07ef233330284f9f6a623783df91938dc940` |
| `audit-owned-native-target.mjs` | `6bbdd85b38b0175c253ce5abe843f3303b882799c488e91a77b1c9dc18edcc9f` |

## Independent numerical and backend gate

The independent state attempt is
`pr427-original-euler38-states-after-root87c-shared-20261011`, recipe SHA256
`edcf8ebda58bb458f5e8e6483075020476e05d216c78a151411f8850ceaf2a94`.
It uses the same source/input pins, runner, wrapper, guardian, and outer limits,
with this distinct exact selector:

```sh
python3 "$HOME/scratch/rumoca/slam-integration-cycle-20261010/run-focused-cargo-test.py" 1 \
  cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  reported_bug_runtime::issue_425_original_38_preserves_quiescent_states_and_native_receipts \
  -- --exact --nocapture
```

The existing test retains four 38×38 state tensors, with `r=1`, `mu=mv=0`, and
`E=2.5`. All values must be finite and exactly equal to those unchanged quiescent
expectations at times 0 through 0.1 inclusive in 0.01 increments. Every sample
requires engine `Cranelift`, no refusal, and no declined operations; retained
checked pure helpers must compile without hidden fallback. The test preserves
`RkLike`, its `Auto` execution policy with those mandatory native receipt checks,
and the normal unit-test stack. This independent state gate has no added child
address-space limit; the separate memory gate above supplies that proof.

The last Rust summary is one passed, zero failed/ignored, 1,432 filtered out,
finished in **94.38 seconds**. The count receipt is accepted, child and guardian
exits are zero, and there is no resource stop. Guardian elapsed time was
**99.732 seconds**, peak aggregate RSS **1,798,872 KiB**, and peak owned cache
**7,336,423,424 bytes**. These timings describe these two actual local gates,
not a general performance guarantee.

| State binding | SHA256 |
|---|---|
| `inputs.json` | `1981079a20927c7e105264b98584e709d095f153f4072e17eeb9171d96d5c4db` |
| `original-euler38-states-1.log` | `c49d8cc3dedc2c2dfcaa74cbcdc5e874d165a39655735e7eb72987a28c2f3d3d` |
| `original-euler38-states-1.guardian.json` | `e2a587c81f0a738e3fcefa2bc72e27395256072397188ec677f08a62e49344c9` |
| `original-euler38-states-1.test-count.json` | `1bb707bea3189782d3105c5dfb35a5ad10437a33a2beb397c3ed38cf2bf4466a` |

The frozen state input manifest likewise retains its pre-execution `UNRUN`
label. Its actual log and accepted count receipt are authoritative for the
completed outcome. Historical d65 memory failure and state timeout are retained,
not rewritten as passes.
