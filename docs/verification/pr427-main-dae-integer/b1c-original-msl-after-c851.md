# Original structured-binder MSL replay after c851

The seven unchanged original MSL 4.1.0 models that failed with
StructuralInvalidBinderScope at the retained fbb CLI now **all complete** their
public short simulation at `c85128c50cd6e65198a712578c80679afc01aeaf`.
The sealed current executable SHA-256 is
`e03b4f4093e4d66aadf57757e791b4959ad559222515e0fc1e98972b25f34f8b`.
The executable was copied outside the Cargo target after its producing gate
terminated. These runs use no live build-cache executable.

Every command retains the original authored file/model, whole pinned MSL source
root, `t_end=0.001` and `dt=0.001`. Each reports exactly two simulation points.

| Original model | BEFORE escaped domain | AFTER variables | Result |
|---|---:|---:|---|
| Clocked.Examples.CascadeControlledDrive.AbsoluteClocks | 5 | 89 | PASS |
| Clocked.Examples.CascadeControlledDrive.Continuous | 5 | 80 | PASS |
| Clocked.Examples.CascadeControlledDrive.SubClocked | 5 | 89 | PASS |
| Clocked.Examples.CascadeControlledDrive.SuperSampled | 5 | 89 | PASS |
| Fluid.Examples.ControlledTankSystem.ControlledTanks | 37 | 543 | PASS |
| Fluid.Examples.Tanks.EmptyTanks | 53 | 154 | PASS |
| Fluid.Examples.Tanks.ThreeTanks | 110 | 285 | PASS |

Names in the table are relative to `Modelica`. BEFORE and AFTER both use the
same seven-model roster. EmptyTanks' first BEFORE attempt hit the scratch guard;
its retained second attempt actually reached the structural refusal. That
resource stop is not counted as semantic evidence.

Actual AFTER evidence lives under
`$HOME/scratch/rumoca/workshop427-ci/evidence/msl-binder-originals-c851`.
`after-evidence-digests.json` SHA-256
`16d28b03b2a0162e14db220c084fa612120d18eab6bc3c873660a8a14b087faa`
binds the issuing source, executable, per-file source hashes and each actual raw
log, guardian receipt and generated HTML. Peak owned group RSS is
1,525,932 KiB; minimum scratch free is 158,362,095,616 bytes. Every child exits
zero with no guard stop. The existing guardian uses 180 seconds, 4 GiB RSS,
8 GiB scratch floor, 16 GiB host-available floor and 8 GiB owned cache limit.

The producer repair preserves the original compact B.1c owner and actual domain
through structural reconstruction. Independent constructor controls establish
structured and observed-owner retention; the public reduced structured-discrete
model also passes at c851. This original corpus replay shows that the same
repair removes the seven former failures through initialization and short
execution. It does **not** establish full trajectories, OMC agreement, a new
MSL baseline, all-target export support or exact-head CI green.
