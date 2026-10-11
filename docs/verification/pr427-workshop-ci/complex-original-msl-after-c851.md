# Three original Complex MSL models after c851

The exact published `21d64ecaa9d205b671598991ee8c4d2f556fed6c` merged MSL
job refused the original BalancingDelta and BalancingStar models in ToDAE:
`Modelica.ComplexMath.real` lacked its checked call-shape certificate.
TestSensors similarly lacked the certificate for `Complex.'*'.multiply`.
The retained `remote-21d/msl-merged.log` SHA-256 is
`d843d6c5ded9beb65c4d435d795f09964403335afd226761310b9fe72001ff16`.

All three unchanged original models now complete their public short simulation
at `c85128c50cd6e65198a712578c80679afc01aeaf`, using the sealed CLI SHA-256
`e03b4f4093e4d66aadf57757e791b4959ad559222515e0fc1e98972b25f34f8b`.
Each command uses its original file, exact entrypoint, whole pinned MSL4.1.0
source root and `t_end=0.001`, `dt=0.001`.

| Model under Modelica.Electrical.QuasiStatic.Polyphase.Examples | Time points | Variables | Result |
|---|---:|---:|---|
| BalancingDelta | 2 | 1104 | PASS |
| BalancingStar | 2 | 1224 | PASS |
| TestSensors | 2 | 2457 | PASS |

Actual AFTER evidence is
`$HOME/scratch/rumoca/workshop427-ci/evidence/msl-complex-originals-c851`.
Its `after-evidence-digests.json` SHA-256
`377ac07abb36311ebabd85942b651d2450b66af887b0e80131e067c434fa9591`
binds exact model source hashes, issuing compiler source/executable and every
raw log, guardian receipt and generated HTML. Peak group RSS is1,571,956KiB;
minimum scratch free is157,202,870,272bytes. All three children exit0 without a
resource stop under the unchanged180s/4GiB RSS/8GiB scratch/16GiB host bounds.

This verifies that the source-owned call-shape repair removes the three actual
originating ToDAE failures and reaches initialization plus short execution.
The independent record/operator unit and public controls at c851 remain
separate evidence. These short runs establish neither full trajectory/OMC
agreement nor a new MSL baseline or exact-head CI success.
