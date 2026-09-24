# Full seed-layout preservation audit — f7819dc1e vs 30e82e22d

**Disposition: identity/band sets preserved; byte-exact numerical preservation fails.** Completed `tier2-seed-layout-f7819dc1e` has the same 566-model roster, 307 compiled models, 288 simulation attempts, 232 IC successes, the exact **211 raw-success model identities** and exact **192 high model identities** as `tier2-torn-readers-30e82e22d`. All 566 band and exit-reason identities match: 192 high, 19 excluded, 278 not attempted, 77 simulation failures; zero missing traces. This is transitive to the published 3b777 baseline for identity sets, per the prior 30e full audit.

All 211 OMC reference trace files match 30e by SHA-256. Of the 211 Rumoca raw traces, **201 match byte-for-byte and 10 differ**; no file is missing or extra. Since 30e raw traces matched ac559 and 3b777 byte-for-byte, the same ten differ from those baselines. Changed raw files (30e hash → f781 hash):

- `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_Steinmetz`: `35bf35b2524601002442893e9d2859ef3a4137a3d4cc7478937c6506f952e0d3` → `b2aaeba2bd2f6c0b627c5b6f4c5a41ec1224da994b60cb0eda7de6aae304cdb3`
- `Modelica.Mechanics.Translational.Examples.Vehicle`: `3c1a8a3ae4e58983ab7dc7033d8c005ae8e59c3c783fd8b9e2fab4dfdf4cd0fd` → `d71f630df9723e9fe94e90995df26efd68834acf1f32df75f73dbe306e2e1273`
- `Modelica.Electrical.PowerConverters.Examples.ACDC.RectifierCenterTapmPulse.ThyristorCenterTapmPulse_RL`: `d3edce2c509a842f0b5191226bdcab8b902f9979448fd740bb1d19b223cdcc09` → `5b680bb8bdabd2d2c52f2df4f887e014cb5f58e8ebffd3548cc29d5ec74f1870`
- `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_DOL`: `d08d6928e2b4f16ec28de1109b8f670ad5e5c8c45beb53941ed4d9f0020d8315` → `10376fcd25335b4416fa40ceadfec8fe87a37928831030cb1d5ffad3e0f102ff`
- `Modelica.Electrical.Machines.Examples.InductionMachines.IMS_Start`: `a05e0579ec66e1a2d7b8c56df7b69da676772958a57278acf258cf79377ae3cf` → `37368de8da9bc0e5df8a1de77ea8b93fab0bb366baf3ebdf603f7a7cb721e3f5`
- `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_withLosses`: `5d4c46d3f4a4e2c06072f9a2d5705c74a0f57503643cff4f62159392a109bdfb` → `0695e69b17ff3a749b743b4d2ab21e266621394f60090eea8391d7a295b8a990`
- `Modelica.Mechanics.MultiBody.Examples.Constraints.SphericalConstraint`: `d75541c93230974123872e530df0755228ed418173e341c767ac741df83dd8bb` → `58f84999bd5eba1cd91d5d08cde83a1a4c27da61380b476355a8c0fb8073fb7f`
- `Modelica.Electrical.PowerConverters.Examples.DCDC.ChopperStepUp.ChopperStepUp_R`: `cdb011a6d8abf00b2eaebc7f4e45ab668d644370e39d6d0c6604db655c0b20bb` → `0ed94095a076a72a53eacdcf2f2c34aed7ce79570bd9792fd81dff020dda41a1`
- `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_Initialize`: `c94e5aca202bc8792118144eef3559598172bc43f745440479c1a0baef3450e4` → `2df5e8f493fad5f40182f1709d9782bf94e017134dced65c27e381e02e5f540e`
- `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_Inverter`: `a86738f3cca252e60ff24f985ce593411342001b0be9e1297cffb5209f1063ac` → `8f4c2803335fdf60dd5a4e5685aebad18367d491bbf26b757c9f233907bf4b47`

**Channel and initial bands:** all 192 compared models remain high. The same 35,961 channels comprise 35,919 high, 42 minor, zero deviation/severe; each model’s compared-channel count and band counts are unchanged. All 35,961 initial channels remain high, and every model’s initial-condition metric record equals 30e exactly. Six model rows have small nonzero score changes, despite unchanged bands:

| Model | Bounded-L1 30e → f781 | Max channel 30e → f781 |
|---|---:|---:|
| IMC_DOL | 5.06243986032362e-7 → 5.06243986032362e-7 | 0.06055236013912452 → 0.060552360139124996 |
| IMC_Initialize | 1.840422088580595e-7 → 1.8415796424569236e-7 | 0.026907922776761895 → 0.026907921406175875 |
| IMC_Inverter | 1.6560526169303375e-7 → 1.6124680743149415e-7 | 0.0000010372256860195046 → 9.984946721268876e-7 |
| IMC_Steinmetz | 0.0000020633645269420123 → 0.0000019355201642242924 | 0.07438677865215004 → 0.07438671389951594 |
| IMC_withLosses | 1.8246546708006186e-7 → 1.824893585809126e-7 | 0.17976647429520948 → 0.1797664746043071 |
| ChopperStepUp_R | 0.000001475525174428587 → 0.0000014755251750708027 | 0.00005494203621129134 → 0.00005494203621129134 |

IMC_Initialize, IMC_withLosses, and ChopperStepUp_R have higher bounded-L1 scores; IMC_Inverter and IMC_Steinmetz improve. IMC_DOL changes only channel mean/max. The cohort mean bounded-L1 improves slightly (1.80763973230468e-5 → 1.80757094940392e-5); this aggregate does not erase the three model-level worsenings or ten byte differences.

**Failure diagnostics:** all failure phases, buckets, codes, and exit reasons remain the same. Eleven timeout error strings change only in measured elapsed seconds. `Modelica.Media.Examples.ReferenceAir.MoistAir1` remains ToDae/absent but its `h_start` diagnostic changes from `type mismatch: expected numeric, got Enumeration / Real` (30e and 3b777) to `division by zero`. The prior 30e audit’s previously documented Inverse_sh_TX/other historical diagnostic differences remain; they are not new high/raw identity losses. The 739c5a513 focus Thyristor timeout remains a separate failed 4/5-raw observation; f781 focus restored five raw traces but does not prove its cause.

No build, simulation, or profile was run for this review. Exact trace-byte preservation and unchanged per-model scores cannot be claimed for f781.

## Publication disposition

**Accept on the stated cohort identity and band gate, with disclosed numerical differences.** Main reports 244 architecture checks, fmt, and an `xtask diff` with zero model/band changes; worker reports seed-LU sampled stacks 97→0. Those reported checks are complementary to this independent artifact comparison. The exact 211 raw-success and 192 high *model identities* are preserved; all compared channel and initial bands remain within the same categories. Ten raw trace hashes change, six model score rows change, and three bounded-L1 scores worsen slightly while staying high. No failure phase, bucket, code, or exit-reason identity changes; `MoistAir1` has a substantive diagnostic-message change within the same ToDae failure. Publication must not describe the new traces or all scores/errors as byte-identical. The prior 739 focus timeout remains in the record and is not causally attributed by these checks.
