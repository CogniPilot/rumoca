# Independent preservation identity audit — 2a0ea3aa

Read-only audit, 2026-09-22. No builds, simulations, timed runs, code changes, or roster edits. Only this owned artifact was written. Targeted inputs and SHA-256s are retained below.

**Finding:** fixed194 exactly equals the stated union of main-f477, 67978 and 456 highs, with no duplicates or unsupported member. Latest clean 2a0ea3aa retains 187; exactly seven fail simulation. Its 189 total highs include two outside fixed194: DifferenceAmplifier and NandGate. No additional current high-parity loss found across the inspected cohorts.

**Historical omission:** the retained full 7ddbadf table has 193 high/193 compared and includes NandGate, absent from fixed194. Its trace metadata is dirty and the working-tree digest is f0247eca854dfd7c981bea251696b5d056ca114e3a6c551fb95ab4558bf1c659; this is historical evidence, not a reproducible clean-source baseline. The contemporaneous transition diff independently records NandGate high→simulation failure. PrismaticConstraint, the other departure, already belongs to fixed194 and is high now.

The clean 7ebb and b685 full cohorts independently establish NandGate AND DifferenceAmplifier as high. Therefore an exhaustive preservation obligation across these inspected runs covers **196 identities, 189 currently retained, the same seven missing**. Fixed194 remains a valid frozen original roster, but calling it all historical/campaign successes would be false. Do not delete or rewrite it; separately preserve these later two identities or maintain a documented monotone union. No recovery is claimed from aggregate gains.

## Exact outstanding identities

| Model | Latest phase / failure / seconds | Earlier high proof |
|---|---|---|
| `Modelica.Electrical.Machines.Examples.InductionMachines.IMC_YD` | Success / sim_solver_fail / timeout after 12.000s / 12.019641796 | main-f477d0b69-evidence; 585 compared channels |
| `Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_VoltageSource` | Success / sim_solver_fail / timeout after 12.000s / 12.026279106 | main-f477d0b69-evidence; 697 compared channels |
| `Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL` | Success / sim_solver_fail / timeout after 12.000s / 12.014774222 | main-f477d0b69-evidence; 541 compared channels |
| `Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint` | Success / sim_solver_fail / timeout after 12.000s / 12.113856253 | main-f477d0b69-evidence; 1080 compared channels |
| `Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b` | Success / sim_solver_fail / timeout after 12.000s / 12.088211076 | tier2-67978a5d; 1293 compared channels |
| `Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1` | Success / sim_solver_fail / timeout after 12.000s / 12.07313171 | main-f477d0b69-evidence; 854 compared channels |
| `Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar` | Success / sim_solver_fail / timeout after 12.000s / 12.056990154 | main-f477d0b69-evidence; 872 compared channels |

All seven requests retain simulation timeout=12s. They exactly match the 767 census model keys; SMR has progressed from its old t=0 failure to a timeout. Current `missing_trace_models=0` means no missing comparator inputs, not complete preservation. ArmatureStroke is high in the latest table; the seven remain explicitly open. IMS_Start/Thyristor raw recoveries are excluded, not parity gains.

## Provenance and scheduling limits

Main-f477 CI provenance binds head f477d0b698954b5a70f86286aaae9a3570ef39d5 (tree identical to 00ace1da); downloaded band/trace SHA-256s were independently matched to provenance.json. Its band commit is unknown and trace dirty: preserve that limitation. Later clean full tables and trace metadata are tabulated below. No executable equivalence or causal bisect was inferred from elapsed time.

Canonical full log records stage/sim parallelism4 and requested/effective/spawned/pinned/max_active=4. Frozen worker source sets jobs1, Rayon1, MIMALLOC_ARENA_EAGER_COMMIT=0 and MIMALLOC_PURGE_DELAY=0; canonical processes are pinned. Sequential A/B script uses jobs1 but Rayon4 after Nix, no explicit affinity or allocator override, and fresh one-shot workers. Full canonical request also has explicit_sim_target=false versus diagnostic=true (in addition to timeout12/120, emit_json false/true, source root and output paths).

No evidence here isolates affinity, concurrency, allocator, Rayon, artifact emission, startup/cache state or background load as the cause of a timeout. Matching MSL entry counts alone does not prove byte-identical libraries. Byte-identical published A/B traces do not establish identical internal steps/cost. The sequential pair supports a change under that diagnostic configuration only; it cannot close canonical Planar or quantify canonical speedup. Galileo should bind the next profile to actual canonical daemon settings and source/executable/request hashes; retain original failures and avoid selecting successful retries. No new profile authorized by this audit.

## Machine-readable audit details

```json
{
  "runs": {
    "main-f477d0b69-evidence": {
      "commit": "unknown",
      "trace_commit": "f477d0b698954b5a70f86286aaae9a3570ef39d5",
      "dirty": true,
      "high": 191,
      "outside194": [],
      "compared": 191,
      "excluded": 21,
      "missing_traces": 0
    },
    "tier2-67978a5d": {
      "commit": "67978a5d263e726ddbfb596e4735f7b55d6a2e7c",
      "trace_commit": "67978a5d263e726ddbfb596e4735f7b55d6a2e7c",
      "dirty": false,
      "high": 192,
      "outside194": [],
      "compared": 193,
      "excluded": 22,
      "missing_traces": 0
    },
    "tier2-shared-runtime-456f1619": {
      "commit": "456f161929594ae8d89d866e3c03cb75e2f038f2",
      "trace_commit": "456f161929594ae8d89d866e3c03cb75e2f038f2",
      "dirty": false,
      "high": 187,
      "outside194": [],
      "compared": 188,
      "excluded": 16,
      "missing_traces": 0
    },
    "tier2-observable-7be19ca6": {
      "commit": "7be19ca6cc00b329cd9ce6179dcf7c21962ddb71",
      "trace_commit": "7be19ca6cc00b329cd9ce6179dcf7c21962ddb71",
      "dirty": false,
      "high": 178,
      "outside194": [],
      "compared": 178,
      "excluded": 18,
      "missing_traces": 0
    },
    "tier2-observable-scaling-608ee8b9": {
      "commit": "608ee8b9a835e7d3235d8e05817b29caa0b590fd",
      "trace_commit": "608ee8b9a835e7d3235d8e05817b29caa0b590fd",
      "dirty": false,
      "high": 185,
      "outside194": [],
      "compared": 185,
      "excluded": 17,
      "missing_traces": 0
    },
    "tier2-shared-runtime-ca785215": {
      "commit": "ca7852150e8071144ba2606416a9bf85b0b2b3b0",
      "trace_commit": "ca7852150e8071144ba2606416a9bf85b0b2b3b0",
      "dirty": false,
      "high": 186,
      "outside194": [],
      "compared": 186,
      "excluded": 16,
      "missing_traces": 0
    },
    "tier2-package-initial-767151bdc": {
      "commit": "767151bdc52d875572c0f57664ad2855f98a90c6",
      "trace_commit": "767151bdc52d875572c0f57664ad2855f98a90c6",
      "dirty": false,
      "high": 186,
      "outside194": [],
      "compared": 187,
      "excluded": 17,
      "missing_traces": 0
    },
    "tier2-initial-boundary-499b5533f": {
      "commit": "499b5533fcf2d4d319bce9f80ab2c8e6f898a9d9",
      "trace_commit": "499b5533fcf2d4d319bce9f80ab2c8e6f898a9d9",
      "dirty": false,
      "high": 187,
      "outside194": [],
      "compared": 187,
      "excluded": 17,
      "missing_traces": 0
    },
    "tier2-progress-projection-7ebb651bb": {
      "commit": "7ebb651bb463e8c73dd2f5e7f544ae9e75763e0b",
      "trace_commit": "7ebb651bb463e8c73dd2f5e7f544ae9e75763e0b",
      "dirty": false,
      "high": 189,
      "outside194": [
        "Modelica.Electrical.Analog.Examples.DifferenceAmplifier",
        "Modelica.Electrical.Analog.Examples.NandGate"
      ],
      "compared": 189,
      "excluded": 18,
      "missing_traces": 0
    },
    "tier2-prepared-refresh-b685d26e3": {
      "commit": "b685d26e36063147da4bbcc007294ef22446d38b",
      "trace_commit": "b685d26e36063147da4bbcc007294ef22446d38b",
      "dirty": false,
      "high": 189,
      "outside194": [
        "Modelica.Electrical.Analog.Examples.DifferenceAmplifier",
        "Modelica.Electrical.Analog.Examples.NandGate"
      ],
      "compared": 189,
      "excluded": 17,
      "missing_traces": 0
    },
    "tier2-stage-seed-2a0ea3aa": {
      "commit": "2a0ea3aa7d4d5f54b2f37c8845a41f01d69df871",
      "trace_commit": "2a0ea3aa7d4d5f54b2f37c8845a41f01d69df871",
      "dirty": false,
      "high": 189,
      "outside194": [
        "Modelica.Electrical.Analog.Examples.DifferenceAmplifier",
        "Modelica.Electrical.Analog.Examples.NandGate"
      ],
      "compared": 189,
      "excluded": 19,
      "missing_traces": 0
    }
  },
  "historical_7ddbadf": {
    "commit": "7ddbadf77eb5699580c32cef680c3e1ea3a36fdc",
    "dirty": true,
    "high": 193,
    "outside194": [
      "Modelica.Electrical.Analog.Examples.NandGate"
    ]
  },
  "fixed_roster": 194,
  "retained": 187,
  "latest_high": 189,
  "expanded_obligation": 196,
  "missing": [
    {
      "model": "Modelica.Electrical.Machines.Examples.InductionMachines.IMC_YD",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.019641796,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 585
    },
    {
      "model": "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_VoltageSource",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.026279106,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 697
    },
    {
      "model": "Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.014774222,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 541
    },
    {
      "model": "Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.113856253,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 1080
    },
    {
      "model": "Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.088211076,
      "timeout": 12.0,
      "previous_good": "tier2-67978a5d",
      "previous_band": "high",
      "previous_compared_channels": 1293
    },
    {
      "model": "Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.07313171,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 854
    },
    {
      "model": "Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar",
      "phase": "Success",
      "sim_status": "sim_solver_fail",
      "error": "timeout after 12.000s",
      "seconds": 12.056990154,
      "timeout": 12.0,
      "previous_good": "main-f477d0b69-evidence",
      "previous_band": "high",
      "previous_compared_channels": 872
    }
  ],
  "request_differences": {
    "sim_timeout_secs": {
      "canonical": 12.0,
      "diagnostic": 120
    },
    "source_root_path": {
      "canonical": "/tmp/rumoca-fluid/crates/rumoca-test-msl/../../target/msl/ModelicaStandardLibrary-4.1.0",
      "diagnostic": "/tmp/rumoca-fluid-harness/target/msl/ModelicaStandardLibrary-4.1.0"
    },
    "output_dir": {
      "canonical": "/tmp/rumoca-fluid/crates/rumoca-test-msl/../../target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar",
      "diagnostic": "/tmp/rumoca-fluid-speed-bdf/target/planar-ab-b685-2a0ea3aa/new/worker"
    },
    "emit_json": {
      "canonical": false,
      "diagnostic": true
    },
    "explicit_sim_target": {
      "canonical": false,
      "diagnostic": true
    }
  },
  "artifact_sha256": {
    "/tmp/rumoca-zero-rhs/infra/verification/fluid-preservation-targets.json": "87dc01c6d031cf157506d5045bc643cdbe8e1e578201485811ec2659f0cd0f9b",
    "/tmp/rumoca-fluid/infra/verification/fluid-preservation-targets.json": "87dc01c6d031cf157506d5045bc643cdbe8e1e578201485811ec2659f0cd0f9b",
    "/tmp/rumoca-fluid/target/fluid-campaign/main-f477d0b69-evidence/msl_band_table.json": "7b7c81d69b70fb651a3964f17933af8da5ff63fb4373a4ebe973c3be020a86cc",
    "/tmp/rumoca-fluid/target/fluid-campaign/main-f477d0b69-evidence/sim_trace_comparison.json": "804d8799fd44dc6a63c1e9ffb979ea6aee9ddf09af97e0b81e37e5facd791227",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-67978a5d/msl_band_table.json": "c9f52099c1355b350a8c4dcfcbfeeaa8b077cdb0fa612284ebc97985ca097c15",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-67978a5d/sim_trace_comparison.json": "564c9104da0b37f71c51775e1e871d50ccbabccdfe697026dc26a8dfbdf0062f",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-shared-runtime-456f1619/msl_band_table.json": "6701deebcfc49c02c42e819be64c9a0b91b438fe8586380825b0cd64083448f8",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-shared-runtime-456f1619/sim_trace_comparison.json": "814bfae2536405fbb1e145ac13561a4aae3b0cbaf74f34bdc6aac99d291a2bc0",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-observable-7be19ca6/msl_band_table.json": "934c58622bf6f036700b4be3d6f7f0a443e6c9531c0b0ac47233009ec205f6bb",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-observable-7be19ca6/sim_trace_comparison.json": "d5af01ac3c73a4ae38482ebba3003653ff01ce0adf175822fb90374b00739b98",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-observable-scaling-608ee8b9/msl_band_table.json": "e91e25c850d6ccd791172a1ba3774643cfaf554f7a705c9b12b2b09ff1e34b85",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-observable-scaling-608ee8b9/sim_trace_comparison.json": "7192d3663890b949d53abcd05037fb322e1d25cddd592b6f43802a779f1393ae",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-shared-runtime-ca785215/msl_band_table.json": "18db06a05745666cf53de857726f117efa05e9a8d3f12efb9c6b59cd123e3ccf",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-shared-runtime-ca785215/sim_trace_comparison.json": "1dd7244620bf6f171b14f793d94740d6b86aad3481dd9a73be923a6be7967bef",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-package-initial-767151bdc/msl_band_table.json": "ecbe0f7734455e2101edbaec4824aaa0e6898efc3f21f456d13cc69e955ab169",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-package-initial-767151bdc/sim_trace_comparison.json": "567d4188f358caf5f1ba3fc9d9a21d16f86aa33dcedb19c084f0a5bd8d78d5ae",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-initial-boundary-499b5533f/msl_band_table.json": "fac574f6f33aca7ecf2a93e0ee2c422051ac7544c3d702dd69dd5386072528f7",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-initial-boundary-499b5533f/sim_trace_comparison.json": "c5789e1292f9485cc5f635c7a56c9233ea197c613b5652da7535f358d8c9bc11",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-progress-projection-7ebb651bb/msl_band_table.json": "797bb7e2b775e9c9bfad0a5d35a3bb06f941897a46165da52060206555534b07",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-progress-projection-7ebb651bb/sim_trace_comparison.json": "aeca0f8bc259de3f18f1e110172650e352f1103217b225938ea965347695ae44",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-prepared-refresh-b685d26e3/msl_band_table.json": "05bf7c3cdb340a79964fc79c26a903dac3d77553a38100b4cd1d6b84c1c93056",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-prepared-refresh-b685d26e3/sim_trace_comparison.json": "d08b26e0222bef0a5bd77e6339f15e4bf9d100407b1119ad763a9bc2ca28599a",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/msl_band_table.json": "938bac678b2439ec182192dd5865d2e583779b10207a6bab30003ea8ea11ee3c",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/sim_trace_comparison.json": "4e25fbe0d10c251c5b60911967e837a4c74ad0f17b7636ff4aa3e79aaf78731b",
    "/tmp/rumoca-zero-rhs/infra/verification/preservation-census-767-evidence.json": "584d9874ec0752c1b0fa09d346fef7af9ec3ae97d7bdb34d563de802c13a45c8",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/msl_results.json": "b8314c9e9dc511a550b6b0a9d1c924007f828d2556543c88a8768a3fe3842d5a",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.InductionMachines.IMC_YD/result.json": "5755c623afe31cd4a4e4884ba40ff3de89010fbc4ca7682392523e29b14ac183",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.InductionMachines.IMC_YD/request.json": "b9f414c620737e46831af86517893ed348e8057703639e7627aed16fe0b435f3",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_VoltageSource/result.json": "3425a6bbe8c9334a3437602bc05a7b4f733f99bb397d4cf2466d0ebb1b4b70f1",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMPM_VoltageSource/request.json": "eb41879aeeadc03130495f7c60731f4a13efca61e95094e06b4c4956b2ee02d5",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL/result.json": "e403b15f19e20858335e5aec1605775da2c5098c219d5d469621ac1e95e0ad05",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Electrical.Machines.Examples.SynchronousMachines.SMR_DOL/request.json": "637101497b0dd8535a816682857409a9510c2155cb5ec56ffb48ec7687fbe76d",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint/result.json": "d15cda8215de0b1d6e433e4c978fcee3ef625d3f92c3bfb4001b9349fb1cf8aa",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint/request.json": "11c9ea95ceb3fdbe4c5a8a961995ba12d400906c67110a305d7f7ced6ee8f74c",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b/result.json": "9e58fe1efa1f2ac63aadb4697ad46c0f05917f0fd0cdf89ddaeb5ee06709b322",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.Engine1b/request.json": "95787e46ecd534993afbe38dc2d79e7e55e1ff45ae58da3ad4707fdd1d489e01",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1/result.json": "b490d04c66b30d697f2734d6abb0c4eb211e431b6b243ab086f318853e260655",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1/request.json": "f4b1b0be4030079801125c7fd7669eaddcc95b2c2897ca2403fe37b509167036",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar/result.json": "5f2b6b2924ecd73467a754add200f7563e375ac8971953499803308dfaee9646",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa/model_worker/Modelica.Mechanics.MultiBody.Examples.Loops.PlanarFourbar/request.json": "cad04f0dce3634b9048a0b24fece2c93fdb808c9300ad518e61b4adda544ff7f",
    "/home/jgoppert/git/rumoca/target/msl/results/msl_band_table.json": "803e8910028dbf5c05b76c1bad199ad7569a7bc7cc962db0ac77b0f8b2ed71ed",
    "/home/jgoppert/git/rumoca/target/msl/results/sim_trace_comparison.json": "a9e0c52073273ea406e0af70f217e3e777617b47ee13c1d18516b7a4fb44e642",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-historical-7ddbadf-to-67978-diff.json": "57944c95c39576c09dde3f163d76bb0f1c562840bbbe9fc71c892033186990da",
    "/tmp/rumoca-fluid/target/fluid-campaign/main-f477d0b69-evidence/provenance.json": "435f43a068101407960c3e594556aaa09d0773b449429231888ee60820c7b589",
    "/tmp/rumoca-fluid-speed-bdf/target/planar-ab-b685-2a0ea3aa/new/request.json": "307138e8e45a8ef633780d97232ece5e3e161eb35e9cd080c3cc3bfbe45dfd4a",
    "/tmp/rumoca-fluid/target/fluid-campaign/stage-seed-planar-ab-review/scheduling-provenance.md": "c9bdb73f1e3f868003ef3041f1631945f7a8b94ad133bb7e3db0dc49b04f9ede",
    "/tmp/rumoca-fluid/target/fluid-campaign/stage-seed-planar-ab-review/run-sequential.sh": "2d238ca182a21a8136939d70906aed7fcf5ca8d79d3e39ef06e50aadd99ee39b",
    "/tmp/rumoca-fluid/target/fluid-campaign/tier2-stage-seed-2a0ea3aa.log": "8a61fdfc4716c39276472351b1592950ccd4208f0b5b5a46778205bb92e6dfba",
    "/tmp/rumoca-fluid/target/fluid-campaign/stage-seed-planar-ab-review/comparator-provenance.json": "0ddc4d2b61be5545429bb0a77e1d4593fe22696744f224857e3dfc24660c6508",
    "/tmp/rumoca-fluid/target/fluid-campaign/stage-seed-preservation-audit.json": "9daf61452a16d92a02fa686559e11bfc2c2c75b405a46c84dafe357a574a4fea",
    "git:2a0ea3aa:crates/rumoca-worker/src/lib.rs": "6676ef60d5328c634ff238874b36f4038f567a44a4beb58475458cea6fc6f44f"
  }
}
```
