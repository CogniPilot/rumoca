# Shared-layout Tier 1 audit: d0f1501a versus ce1a85bc

The terminal marker reports `focus=1`, `canary=0`. This is an observed focus preservation loss; d0f1501a remains held unpublished. No retry was made.

| Scope | Exact comparison |
| --- | --- |
| Focus roster | Same five models; `sim_ok` falls from 5 to 4. ThyristorCenterTap2Pulse_RLV_Characteristic changes from `sim_ok` to `sim_solver_fail`, with `timeout after 12.000s` (candidate run 12.011232517 s; baseline 11.882159980 s). Its previously completed raw trace, SHA-256 `bcd5ebc4dcc2da57efffc893dbe1ed60fc8ce866198fa0f3fe8af1c2f84892f7`, is lost. |
| Surviving focus traces | Four of five Rumoca raw traces and all five OMC reference files are byte-identical. The three compared models remain high: 1256/1256 channels and 1256/1256 initial channels high; score, band, and initial records are unchanged. Policy exclusions fall 2→1 only because Thyristor failed before comparison; the remaining DemoPowerSupplyWithBuffer exclusion is unchanged. |
| Canary | Same 20-model roster and statuses, with 9 `sim_ok`. All nine Rumoca raw traces and nine OMC references are byte-identical. All nine comparisons remain high, including 175/175 channels and initial channels; score, band, and initial records are unchanged. No exclusion or missing comparison. |

Fourbar single-observation timing (ce1a85bc → d0f1501a): simulation 5.977719235 → 6.828176395 s; simulation build 9.999333088 → 10.890023108 s; compile 1.926467177 → 2.498251824 s; simulation wall 16.133088221 → 17.956383475 s. These observations establish neither a speed gain nor timeout causality.

Sources: `/tmp/rumoca-fluid/target/fluid-campaign/shared-layout-tier1-d0f1501a.exit` and the `paired-preservation-focused-{ce1a85bc,d0f1501a}` and `canary-{ce1a85bc,d0f1501a}` campaign artifacts in the same directory.
