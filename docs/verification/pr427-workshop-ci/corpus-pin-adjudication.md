# Workshop corpus pin adjudication

Eight stale MSL expectations now describe the compiler's measured behavior at
`5e6f9cb9afec4caf992a370db78c1a5b342d5336`. Six models previously pinned to be
refused simulate successfully. Their 38 probes were checked independently
against OpenModelica before updating the manifest. The focused eight-row gate
then passed in 50.594 seconds. The other 20 entries, the 180-second corpus
budget, and existing tolerances are unchanged.

| Model | New observable coverage | Reference evidence |
|---|---|---|
| Boolean BackSample | Sample, shift and back transfer; 0–0.09 s | 10/10 trajectory channels high |
| Integer BackSample | Sample, shift and back transfer; 0–0.09 s | 10/10 trajectory channels high |
| Counter | Output bit and first increment; 0–1.6 s | Separate OMC 1.6 s, 160 intervals |
| FullAdder | Sum and input transitions; 0–0.02 s | 194/194 trajectory channels high |
| QuadratureLobatto3 | Position, derivative and quadrature; 0–0.02 s | 3/3 trajectory channels high |
| ShowCompositeStep | Initial/composite/parallel activation; 0–1.1 s | 53/53 trajectory channels high |

The longer horizons exercise actual state or clock transitions. Each row pins
movement larger than its tolerance; freezing its trace at initialization would
fail. Discrete probes use `1e-9`. The quadrature readings use the existing
recorder's numeric tolerance convention. Exact-zero initialization selects the
last zero-time OMC row. StateGraph activation is discrete and held; its last OMC
sample before 1.1 seconds is at 1.08 seconds.

The full comparator at this compiler source measured 300 models, with 49
policy skips, one missing reference and zero nonidentifiable traces. All 300
compared models were strict-high, but the aggregate MSL gate failed its budget
and coverage ratchets. Those aggregate failures remain unresolved by these
focused pins. Counter's default 100-second/500-interval OMC reference has a
reviewed output-grid failure, so the separate short reference earns no cohort
parity credit and does not change that policy exclusion.

AutomaticSeed remains refused in Flatten with `EF036`: its parameter attribute
reaches unordered foreign random-library state (SPEC_0040 FLAT-C05 and
SPEC_0022 FUNC-022/FUNC-032, MLS §12.3). Friction remains refused in ToDAE with
`ED001`: 104 scalar equations and 102 unknowns. These update stale `ED019`
expectations; they do not claim that either model is supported or repaired.
Both exact errors already occur in the retained historical `86c3bc1a` run.
No compiler validator or failure exclusion changed.

The [receipt](corpus-pin-adjudication.json) records every checked probe, source
and CSV hashes, refusal scopes, and focused gate hashes. Raw evidence is under
`$HOME/scratch/rumoca/pr427-final-msl-pin-adjudication-20261012`.
The complete 28-row canonical gate and quick/full qualification of the
integrated tip remain pending.
