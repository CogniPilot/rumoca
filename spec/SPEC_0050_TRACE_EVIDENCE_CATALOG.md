# SPEC_0050: Trace Evidence Catalog

## Status
REFERENCE

## Summary
Lookup catalog for the trace-production, rejection, typed-exception, and
simulation-soundness rules governed by SPEC_0033 §6a.

## Specification

Rows below are normative by reference from SPEC_0033 §6a and add no independent
requirements.

| Rule | Owner/Where | Brief Justification |
|---|---|---|
| Produced traces MUST have finite nondecreasing time, unique channels, and rectangular data | trace producers | Interpolation needs a valid relation |
| Trace serialization and decoding MUST preserve finite time and channel values bit-for-bit, including adjacent event coordinates and signed zero | trace transport | Rounding can erase an event side and manufacture interpolation error |
| A time regression MUST fail unless the preceding row is settled and the shared predicate proves one semantic instant; retain that settled row unchanged | trace producers | Proximity alone cannot hide lateness |
| At one coordinate, settled replaces initialization, event-left, or nominal; event-left never replaces settled; exact duplicate nominal suppresses without reevaluation | trace producers | Preserve superdense role order |
| Published state events MUST use the common host application coordinate; localization and continuation coordinates stay private | ME host | Solvers cannot change trace semantics |
| The comparator MUST reject malformed time, names, or shape before interpolation and MUST NOT repair evidence | trace comparator | Oracles cannot manufacture evidence |
| For a channel whose Rumoca `phasor` (SPEC_0040 SOLVE-C65) records an angle, the comparator MUST compare by the shortest signed difference modulo `2*pi` and interpolate along the shortest arc. It MAY drop a grid sample of an angle or cosine-of-angle channel only where the phasor magnitude is at most `n_h * min(S_re, S_im)` in both traces (`S` a component channel's normalization scale, `n_h` the largest normalized error of a high channel) and both component channels are compared and high; it MUST NOT drop a sample on the channel alone. A channel the dropped samples leave without a comparable sample pair is listed in `undefined_phasor_channels`, and every phasor channel records its function, components, and dropped samples | trace comparator | An undefined angle carries no evidence; a phase error at any resolved magnitude stays visible |
| The state-selection comparison MUST name each traced state by the source scalar its `state_coordinate` records (formal order `k` as `k` nested `der`), a state without one by its own name, and compare those names with the OMC `init.xml` states; it MUST NOT infer a source from a generated name | state-selection comparator | Coordinate naming must neither hide nor invent a selection difference |
| OMC references MUST be produced with OpenModelica's tool-specific `ModelicaServices` pinned as `[libraries.omc_services]` in `examples/modelica_dependencies.toml` and loaded before the MSL; a session whose `ModelicaServices` source is any other file MUST be refused, the pinned services MUST be part of the reference cache key, and the generic MSL implementation or a host-installed library MUST NOT be used | OMC reference producer | The generic implementation resolves a `modelica://` URI as a file name, so a reference would depend on which host produced it |
| A trace exception MUST be a typed row in `msl_trace_compare_exclusions.json` (schema `msl_trace_exceptions_v2`): kind `impure_source`, `reference_failure`, `model_issue`, `nonidentifiable`, or `comparator_limitation`, a reason, and evidence stating at least one checkable fact (channel counts, tolerances, event times, reference values); a cited repository artifact MUST name the commit that recorded it (an `msl:` documentation artifact needs none); an untyped or unevidenced row MUST fail to load | comparator, review | An exception is a claim reviewed like code; free-form allowances cannot be audited |
| A `comparator_limitation` row MUST name the comparator improvement that retires it (`near_zero_channel_scaling`, `event_instant_alignment`, `aggregate_tolerance_at_annotation_scale`) | comparator, review | The end state is fewer exceptions through a better comparator, not fewer through refusal |
| The exception file a run reads MUST be the one the reviewed baseline boundary pins: the band table records its SHA-256, the quality gate fails when it differs from the boundary's `exclusions_sha256`, and the baseline ratchet reports a changed digest without a boundary pinning it as a regression, so an added or changed row needs a new reviewed boundary with evidence | band table, harness gate, baseline ratchet | An unreviewed row could otherwise retire a roster member |
| Excepted rows remain visible and non-strict-high but are not refinement counterexamples | comparator, reports | Oracle-test boundaries must be auditable |
| Simulation soundness: completions that are neither strict-high nor covered by a typed exception MUST number zero. Until they do, the baseline names each one in `unexcepted_non_high_models`; a current completion outside that roster fails the gate, so the roster only shrinks, and the PR report lists it by triage package. A model joins the roster only through the `roster_additions` of a reviewed reference boundary naming its open defect, facts, and owner | harness gate, baseline, PR report | If it compiles, it simulates correctly; a count alone lets one model replace another |
| The PR report MUST split completed simulations into verified (strict-high), excepted (by exception kind), and unclassified, state compiled models that did not simulate, and define every table column in a legend under its table | PR report | A completion count alone reads as a correctness count |
| Certification timing margin: a strict-high completion enters the baseline's `certified_strict_high_models` and its ratcheted counts (successful simulations, initial-condition successes, models compared including trace-accounted exclusions, strict-high, high+near, no-severe) only when its evidence run used at most half of every phase budget it ran under (the model-attempt budget per worker phase, the solver timeout for the simulation run). Above it, the model stays simulated and reported, uncertified with the typed reason `timing_margin` in `timing_margin_models` (worst phase, budget share); a run compares its own counts against the baseline's counts less those models. The shards and the merge read one budget definition | harness gate, baseline ratchet, CI workflow | Two CI runs of one tree differed by up to 1.73x in per-model phase time, so a certification within that spread of its budget would fail a later run with no regression |
| A model class known to simulate incorrectly MUST be refused at construction (SPEC_0036) | compiler owners | A wrong trace is false success |

## References

- [SPEC_0033 §6a](SPEC_0033_DEVELOPMENT_PROCESS.md#6a-two-tier-verification-cadence)
  — owning development-process rules.
