# Original conditional-array model qualification

The unchanged 18-model originating roster was replayed with the normal CLI at
`d4e441eb5b539855b88f254cc9127664966360d8`, against the original Modelica 4.1.0
sources. The sealed executable SHA256 is
`a953b50bad7187bc80c7a5ef7b56453ca1083a6ff7605b396a77d1e6cf188403`.
The source seal remained identical before the build and after all model runs.

| Originating class | Checked stage | Passed / executed |
|---|---|---|
| Previously certified ED020 failures | DAE47 emission | 14 / 14 |
| Noncertified ED020 failures | DAE47 emission | 1 / 2 |
| Previously certified reconstruction failures | Solve emission | 0 / 2 |

The producer repair closes all 14 certified DAE failures and the noncertified
`Modelica.StateGraph.Examples.ControlledTanks` failure. The original cases remain
visible. `Modelica.Electrical.PowerConverters.Examples.ACAC.SoftStarter` now
reaches a distinct ED020 refusal: “expected a numeric expression, found
Enumeration”, at `Electrical/PowerConverters/ACAC/Control/SoftStartControl.mo:38`.
`Modelica.Electrical.Analog.Examples.DemonstrateLightning` and
`Modelica.Electrical.Analog.Examples.ResonanceCircuits` still fail checked
index-reduction reconstruction with the original escaped conditional-arm
contract. Those three cases are unclosed.

Every model ran once under the unchanged 600-second, 4-GiB resident-plus-swap,
8-GiB cache, 16-GiB host-reserve and 8-GiB scratch-reserve limits. No model run
was stopped by a resource guard. The normal CLI build passed in 498.27 seconds
with a 2,893,744-KiB peak resident-plus-swap observation. The ownership audit
found no observed target references after completion; denied process reads
remain explicitly unobserved.

This proves checked compilation stages only. It does not establish simulation
correctness, OMC agreement, final-head correctness or green CI. The adjacent
JSON pins every earlier failure, command stage, artifact, raw log and guardian
receipt. Raw evidence and the sealed CLI are retained outside the Cargo target
under `$HOME/scratch/rumoca/workshop427-conditional-operand-20261012/qualification`.
