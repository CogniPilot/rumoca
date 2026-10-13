# Fourbar test contracts

`omc-reference.csv` is the unchanged output of one independent OpenModelica
`a96aa1a-cmake` simulation of the unchanged wrapper extending MSL 4.1.0
`Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1`. DASSL runs from 0 to 0.1
with 20 output intervals and tolerance 1e-8. It retains OMC's identical duplicate
final sample. The [provenance](provenance.json) pins actual source, compiler,
configuration, result and raw-log digests, plus the 180-second/6-GiB limit.
The attempt completed in 10.93 seconds with 528016 KiB peak RSS.

The native regression uses the existing BDF/default tolerance contract on its
21-point 0.005-second grid. It compares `j1.phi` and `j1.w` against the actual
independent reference at every reference point, within ten times the sum of
native/OMC relative tolerances, scaled by max(abs(reference), 1), plus native
absolute tolerance. This is a focused native trajectory control.

`published-strings.csv` records the complete 27-entry canonical checked public
FMI String inventory and its storage extents. This is a test oracle, not a
second storage-layout rule. The test obtains actual types, names, extents and
declared start-value counts from the compiler-issued `FmiComponent`.

The current FMI-LS profile refuses that complete inventory before publication.
This negative contract gives no component execution or numerical comparison
credit. No variable was dropped, and String accessor support remains pending.
