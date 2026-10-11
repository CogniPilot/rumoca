# Ordinary structured arm roots

The unchanged original scenario test at `6688b285347080fc1529dcede929bff77b4f8f95`
failed for QuadrotorAcro and ReusableBoosterLanding. The temporary diagnostic
probe recorded two distinct owners; [the BEFORE receipt](ordinary-structured-arm-root-before.json)
binds the original source bytes, exact spans, raw log and resource measurements.
The probe must be removed before final production validation.

QuadrotorAcro's nested rate-controller relation (QuadrotorSIL.mo line 239)
was expression 1772, issued and active in arm 7. ToDAE requested a structured
root over the loop domain. The owner rejected the relation as an ordinary
discharged expression, although its live arm was exactly the publishing arm.
Booster's line-1292 deadline failed at a different time-event owner and is
not repaired or claimed qualified here.

The structured-root repair derives activation from the existing canonical
expression/ModelArm owner. The relation and that exact guard must both belong
to the target domain or its lexical ancestors and must have no function scope.
Live construction checks lexical arm visibility; whole-model replay requires
the exact closed conditional. Generic domain-expression publication remains
strict. The existing root representation carries activation, and JSON and
structural replay rederive it through the same checked constructor. No new
wire field, compatibility adapter, arbitrary Boolean authority or pointwise
construction path is introduced. This follows MLS §§3.6.5/8.5 and accepted
SPEC0007 / DAE-C22, SPEC0032 §1 and SPEC0029's shared-owner rule.

Peer constructor controls cover live and closed publication, compact extent
2 versus 1,000,000, JSON/bincode replay, and foreign same-geometry domain,
sibling arm and function-boundary refusals. The public-source control keeps
two loop points with opposite selected branches and independent exponential
state/RHS oracles before their next thresholds. Its three samples deliberately
isolate admission and point activation; it does not claim event-crossing
numerical proof. These AFTER controls remain UNRUN in this source lane.
