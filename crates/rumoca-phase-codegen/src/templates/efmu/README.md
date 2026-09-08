# `efmu`

## Use case

Emit one correlated eFMI product containing checked Algorithm Code and its
Solve-owned Production C refinement.

## Contract

Every C/H file consumes only the prepared `SolveAlgorithmBlock` view retained
with the exact `AlgorithmCodePackage`; the GALEC package never emits C.

## Unsupported

The initial profile rejects every lifecycle expression, call, branch, tensor,
effect, or ABI case not represented by the sealed Production plan.

## Verification

Focused tests compile and execute rendered C from the checked correlated
product as a syntax and integration witness. The artifact remains experimental
and cannot claim authenticated Production-C or ECM-003 numeric correctness:
there is not yet a checked SPEC_0047 §4.31 toolchain receipt proving
`FLT_EVAL_METHOD == 0`, absence of excess precision, disabled contraction, the
required floating-environment/status behavior, and trap behavior for the exact
compiler and flags. Full eFMI schema and benchmark gates remain required for a
production conformance claim after that receipt exists.

## Example

Run `rumoca build --target efmu Model.mo` after the model passes the target's
explicit eFMI admissibility profile.
