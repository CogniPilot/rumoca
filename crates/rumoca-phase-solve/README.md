# rumoca-phase-solve

Solve preparation phase crate.

Transforms `rumoca-ir-dae::Dae` into `rumoca-ir-solve::SolveIr` as a pure
symbolic phase contract. Runtime backends consume Solve IR; this crate does not
execute simulations.
