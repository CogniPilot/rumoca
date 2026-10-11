# Original TestGlycol directional-owner diagnostic

Production base `d65f1f659`; temporary tracing-only source
`e91d6ec5a55d0e267f6d01be1e89f3788bdf98c3`. The temporary logger must not
land. [Receipt](glycol-directional-e91-actual.json) binds the source, sealed
CLI, original MSL source, recipe, raw trace and all guardians.

The phase-solve tracing check and tracing CLI build passed. The sealed actual
original `Modelica.Media.Incompressible.Examples.TestGlycol` invocation used
`t-end=.01`, `dt=.005` and the pinned MSL4.1.0 source unchanged. It exited1
before simulation and emitted no CSV. Build148.03s/probe15.01s; no guardian
resource stop. The terminal audit saw zero owned-target references and the
runner released its lease. Foreign isolated native tasks remained visible;
2,284 denied reads were explicitly unobserved.

The actual trace first distinguishes these owner decisions (lines55–71):

- Nested `exp`, `Polynomials.evaluate`, `dgelsy_vec`, `leastSquares`,
  `Polynomials.fitting` and `from_degC` have directional owners.
- `Medium.dynamicViscosity`, owner6/call2855/function8, has no directional
  owner. Its call span is2523..2560 in Incompressible.mo. The registered call
  has one assertion, is nonrecursive, and has no active clock. The continuous
  projection explicitly takes the missing-directional fallback.
- Scalar expansion enters the original dynamicViscosity → fitting →
  leastSquares → dgelsy_vec frames. Exact Integer comparison expression1579
  (`Equal`, operands1341/1578, function scope3) then refuses with
  `InvalidCallInterface`, LAPACK source span207504..213334. The final public
  error is the existing EL005 continuous algebraic row2 derivative failure.

This establishes the missing *outer* directional owner and subsequent scalar
fallback chain. It does not establish why the checked derivative producer
withheld that owner. The assertion count is correlation, not a proven cause;
no assertion was disabled and no copied scalar-body repair is authorized by
this evidence. The next discriminator must identify the exact unsupported
operation in that original checked owner, preserve its assertion tuple and
scope, and retain the feature-correct original-model regression.

No simulation, numerical parity, original-model closure or final-tip pass is
claimed. This receipt is diagnostic evidence only.
