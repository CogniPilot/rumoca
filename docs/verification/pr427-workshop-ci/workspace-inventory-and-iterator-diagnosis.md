# Complete workspace inventory and reduction reference order

The frozen `6d62feb2a61e8854c1856e3566470835291151f2` workspace ran all
10,288 selected Nextest cases: 10,188 passed, 100 failed, zero skipped.
Seventy failures were in the GALEC/FMU suite. The complete failure roster,
source and log hashes, and bounded process receipts are in the
[receipt](workspace-inventory-and-iterator-diagnosis.json). This is a failed
baseline inventory, not a repaired workspace or CI result.

The algebraic-family numerical failure differs by one binary64 ULP at
`score[3,6]`. A separate diagnostic preserves the original kernel and checks
both Interpreter and Auto execution. Every recorded gray, gradient, and
squared-gradient value agrees with independent arithmetic; every canonical
residual is exactly zero. The score agrees bit for bit with a reduction
whose normalized comprehension domain has `column` outside `row`.

[MLS §10.4.1.2](https://specification.modelica.org/maint/3.6/arrays.html#constructor-with-several-iterators)
expands a several-iterator array constructor with its last iterator outermost.
Rumoca represents the reduction argument by that constructor and folds its
canonical row-major view. The old test reference put the first iterator
outermost instead. The corrected reference retains the original source and
all exact-bit assertions. The empty rectangular reduction fixture likewise
expects the constructor shape `[3,0]`, rather than `[0,3]`. The triangular
constructor refusal fixture now puts its outer `i` iterator last, so the inner
`j` range can name it; the previous fixture correctly failed name lookup before
reaching the intended nonrectangular-domain diagnostic.

A separate OpenModelica run of the unchanged original kernel reports the
first-iterator-outer score. That one-ULP difference remains recorded: this
receipt does not claim bitwise OMC equivalence or a broader numerical parity
measurement. The test fixes align independent reference arithmetic with
Rumoca's canonical constructor order; they do not loosen tolerances or change
compiler arithmetic.

Raw artifacts remain beneath `$HOME/scratch/rumoca/workshop427-final-broad-20261012/evidence`.
The temporary diagnostic additions are isolated in their own worktree and are
not part of the workshop branch. Combined-source qualification remains pending.
