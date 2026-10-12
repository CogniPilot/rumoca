# Authored Kalman initialization: missing checked LAPACK body

At `ca4962550e4bc9be1077f0ad633471866b956312`, the actual example suite ran
seven tests: five passed and two failed. Fourteen of fifteen declared demo
models compiled. Both failures were the same Kalman producer: ToDae ED019
rejected the initialized protected `m` argument of MSL `LAPACK.dgetrf`.
No Kalman numeric initialization or ODE trace executed.

The two standalone regressions at
`e5388bd4bf2c08e4c20d29751bd3fedb95cb8fb6` then both failed at that same
producer (zero passed, two failed, zero ignored, 0.01 seconds of tests).
[The portable receipt](kalman-lu-profile-before.json) pins the exact commands,
source, raw logs, guardian reports, resource limits and MSL source hash.
The 64 MB historical workshop log includes a Debug source map; it remains
outside the repository. Commit `eb222e6fd` uses the compiler's Display
diagnostic instead.

MSL 4.1.0 `Math/package.mo:2602` implements `Matrices.inv` by calling
`dgetrf`, asserting its status, and calling `dgetri` with the issued factors
and pivots. At lines 7341/7407 those external declarations initialize their
outputs and extent locals from their inputs. MLS §12.4.4 initializes such
bindings before a foreign call. Constant folding cannot currently remove
this computation: the shared evaluator refuses externals outside its
translation-read catalog, and the initial checking-statement owner requires
zero outputs, whereas the authored test returns Boolean.

The proposed DAE-C34 profile uses existing checked function construction.
It proves the exact Fortran entry point, explicit purity, ordered plain
arguments, Real input/output matrix binding and shape, Integer extents,
disjoint typed outputs, pivot-vector shape, physical leading dimension and
workspace. Generic external argument validation stays strict. The native
body retains lower multipliers, upper entries, ordered pivots and the first
zero-pivot status. Inverse construction applies row interchanges once to
the identity and performs compact row-vector forward and shared upper back
substitution. The earlier per-column `LinearSolve` prototype was removed:
the runtime refactors each vector RHS, which would make the inverse quartic.
The replacement has quadratic graph size and cubic arithmetic work.

[Netlib DGETRF](https://www.netlib.org/lapack/explore-html/db/d04/group__getrf_gaea332d65e208d833716b405ea2a1ab69.html)
retains the complete factorization and its singular status.
[DGETRI](https://www.netlib.org/lapack/explore-html/da/d28/group__getri_ga8b6904853957cfb37eba1e72a3f372d2.html)
returns before pivot application when singular and consumes only the first
`n-1` pivots. An adversarial review caught the prototype's incorrect final
pivot read before qualification. Controls now include unused final pivots,
the scalar case, two noncommuting swaps, a later swap of existing multipliers,
first/later singular pivots, invalid consumed pivots and strict ABI refusals.

Candidate `cedf2e6c7318ec6c8049bb0b1394069dd1595aef` is **unrun**.
Its focused selector `native_lu_inverse::` selects ten ordinary `suite_core`
tests, including the unchanged standalone source equations and exact
three-point numeric grids. Shared `native_linear_solve::` and
`native_least_squares::` require after regression runs because the pivot
swap helper and atomic output publication are shared.

The unchanged authored Kalman model is tested through Interpreter and Auto
sessions on eleven exact points from 0 to 0.1, preserving its own estimate
and covariance initialization assertions and the existing ODE error bound.
Each actual session prints its engine/refusal receipt. Algebraic standalone
Auto probes do not establish native execution; no native, browser, OMC,
instrumented coverage, full CI or MSL-cohort pass is claimed here.
