# Efficiency dossier: Vehicles.Rdd2.NavigationEstimator, embedded-c-galec

Every figure below comes from a command whose output is stored beside this file.
`cmd/README.txt` lists the commands; `data/` holds the raw output. Nothing here is
estimated unless the text says so explicitly and states the basis.

**Target hardware.** The deployment board is the NXP MR-VMU-TROPIC: i.MX RT1064,
single Cortex-M7 at 600 MHz, fpv5-d16 FPU, 4 MB flash, 1 MB SRAM with 512 KB tightly
coupled, running Zephyr RTOS. The WCET tables below state times at the 480 MHz the
measurement brief specified; multiply by 0.8 for the RT1064's 600 MHz. The conclusion
is unchanged by the faster clock: the worst-case step is 5.6 to 7.0 ms at 480 MHz,
so 4.5 to 5.6 ms at 600 MHz, against the block's own 1.0 ms declared period, and the
optimistic one-flop-per-cycle bound of 1362 us scales to 1090 us, still over budget.
R1 is therefore required for flight at the declared rate, not an optimization.

**Baseline shift.** These measurements predate the never-concurrent scratch overlay
commit. After it, `sizeof(NavigationEstimatorState)` is 59452 (the proven floor for
region overlay) and the UKF estimator's state is 45452; the flash and instruction
figures are unchanged within 300 bytes. The savings in sections 2 and 6 stack on the
new baseline exactly as computed, since region overlay and the intra-function work
here free different bytes.

## 0. What was measured, and against what

| item | value |
| --- | --- |
| compiler | `/home/jgoppert/git/rumoca/target/release/rumoca`, `build-info` = `9180da0e44e3-dirty`, `--version` = `rumoca 0.10.0` |
| model | `Vehicles.Rdd2.NavigationEstimator` from `/home/jgoppert/git/modelica_models/Vehicles/package.mo` |
| target | `embedded-c-galec` |
| cross toolchain | `arm-none-eabi-gcc (Arm GNU Toolchain 15.2.Rel1 (Build arm-15.86)) 15.2.1 20251203` |
| baseline flags | `-Os -std=c99 -mcpu=cortex-m7 -mfpu=fpv5-d16 -mfloat-abi=hard -ffunction-sections` |
| host toolchain | `gcc (GCC) 15.2.0`, `gcov` 15.2.0, used only for execution counts |

Regenerating the sources with the release binary produced files byte-identical to the
artifacts already in `scratchpad/embed-assess/nav-embedded/` (`cmp` on all four files,
all IDENTICAL). The measured object matches the stated baseline exactly:

```
   text	   data	    bss	    dec	    hex	filename
  63310	      0	      0	  63310	   f74e	nav_m7_Os.o
    168	      0	      0	    168	     a8	kernels_m7_Os.o
  63478	      0	      0	  63478	   f7f6	(TOTALS)
```

`sizeof(NavigationEstimatorState)` on the target ABI is 59560 bytes, of which
`sizeof(NavigationEstimatorScratch)` is 55788 (`data/struct_sizes.txt`, measured by
declaring `char probe[sizeof(T)]` and reading the symbol size out of an ARM object).
The remaining 3772 bytes are the inputs, outputs, parameters and states.
The x86-64 host build reports the same two numbers, so the layout is ABI-stable here.

`nm -u` on the object lists the only external dependencies: `asinf atan2f cosf expf
memcpy memset powf sinf sqrtf` plus the four kernel entry points. No `__aeabi_d*`, no
double-precision math. The single-precision claim holds.

### Corrections to the assumptions in the brief

Three premises in the assignment do not match the artifact at this head, and the
correct numbers change the ranking of the work:

1. **The scratch groups are already unioned.** `ScratchGroup0..7` are `union`, not
   side-by-side structs. Summing the 64 individual per-function scratch structs gives
   148077 bytes; the eight unions total 55788. The overlay already saved 92289 bytes.
   What remains is not "structs that never run concurrently placed side by side"; it is
   one union member, `NavigationEstimatorScratch_step` at 30176 bytes, which holds
   twenty-one 15x15 buffers belonging to seven mutually exclusive branches of one
   `if/else if` chain. See section 2.

2. **The families are 4+4+4+5, not 8+8+8+10.** The source contains 4 `correctLinear`,
   4 `solveSPD`, 4 `josephUpdate` and 5 `symmetrize` specializations, with measurement
   dimension m in {6,3,1,2} and symmetrize sizes {15,6,3,1,2}. At `-Os` gcc inlines all
   four `solveSPD`, all four `josephUpdate`, four of the five `symmetrize` and one
   `correctLinear` into their single callers, so only 4 specialization symbols survive
   in the shipped object, totalling 10854 bytes. That is where the brief's 10.9 KB
   comes from. Forced out of line the 17 specializations cost 14742 bytes.

3. **The corrections are mutually exclusive.** `step` selects at most one aiding
   correction per invocation through an `else if` chain
   (`Vehicles_Rdd2_NavigationEstimator.c:10927, 11025, 11132, 11238, 11344, 11447,
   11556, 11658`). "All corrections firing in one step" cannot happen. The worst case
   is the most expensive single branch, which section 5 quantifies.

### Method for the dynamic numbers

`cov/driver2.c` warms the filter for 40 steps on IMU + mocap + barometer, calls
`__gcov_reset()`, runs exactly one `NavigationEstimator_dostep` with one aiding source
fresh, then `__gcov_dump()`. gcov therefore reports the execution count of every source
line for that one step. `data/dyn.pl` multiplies the per-source-line Cortex-M7 `-Os`
instruction mix (from `objdump -dlr` on the ARM object built with `-g`; `-g` does not
change code generation, the object is byte-identical in size) by those counts.

Floating-point instruction counts obtained this way are exact, because every FP
instruction sits on an expression line whose gcov count is the number of times that
expression runs. The check: the `G*Q*G'` nest in `discreteProcessCovariance` has bounds
15x15x12x12 for the inner accumulation and 15x15x12 for the outer, so 32400 + 2700 =
35100 fused multiply-adds. The measurement reports exactly 32400 and 2700.

Total instruction counts are given two ways. `inner` is exact: `data/loops2.pl` finds
every backward-branch span in the ARM object, counts the instructions in it, and
multiplies by the gcov trip count of the expression line inside it. `model A` multiplies
every line's instruction count by that line's gcov count; it is an upper bound, because
gcc places both the per-iteration test and the once-per-entry setup on the `for` header
line while gcov reports one count for it. The truth lies between the two, nearer `inner`.

---

## 1. Step-path cost

### 1.1 Where the step goes

Instructions attributed to the generated function whose source line range contains them,
so inlined callees are billed to themselves. Worst-case correcting step (GPS position and
velocity, m = 6). This table uses the upper-bound weighting throughout, so the
percentages are comparable across rows. Per scenario in `data/byfn_*.txt`; command in
`cmd/README.txt` item 5.

```
generated function                      insns   %insns   mac(fma)  fp-insn     %fp
discreteProcessCovariance             1470374    44.6%     143100   194626   49.9%
predictPreintegrated                   665161    20.2%      54468    55428   14.2%
josephUpdate_specialization_31         435559    13.2%      63450    63675   16.3%
rumoca_galec_kernels                   197774     6.0%      14727    14727    3.8%
conjugateReset                         159941     4.9%       1215     1215    0.3%
discreteTransition                     129879     3.9%      10800    23625    6.1%
rumoca_galec_compare_lt                 46440     1.4%          0    14792    3.8%
correctLinear_specialization_36         41038     1.2%       2268     2766    0.7%
rumoca_galec_compare_gt                 34047     1.0%          0    10881    2.8%
continuousTransition                    26154     0.8%          0      900    0.2%
solveSPD_specialization_25              19944     0.6%        515      991    0.3%
symmetrize_specialization_18            10404     0.3%          0     1800    0.5%
correctGps                               8656     0.3%        579      736    0.2%
TOTAL                                 3297344   100.0%     292843   389866  100.0%
```

**Which functions dominate multiply-accumulate count.** Three functions hold 260550 of
the 292843 multiply-accumulates in the step, that is 89.0%:

| function | MACs | what it computes |
| --- | ---: | --- |
| `discreteProcessCovariance` | 143100 | `G*Q*G'` then two `Phi*Qd*Phi'` forms for a Simpson-rule integral |
| `predictPreintegrated` | 54000 | `Phi*P*Phi'` |
| `josephUpdate_specialization_31` | 63450 | `F*P*F' + K*R*K'` |

**Fraction that is covariance arithmetic.** Summing `discreteProcessCovariance`,
`predictPreintegrated`, `josephUpdate`, `discreteTransition`, `conjugateReset`,
`correctLinear`, `solveSPD`, `symmetrize`, `continuousTransition`, `noiseInputMatrix`
and `processNoiseMatrix`: **90.0 percent of the step's instructions and 94.2 percent of
its multiply-accumulates** (2966171 of 3297344 instructions, 275816 of 292843 MACs).
Nominal-state propagation, quaternion algebra, gating and status bookkeeping share the
remaining tenth.

### 1.2 Static histogram per symbol

`data/static_hist.txt` holds the full per-symbol static instruction mix from
`objdump -d`. Totals over the 43 symbols in the object: 167 `vmul.f32`, 493
`vmla.f32`, 361 `vadd/vsub.f32`, 99 `vdiv.f32`, **0 `vsqrt.f32`**, 489 `vcmp/vcmpe.f32`,
89 `vabs/vneg.f32`, 97 `vsel*.f32`, 19754 instructions total. Static call sites resolved
through relocations (`objdump -dr`):

```
    533 rumoca_galec_copy_real      83 sqrtf         59 rumoca_galec_fill_real
     51 rumoca_galec_compare_gt     33 memcpy        28 memset
     26 rumoca_galec_compare_ge     22 rumoca_galec_scaled_add_real
     17 cosf    14 sinf    12 atan2f    10 rumoca_galec_dot_real
      3 asinf    1 expf     1 powf
```

Two things stand out. First, `vsqrt.f32` never appears: gcc must preserve `errno` for
`sqrtf`, so all 83 square roots are library calls. Second, `rumoca_galec_copy_real` is
called from 533 static sites, which is the marshalling traffic quantified in 1.3.

### 1.3 Data movement

Measured element operations inside the shared kernels for one step (from the kernel
translation unit's own gcov counts, `data/gcov_*_kernels.gcov`):

| kernel | IMU only | mocap | GPS pos+vel | magnetometer |
| --- | ---: | ---: | ---: | ---: |
| `copy_real` element moves | 3828 | 6276 | 6807 | 6622 |
| `fill_real` element writes | 459 | 774 | 1119 | 1044 |
| `dot_real` MACs | 18 | 1386 | 1386 | 270 |
| `scaled_add_real` MACs | 6777 | 8532 | 13341 | 12216 |

On top of that, model code writes 2637 elements into argument-marshalling buffers and
1482 elements into result read-back buffers in the GPS step, and issues 1431 `memcpy` and
1110 `memset` calls. The pattern that generates all of this is visible around every
protected-function call: the callee leaves its result in its own context region, the
caller copies it out row by row, then copies it again into the next callee's argument
buffer. `correctLinear_specialization_36` performs seven full 15x15 copies (1575 float
moves) between `josephUpdate`, two `symmetrize` calls and `conjugateReset`
(`Vehicles_Rdd2_NavigationEstimator.c:7899-7960` shows the same sequence for m = 1).

---

## 2. Symmetry

### 2.1 What symmetrize actually costs in operations

`symmetrize_specialization_18` (15x15) is called 4 times per correcting step (dynamic
call count, `data/dyn_mocap.txt`). Each call is 225 `vmul.f32` + 225 `vadd.f32` + 450
loads + 225 stores; the measured cost is 2601 instructions per call (10404 over the four
calls), of which 450 are FP arithmetic. Four calls per step is 1800 FP instructions and
10404 instructions in total: 0.5 percent of the step's FP instructions and 0.3 percent
of the step. The m-by-m specializations (6x6, 3x3, 2x2, 1x1) are inlined and cost 36, 9, 4 and
1 elements respectively.

**Removing `symmetrize` alone is not worth doing.** Symmetry is worth doing for two
other reasons: the storage, and the second half of every quadratic form.

### 2.2 Storage

Counting `float x[15][15]` members inside the largest member of each overlay union:

```
group 0  NavigationEstimatorScratch_dostep                            2 x 900 =   1800
group 1  NavigationEstimatorScratch_step                             32 x 900 =  28800
group 2  NavigationEstimatorScratch_correctBarometer                  9 x 900 =   8100
group 3  NavigationEstimatorScratch_correctLinear_specialization_36   9 x 900 =   8100
group 4  NavigationEstimatorScratch_discreteTransition                3 x 900 =   2700
groups 5,6,7                                                          0        =      0
                                                       scratch 15x15 total  =  49500
                    state (stateCovariance + previous_stateCovariance)      =   1800
```

**51300 of the 59560 bytes of the instance, 86.1%, are 15x15 float matrices.**

Upper-triangular packing of a symmetric 15x15 matrix is 15*16/2 = 120 floats = 480
bytes, against 900 bytes dense: 420 bytes saved per slot, 46.7%.

Not all 55 scratch slots are symmetric. Reading the member names
(`data/names.pl` output): all 32 in `step` are covariance values being shuttled between
calls; 8 of the 9 in `correctLinear_specialization_36` are covariances and one is the
Joseph factor `I - K*H`; 8 of the 9 in `correctBarometer` are covariances and one is the
`currentToDelayed` retrodiction matrix; the 3 in `discreteTransition` are `A`, `A^2` and
the transition matrix, none symmetric; both in `dostep` are covariances. That is 50
symmetric of 55 in scratch, plus both state covariances.

**52 slots x 420 bytes = 21840 bytes saved by triangular storage, taking the instance
from 59560 to 37720 bytes, a 36.7% cut.**

### 2.3 The branch-local buffers in `step`

`step`'s 30176-byte scratch is 95.4% 15x15 matrices, and 22 of those 32 buffers live in
exactly one branch of the `else if` chain: three per arm for branches 1 through 7 and
`result244` alone in the else arm. The census is by first and last source line touching
each buffer against the verified branch boundaries at 10927/11025/11132/11238/11344/
11447/11556/11658; every buffer's uses fall strictly inside its owning arm, and the
chain closes at line 11687 after the last use at 11685.

| branch | buffers |
| --- | --- |
| mocap | `rumoca_value_argument_67`, `result69`, `call7562_result53` |
| GPS pos+vel | `rumoca_value_argument_95`, `result97`, `result81` |
| GPS position | `rumoca_value_argument_123`, `result125`, `result109` |
| GPS velocity | `rumoca_value_argument_151`, `result153`, `result137` |
| barometer | `rumoca_value_argument_179`, `result181`, `result165` |
| optical flow | `rumoca_value_argument_207`, `result209`, `result193` |
| magnetometer | `rumoca_value_argument_235`, `result237`, `result221` |
| else arm | `result244` |

Exactly one row is live per step. Overlaying them onto one set of three leaves 19 slots
free: **17100 bytes**, from a purely intra-function liveness argument that the existing
overlay pass already knows how to express, applied to branches rather than to functions.

Applying branch overlay first and triangular packing second: 55 - 19 = 36 scratch slots,
of which about 33 are symmetric, gives 33*480 + 3*900 = 18540 bytes against 49500, a
saving of 30960 bytes; the state covariances add 840 more.
**Instance drops from 59560 to about 27760 bytes, 53.4%.**

This figure is computed group by group from the largest member of each union, so it is an
upper bound: once the largest member shrinks, a different member can set the group size.
The two groups where that matters are group 1, whose second-largest member
`navigationEstimate` is only 168 bytes and so cannot bind, and group 3, whose members
`correctLinear_specialization_44` (10056 bytes) and `_55` (9704) are close behind the
11208-byte `_36` and shrink by the same amount, so the group tracks. Group 2 is the one
to watch: `correctBarometer` at 8632 falls to about 5272 with triangular packing, at
which point `predictPreintegrated` at 7104 would bind unless it is packed too.

### 2.4 Symmetry in the arithmetic

Each quadratic form `A*X*A'` with X symmetric has a symmetric result, so only the upper
triangle needs computing. For the three dominant forms, at n = 15 and m = 6:

| form | as emitted | two-pass | two-pass, upper triangle only |
| --- | ---: | ---: | ---: |
| `Phi*P*Phi'` (predict) | 54000 | 6750 | 5175 |
| `F*P*F' + K*R*K'` (Joseph) | 63450 | 8640 | 6435 |
| `discreteProcessCovariance` (three forms) | 143100 | 18360 | 13950 |

The arithmetic, stated once. For `A(p x n) * X(n x n) * A'`:

* as emitted: `p*p*n*n + p*p*n` MACs, because the inner product `A*X` is recomputed
  from scratch for every column index of the outer product;
* two-pass with one `p x n` temporary: `p*n*n + p*p*n`;
* two-pass writing only the upper triangle of the result: `p*n*n + p*(p+1)/2*n`.

At p = n = 15 that is 54000 against 6750 against 5175, a factor of 8.0 and 10.4.

---

## 3. Specialization families

### 3.1 Are the copies structurally identical

Yes, exactly, modulo the dimension constant and the gensym numbering. Bodies extracted
to `data/{ju31,ju43,ss25,ss42,sym18,sym24,cl36,cl44,cl50,cl55}.c`; all four
`correctLinear` bodies are 707 lines, all four `josephUpdate` 49 lines, all four
`solveSPD` 177 lines, all five `symmetrize` 12 lines.

Normalizing only the specialization index, the `callNNNN`/`rumoca_loop_NN` gensym
numbers and the measurement dimension:

```
ju31 vs ju43   : differing lines = 0 / 49    (raw diff 12 lines)
cl36 vs cl44   : differing lines = 0 / 707   (raw diff 194 lines)
cl36 vs cl50   : differing lines = 0 / 707
cl36 vs cl55   : differing lines = 0 / 707
cl44 vs cl50   : differing lines = 0 / 707
sym18 vs sym24 : 6 / 12   (the residue is the literal 15 vs the parameter)
ss25 vs ss42   : 14 / 177 (the residue is 6 vs 3, 6-1 vs 3-1, 6+1 vs 3+1)
```

The raw difference between two `josephUpdate` copies is 12 lines and every one of them
is a dimension or a name:

```
<     float gain[15][6],                 >     float gain[15][3],
<     float measurementNoise[6][6]) {    >     float measurementNoise[3][3]) {
<     ... contracted_6 <= 6; ...         >     ... contracted_6 <= 3; ...
<     ... contracted_8 <= 6; ...         >     ... contracted_8 <= 3; ...
```

### 3.2 What the copies cost, and what one parameterized function would cost

Per-family flash, measured on a build with `__attribute__((noinline))` on every
generated static function so each appears as its own symbol (`data/nm_noinline.txt`;
whole object 63806 bytes, only 496 more than the shipped 63310, so the inlining gcc does
at `-Os` is close to cost-neutral):

| family | copies | bytes each | subtotal |
| --- | ---: | --- | ---: |
| `correctLinear` | 4 | 3776, 2780, 2732, 2112 | 11400 |
| `solveSPD` | 4 | 688, 668, 572, 276 | 2204 |
| `josephUpdate` | 4 | 216, 212, 208, 180 | 816 |
| `symmetrize` | 5 | 86, 78, 66, 66, 26 | 322 |
| | | | **14742** |

That is 23.1% of the 63806-byte object.

Two families were rewritten by hand as one dimension-parameterized function and compiled
with the identical baseline flags (`variants/joseph.c`, sizes from `nm` on
`data/joseph.o`):

| function | bytes | note |
| --- | ---: | --- |
| `ju_emitted` (fixed n=15, m=6) | 212 | matches the generated copy exactly |
| `ju_param` (n and m are arguments) | 232 | one copy replaces four, saves 584 bytes |
| `sym_emitted` (fixed 15) | 62 | |
| `sym_param` (n is an argument) | 70 | one copy replaces five, saves 252 bytes |

**Operation cost of parameterization: zero.** The measurement is exact:
`ju_param` executes 63450 MACs and 225 adds, the same as `ju_emitted`, and produces a
bit-identical result (`variants/check`: `emitted vs param max abs diff = 0`). What
changes is the loop bound: it is a register instead of an immediate. On the host at `-O2`
this costs a factor of two because the vectorizer loses the trip count (12.85 us against
25.24 us); at `-Os` on Cortex-M7 nothing is vectorized or unrolled either way, and the
inner loop is 7 instructions per MAC in both.

Projecting the other two families from the measured parameterization overhead
(`ju_param`/`ju_emitted` = 1.094, `sym_param`/`sym_emitted` = 1.129): one parameterized
`solveSPD` at about 750 bytes replacing 2204, one parameterized `correctLinear` at about
4150 bytes replacing 11400. **Total projected saving about 9600 bytes, 15% of the
image.** The two family figures marked "about" are projections from a measured ratio,
not measurements; the `josephUpdate` and `symmetrize` figures are measurements.

---

## 4. Block sparsity

### 4.1 The observed structure of H

Every correction builds its measurement Jacobian in the emitted code. The patterns, read
from the source:

| correction | m | raw Jacobian | nonzeros | retrodicted? |
| --- | ---: | --- | --- | --- |
| mocap (4623) | 6 | `[I3 0 0 0 0; 0 0 I3 0 0]` | 6 of 90 | no |
| GPS pos+vel (5369) | 6 | rows 1-3 and 4-6 selectors | 6 of 90 | yes |
| GPS position (6659) | 3 | rows 1-3 selector | 3 of 45 | yes |
| GPS velocity (6936) | 3 | rows 4-6 selector | 3 of 45 | yes |
| barometer (8137) | 1 | row 3 of `rotationWorldBody` in columns 1-3 | 3 of 15 | yes |
| optical flow (9505) | 2 | 2x3 block in the velocity columns | 6 of 30 | yes |
| magnetometer (10020) | 1 | two entries in columns 8 and 9 | 2 of 15 | yes |

The raw Jacobians are between 6.7 and 20 percent dense. Six of the seven corrections then
form
`H = delayedH * currentToDelayed`, where `currentToDelayed` is a dense 15x15
retrodiction matrix, so the H that reaches `correctLinear` is dense by construction. The
sparsity is destroyed on purpose, by the retrodiction, and it costs `m*15*15` MACs to
destroy it:

```
GPS position, measured: line 6659 executes 675 times.
  675 = 3*15*15 fused multiply-adds, of which only 45 have a nonzero left factor,
  because the selector has 3 nonzeros. 630 of the 675 multiply a runtime-computed
  0.0f. The compiler cannot fold them: the zero comes from a `? 1 : 0` on the loop
  index, not from a literal.
barometer, measured: line 8145 issues 15 calls to scaled_add_real(15), 225 MACs,
  of which 45 have a nonzero scale.
```

Literal folding is already complete: grepping the 929289-byte source for `* 0.0f`,
`0.0f *`, `* 1.0f`, `1.0f *`, `+ 0.0f`, `0.0f +` returns 2 hits, both inside conditional
expressions inside `predictPreintegrated` at lines 2190 and 2197, both of the form
`(cond) ? (1.0f * (float)(i == j)) : ...`. What remains is not literal zeros; it is
index-conditional zeros inside loops, which no scalar folding pass can reach without
unrolling the loop.

### 4.2 One representative correction, counted both ways

Mocap, m = 6, the only correction whose H is a genuine selection matrix at the point of
use. Measured MACs for the `correctLinear_specialization_36` symbol in a mocap step:
66233 (`data/dyn_mocap.txt`), which is 63450 for the inlined `josephUpdate` plus 515 for
the inlined `solveSPD` plus 2268 of its own. The `P*H'` product is not inside that figure
because it runs in the shared `rumoca_galec_dot_real` kernel, where its 1350 MACs are
measured. The full correction, with the shape of each stage read from the loop bounds in
`data/cl36.c`:

| stage | shape | MACs as emitted |
| --- | --- | ---: |
| `crossCovariance = P*H'` (in `dot_real`) | 15x6 dot products of length 15 | 1350 |
| `S = H*(P*H')` | 6x6, contraction over 15 | 540 |
| Joseph `F*P*F'` | 15^4 + 15^3 | 54000 |
| Joseph `K*R*K'` | 15*15*6*6 + 15*15*6 | 9450 |
| `solveSPD`, 6x6 Cholesky with a 6x16 right side | | 515 |
| gain application, inject, gating | | 1728 |
| | | **67583** |

Now the same result with structure used. H selects state indices 1,2,3,7,8,9, so
`S*P`, `P*S'` and `S*P*S'` are gathers, and `F = I - K*S` is the identity outside six
columns:

| variant | MACs | ratio |
| --- | ---: | ---: |
| as emitted | 67583 | 1.0 |
| two-pass temporaries only, no sparsity, no symmetry | 12773 | 5.3 |
| two-pass, upper triangle only | 10343 | 6.5 |
| two-pass, upper triangle, H treated as a selection | 6113 | 11.1 |

The upper-triangle row takes the 6x6 innovation covariance `S = H*P*H'` as an upper
triangle too (21*15 = 315 MACs rather than 540), not only the two Joseph forms; the
10343 total is 1350 + 315 + 5175 + 1260 + 515 + 1728.

The last row in full: `P*H'` and `H*P*H'` become 0 MACs, 90 and 36 gathered floats;
`F*P*F'` becomes `P - K*(S*P) - (K*(S*P))' + K*(S*P*S')*K'` at 1350 + 540 + 720 = 2610;
`K*R*K'` at 540 + 720 = 1260; `solveSPD` and the gating unchanged at 515 + 1728.

**What structural sparsity already saved: nothing on the covariance path.** The zeros in
H are consumed by full-density contractions. What remains is 54000 of 67583 MACs in one
dense 15x15 quadratic form that is 8x larger than the same product written with a
temporary, with a further 2x available from its symmetry and a further 2x from the
six-column structure of `F`.

### 4.3 The same pattern outside H

`conjugateReset` builds a 15x15 block-diagonal reset Jacobian with

```c
for (int32_t k = 1; k <= 9;  k += 1)
  for (int32_t i = 1; i <= 15; i += 1)
    for (int32_t j = 1; j <= 15; j += 1)
      if (k == i) { ... }
for (int32_t k = 10; k <= 15; k += 1)
  for (int32_t i = 1; i <= 15; i += 1)
    for (int32_t j = 1; j <= 15; j += 1)
      if (k == i) { ... }
```

Measured trip counts for one mocap step: 2025 and 1350 executions of the two `k == i`
tests, at 37 and 30 Cortex-M7 instructions per iteration, so 74925 + 40500 = 115425
instructions, 4.3% of the whole step, to fill 15 rows. The `k` loop is a pure selector:
the body runs 225 times per value of `k` and writes one row. Recognizing that `k == i`
picks a single row collapses this by a factor of 15.

---

## 5. WCET proxy

The block's declared clock is 1 kHz. `Avionics/package.mo:226` declares
`parameter Real samplePeriod(unit = "s", min = 1.0e-9) = 0.001;` and the generated
`NavigationEstimator_startup` writes `self->samplePeriod = 0.001f` and
`self->clockSamplePeriod1 = 0.001f`. **The step budget is 1000 us.**

Measured per-step cost, one `dostep` each, one aiding source fresh per row:

| branch fired | m | MACs | FP insns | flops | insns (exact inner loops) | insns (upper bound) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| none (IMU only) | - | 211767 | 287419 | 485267 | 1898249 | 2401899 |
| mocap | 6 | 282842 | 374866 | 629576 | 2585998 | 3178600 |
| GPS position+velocity | 6 | 292843 | 389866 | 653986 | 2687571 | 3297344 |
| GPS position | 3 | 286928 | 380699 | 641936 | 2516245 | 3186508 |
| GPS velocity | 3 | 286927 | 380441 | 641935 | 2512498 | 3185668 |
| barometer (gate rejected) | 1 | 220634 | 310951 | 509638 | 2080345 | 2569210 |
| optical flow | 2 | 284121 | 381484 | 636404 | 2657192 | 3187400 |
| magnetometer | 1 | 278849 | 372048 | 625696 | 2732602 | 3361566 |

"flops" counts each fused multiply-add as two operations. "FP insns" counts each FP
instruction once. The barometer row is the one measured case where the innovation gate
rejected. It uses the same m = 1 specialization as the magnetometer row, which was
accepted, so the two are closely comparable: the accepted path costs 652257 more
instructions and 58215 more MACs, which is the `josephUpdate` plus the two 15x15
`symmetrize` calls plus the `conjugateReset` that a rejection skips
(`Vehicles_Rdd2_NavigationEstimator.c:7899-7960`).

**Stated assumption for the first figure, as the brief specifies: one floating-point
operation retired per cycle, 480 MHz.** Worst case is the GPS position+velocity branch
at 653986 flops:

> **653986 flops / 480 MHz = 1362 us.**

That already exceeds the 1000 us budget by 36%, and it is the optimistic bound.

**Second figure, and the one to plan against.** The measured instruction mix says the
`-Os` code retires roughly one FP instruction every seven instructions in its hot loops.
The innermost loop of `discreteProcessCovariance` at `-Os` is seven instructions for one
`vmla.f32`:

```
34: vldmia   r1!, {s12}
38: add.w    lr, lr, #48
3c: ldr      r3, [sp, #12]
3e: vldr     s13, [lr, #-52]
42: cmp      r1, r3
44: vmla.f32 s14, s12, s13
48: bne.n    34
```

At one instruction per cycle, no wait states and no cache misses, 480 MHz:

| branch | exact inner-loop instructions | us | upper-bound instructions | us |
| --- | ---: | ---: | ---: | ---: |
| IMU only | 1898249 | 3955 | 2401899 | 5004 |
| GPS position+velocity | 2687571 | 5599 | 3297344 | 6870 |
| magnetometer | 2732602 | 5693 | 3361566 | 7003 |

> **Worst case is 5.7 to 7.0 ms per step against a 1.0 ms budget: the block as generated
> is 6x to 7x over its own declared period on a 480 MHz Cortex-M7.**

Even the IMU-only step, which runs on every tick, is 4.0 to 5.0 ms.

Sanity check on the same code on a different machine: the host x86-64 build at `-O2`
(AMD Ryzen 9 5950X) takes 51.3 us for the IMU-only step and 79.3 us for the GPS step
(`cov/nav_time`). At the 3.4 GHz reported by /proc/cpuinfo, with AVX2 and four-wide
issue, that is about 174000 and 270000 cycles, an order of magnitude below the ARM
instruction count, which is what vectorizing a scalar contraction eight ways buys. The
same host build at `-O0`, which like the ARM `-Os` build neither vectorizes nor unrolls,
takes 625.3 us and 890.4 us for the same two steps; at the
reported 3.4 GHz that is 2.13M and 3.03M cycles, the same order as the 2.40M and 3.30M
instruction upper bound for Cortex-M7.

The assumptions in the 480 MHz figure are: one instruction retired per cycle; zero flash
wait states, so code runs from ITCM or a perfectly warm I-cache; the 59560-byte state in
DTCM or cached SRAM with no stall on the 15x15 strided column accesses. On a real STM32H7
with the state in AXI SRAM the strided accesses in the quadratic forms will make this
worse, not better. Treat 5.7 ms as a floor.

---

## 6. Ranked implementation plan

Ranking is by measured step-time saved per unit of risk. All savings are against the
worst-case branch (GPS position+velocity, 292843 MACs, 2687571 inner-loop instructions,
5.6 ms).

### R1. Emit `A*X*A'` with an intermediate product

**Saving: 226800 of the 292843 MACs in the worst-case step, 77.4%. Step time falls from
about 5.6 ms to about 2.4 ms.** The three dominant forms drop from 260550 to 33750 MACs
(`discreteProcessCovariance` 143100 to 18360, `predictPreintegrated` 54000 to 6750,
`josephUpdate` 63450 to 8640). The instruction figure: the six innermost quartic loops
measured in `data/loops_gpsboth.txt` hold 1751625 of the step's 2687571 inner-loop
instructions; a two-pass loop has the same shape and the same measured 7 instructions per
MAC, so 33750 MACs cost about 236000 instructions, leaving about 1172000, which is 2.44 ms
at 480 MHz and one instruction per cycle.

Verified numerically: `variants/check` shows the two-pass form agrees with the emitted
form to 1.19e-07 absolute on a result of magnitude 1.11, which is one float ulp.

The lowering currently emits a nested contraction whose inner accumulator does not depend
on the outer tensor index, and recomputes it for every value of that index. The fix is to
hoist the inner contraction into a temporary indexed by the free indices it actually
depends on. This is a general rewrite on the contraction lowering, not a covariance
special case: it fires on every `A*X*A'` in the model.

* **Files:** `crates/rumoca-phase-galec/src/lower/expression_projection.rs`, function
  `lower_materialized_contraction`, which mints the `rumoca_value_contraction_N` and
  `rumoca_value_contracted_N` names and emits the loop nest, with shape helpers in
  `crates/rumoca-phase-galec/src/lower/expression_projection/contraction.rs`. The scratch
  slot for the hoisted temporary is allocated by the same pass.
* **Risk:** medium. Changes results in the last bit. The backend
  interpreter-vs-native differential
  (`crates/rumoca/tests/suite_core/backend_executor_differential.rs`) and the galec and
  fmu suites are the gate. Any golden-file comparison on emitted C will churn.
* **Blocked on:** likely touches
  `crates/rumoca-phase-galec/src/lower/user_functions.rs`, currently owned by the
  Controller item. Coordinate or sequence after it.

### R2. Overlay the branch-local scratch in `step`

**Saving: 16200 bytes of RAM, 27.2% of the instance. No operation change, no numerical
change.** The overlay pass already proves that two functions cannot be active together;
here it needs the same argument within one function across the arms of an `if/else if`.
The 21 buffers and their disjoint line spans are listed in section 2.3.

* **Files:** `crates/rumoca-phase-codegen/src/views/algorithm_code_typed.rs`, which builds
  the overlay groups and carries the proof (see its own test
  `a_read_back_never_reads_the_group_it_writes`), plus
  `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/scratch.jinja` and
  `model.h.jinja`, which print the unions and `CHECKED_SCRATCH_SLOT_BYTES`.
* **Risk:** low, but the checked-overlay proof must be extended, not weakened. The
  emitted `RUMOCA_NAVIGATIONESTIMATOR_CHECKED_SCRATCH_SLOT_BYTES` and the reentrancy
  claim in the header are contracts; both must stay true.
* **Blocked on:** nothing.

### R3. One dimension-parameterized function per family

**Saving: 584 bytes measured for `josephUpdate`, 252 measured for `symmetrize`, about
9600 bytes projected for all four families, roughly 15% of the image. Zero operation
cost, bit-identical results (measured).** Section 3.2.

* **Files:** the specialization/monomorphization step that mints
  `*_specialization_NN`, in `crates/rumoca-phase-galec/src/lower/user_functions.rs`.
* **Risk:** medium-low numerically (results are bit-identical) but high in scope: the
  scratch context of a shared function must be sized for the largest instance, which
  interacts with R2 and with the overlay proof. Sequence after R2.
* **Blocked on:** `crates/rumoca-phase-galec/src/lower/user_functions.rs` is owned by
  the Controller item. This item cannot start until that ownership clears.

### R4. Triangular storage for symmetric matrices

**Saving: 21840 bytes on its own, or 30480 bytes when combined with R2; instance from
59560 to about 28240 bytes, 52.6%.** Also removes the four 15x15 `symmetrize` calls per
step (about 10400 instructions, 0.3 percent) because the result is symmetric by
construction. Sections 2.2 and 2.3.

* **Files:** array layout and subscript projection in
  `crates/rumoca-phase-galec/src/lower/expression_projection.rs`, the scratch slot
  declarations in `crates/rumoca-phase-codegen/src/views/algorithm_code_typed.rs`, and
  the array declaration and subscript printing in
  `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/`. Larger surface than R1
  to R3: every producer and consumer of a symmetric matrix must agree on the packing.
* **Risk:** high. Packing changes the address of every element, so any residual dense
  loop that was not converted silently reads the wrong element. Needs a type-level marker
  for symmetry carried from the front end, not an inference at emission time.
* **Blocked on:** should follow R1, because R1's upper-triangle-only variant is what
  makes the packing pay in operations as well as bytes.

### R5. Treat index-conditional zeros as structure

**Saving: 115425 instructions in `conjugateReset` alone, 4.3% of the step, measured;
plus 675 MACs per GPS-position step and 225 per barometer step in the H construction;
plus, if carried into the Joseph factor, about 2x on `F*P*F'` for corrections whose H is
a selection.** Sections 4.1 and 4.3.

The compiler already folds literal zeros: grepping the emitted source for `* 0.0f`,
`0.0f *`, `* 1.0f`, `1.0f *`, `+ 0.0f`, `0.0f +` returns 2 hits, both inside conditional
expressions. What remains is *index-conditional* structure: `(i <= 3) ? 1 : 0` inside a
loop over `i`, and `if (k == i)` inside a loop over `k` and `i`. These are decidable per
iteration but not foldable without unrolling. Recognizing an index-conditional selector
and lowering it to a gather, or lowering an equality between two loop indices to a
collapsed loop, removes the arithmetic and the iteration count together.

* **Files:** the constant folder and the tensor lowering in
  `crates/rumoca-phase-galec/src/lower/`.
* **Risk:** medium. The analysis must prove the predicate is a function of loop indices
  only. Getting it wrong silently drops terms.
* **Blocked on:** best done after R1, which changes the shape of these loops anyway.

### R6. Build with `-fno-math-errno`

**Saving: 876 bytes of flash and 83 static library calls, measured.** Not a compiler
change: a documentation and integration change.

```
-Os                     text   63310  vsqrt.f32    0  sqrtf-calls   83
-Os -fno-math-errno     text   62434  vsqrt.f32   35  sqrtf-calls    0
```

The 83 library call sites become 35 `VSQRT.F32` instructions; the drop from 83 to 35 is
common-subexpression elimination that the call barrier had been blocking. 23 `sqrtf`
calls run per mocap step, so the dynamic saving is small, but the flash saving is free
and several functions become leaf functions. For reference on the other axis, `-O2` costs
10058 more bytes than `-Os` (73368 against 63310).

* **Files:** the preflight command line printed by the `embedded-c-galec` target and the
  target's documentation.
* **Risk:** none to the compiler; it is a recommendation to the integrator. State that
  the generated code never reads `errno`.
* **Blocked on:** nothing.

### R7. Collapse the read-back and argument-marshalling copies

**Saving: 2637 argument-marshalling element writes and 1482 result read-back element
writes per step, plus 1431 `memcpy` and 1110 `memset` calls, plus 6807 `copy_real`
element moves.** Measured, section 1.3. Roughly 6% of the step is inside the kernels and
about 5% more is `conjugateReset`'s copy and compare traffic. The pattern is a calling
convention choice: results are left in the callee's context and copied out, then copied
again into the next callee's argument buffer. Passing the destination buffer into the
callee, or letting the caller read the callee's context directly when the value is
consumed once, removes both copies.

* **Files:** the `read_back` macro in
  `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja` (line 368),
  the argument-buffer minting in
  `crates/rumoca-phase-galec/src/lower/expression_functions.rs` (lines 874 and 948), and
  the call lowering in `crates/rumoca-phase-galec/src/lower/user_functions.rs`.
* **Risk:** medium. The read-back exists to keep the callee's context region private,
  which is what makes the overlay proof sound. Any change here must keep that proof.
* **Blocked on:** `crates/rumoca-phase-galec/src/lower/user_functions.rs`, owned by the
  Controller item.

### Combined effect

Running instruction budget for the worst-case step, starting from the measured 2687571
inner-loop instructions, 5.60 ms at 480 MHz and one instruction per cycle:

| after | inner-loop instructions | us at 480 MHz | against the 1000 us budget |
| --- | ---: | ---: | ---: |
| as generated today | 2687571 | 5599 | 5.6x over |
| + R1 (two-pass products) | about 1172000 | about 2440 | 2.4x over |
| + R4 arithmetic (upper triangle) | about 1115000 | about 2320 | 2.3x over |
| + R5 (index-conditional selectors) | about 1005000 | about 2090 | 2.1x over |

Those three together take about 63 percent off the step and still leave it about 2x
over budget. Closing the last factor of two needs R7's data movement, which is the largest
single item left after R1 (the kernels and the marshalling copies are about 385000
instructions of the residual), and then a second look at what is left. The honest
statement to the integrator today is that the block does not fit its own declared 1 kHz
period on a 480 MHz Cortex-M7 and will not fit after any single one of these changes.
`samplePeriod` is a tunable parameter, so running the estimator at 200 Hz to 500 Hz is
available as an immediate mitigation while the compiler work lands.

On the memory axis, R2 plus R4 take the instance from 59560 to about 28240 bytes. On the
flash axis, R3 and R6 take about 10.5 KB off the 63.3 KB image; R1 will take more off it,
since a two-pass product is a shorter loop nest than a quartic one.

None of these is a numerical-accuracy improvement, and R1 and R4 both perturb the last
bit. The oracle net is the acceptance gate for all of them.
