//! What the flight objects are allowed to leave undefined.
//!
//! # What this catches
//!
//! Two classes of drift a size ceiling alone would miss, because both can creep
//! in while the artifact still shrinks:
//!
//! * **Heap.** `malloc`/`calloc`/`realloc`/`free` mean the generated code stopped
//!   being statically allocated. There is no allocator in the flight image, so
//!   an undefined allocator symbol is a link failure on the target and a design
//!   failure here.
//! * **Double precision.** The Cortex-M7 this target builds for runs the
//!   generated arithmetic in single precision. One `double` that survives
//!   lowering pulls in the `__aeabi_d*` soft-float helpers and the non-`f` libm
//!   entry points, costing code size and an order of magnitude of runtime. The
//!   undefined-symbol set is exactly where that shows: a `sinf` call leaves
//!   `sinf` undefined, a `sin` call leaves `sin` undefined, and the two are
//!   trivially told apart.
//!
//! # What this does not catch
//!
//! The undefined-symbol set sees a `double` only where the `double` had to call
//! something. The Cortex-M7 the flight rows build for is configured with
//! `-mfpu=fpv5-d16`, a double-precision hardware unit, so a plain `double`
//! multiply compiles to an inline `vmla.f64` and leaves no symbol behind at
//! all. What this check reliably catches on that configuration is a
//! double-precision *library call*, which is where the cost actually is; the
//! `__aeabi_d*` rule is defence in depth for a single-precision FPU
//! configuration (`fpv5-sp-d16`) or a soft-float build, where the same
//! arithmetic does become a helper call.
//!
//! Closing the remaining gap means reading the disassembly for `.f64`
//! instructions rather than the symbol table. The flight artifacts contain none
//! today, so that check would be green if it existed; adding it would widen the
//! toolchain this gate demands, which is a decision for review rather than a
//! silent extension of the contract.
//!
//! # Why a name list rather than a suffix rule
//!
//! "Any libm name that does not end in `f`" is not decidable from the name
//! alone. `erf` is the double-precision error function and ends in `f`;
//! `modf` and `frexp` are double forms whose float siblings are `modff` and
//! `frexpf`. A suffix rule guesses at both. [`DOUBLE_LIBM_FUNCTIONS`] lists the
//! double-precision C99 `<math.h>` entry points by name instead, so a hit is a
//! fact rather than an inference, and a name nobody listed is simply not
//! reported rather than reported wrongly.

use std::collections::{BTreeMap, BTreeSet};

/// Allocator entry points. The flight image has no heap; an undefined one of
/// these is a link failure on the target.
pub(crate) const HEAP_FUNCTIONS: [&str; 11] = [
    "malloc",
    "calloc",
    "realloc",
    "free",
    "aligned_alloc",
    "posix_memalign",
    "strdup",
    "reallocarray",
    "memalign",
    "valloc",
    "_malloc_r",
];

/// Arm EABI soft-float helper prefixes for `double`: the arithmetic and
/// comparison routines (`__aeabi_dadd`, `__aeabi_dcmplt`, `__aeabi_d2iz`, …)
/// and the widening conversions that only exist to feed them (`__aeabi_f2d`,
/// `__aeabi_i2d`, `__aeabi_ui2d`, `__aeabi_l2d`, `__aeabi_ul2d`).
pub(crate) const DOUBLE_EABI_PREFIXES: [&str; 6] = [
    "__aeabi_d",
    "__aeabi_f2d",
    "__aeabi_i2d",
    "__aeabi_ui2d",
    "__aeabi_l2d",
    "__aeabi_ul2d",
];

/// Double-precision C99 `<math.h>` entry points. Each one's `f`-suffixed
/// sibling is the single-precision form the generated code is required to use.
pub(crate) const DOUBLE_LIBM_FUNCTIONS: [&str; 57] = [
    "acos",
    "acosh",
    "asin",
    "asinh",
    "atan",
    "atan2",
    "atanh",
    "cbrt",
    "ceil",
    "copysign",
    "cos",
    "cosh",
    "erf",
    "erfc",
    "exp",
    "exp2",
    "expm1",
    "fabs",
    "fdim",
    "floor",
    "fma",
    "fmax",
    "fmin",
    "fmod",
    "frexp",
    "hypot",
    "ilogb",
    "ldexp",
    "lgamma",
    "llrint",
    "llround",
    "log",
    "log10",
    "log1p",
    "log2",
    "logb",
    "lrint",
    "lround",
    "modf",
    "nan",
    "nearbyint",
    "nextafter",
    "nexttoward",
    "pow",
    "remainder",
    "remquo",
    "rint",
    "round",
    "scalbln",
    "scalbn",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "tgamma",
    "trunc",
];

/// Why one undefined symbol is not allowed in a flight object.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Reason {
    /// Dynamic allocation in an image with no allocator.
    Heap,
    /// A `double` operation reached a soft-float or widening helper.
    DoubleHelper,
    /// A double-precision `<math.h>` call where the `f` form was required.
    DoubleLibm,
}

impl Reason {
    pub(crate) fn describe(self, symbol: &str) -> String {
        match self {
            Self::Heap => format!(
                "`{symbol}` allocates, and the flight image carries no heap: this is a link \
                 failure on the target"
            ),
            Self::DoubleHelper => format!(
                "`{symbol}` is a double-precision soft-float helper: a `double` operation \
                 survived lowering"
            ),
            Self::DoubleLibm => format!(
                "`{symbol}` is the double-precision math entry point; a single-precision \
                 artifact calls `{symbol}f`"
            ),
        }
    }
}

/// One object file's undefined-symbol reading.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ObjectSymbols {
    /// Display name of the object, used in the failure report.
    pub(crate) object: String,
    pub(crate) undefined: Vec<String>,
}

/// One forbidden symbol and the objects it was found undefined in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Forbidden {
    pub(crate) symbol: String,
    pub(crate) reason: Reason,
    pub(crate) objects: Vec<String>,
}

/// Classify one undefined symbol, or `None` when it is permitted.
///
/// A leading underscore is stripped before the heap and libm lookups so a
/// toolchain spelling the reserved alias (`_malloc`, `_sqrt`) is judged the
/// same as one that does not. The EABI prefixes are matched on the raw name,
/// since their leading underscores are part of the name.
pub(crate) fn classify(symbol: &str) -> Option<Reason> {
    if DOUBLE_EABI_PREFIXES
        .into_iter()
        .any(|prefix| symbol.starts_with(prefix))
    {
        return Some(Reason::DoubleHelper);
    }
    // libgcc's own soft-float builtins spell the `double` mode as `df`:
    // `__adddf3`, `__muldf3`, `__extendsfdf2`, `__truncdfsf2`, `__fixdfsi`,
    // `__floatsidf`. They appear instead of the EABI names when a helper is
    // reached through libgcc's naming, and mean exactly the same thing.
    if symbol.starts_with("__") && symbol.contains("df") {
        return Some(Reason::DoubleHelper);
    }
    let bare = symbol.trim_start_matches('_');
    if HEAP_FUNCTIONS.contains(&bare) {
        return Some(Reason::Heap);
    }
    if DOUBLE_LIBM_FUNCTIONS.contains(&bare) {
        return Some(Reason::DoubleLibm);
    }
    None
}

/// Every forbidden symbol across a set of object readings.
///
/// Grouped by symbol and ordered by name, so the same artifact reports the same
/// findings in the same order on every run.
pub(crate) fn scan(readings: &[ObjectSymbols]) -> Vec<Forbidden> {
    let mut hits: BTreeMap<&str, (Reason, Vec<String>)> = BTreeMap::new();
    for reading in readings {
        for symbol in &reading.undefined {
            let Some(reason) = classify(symbol) else {
                continue;
            };
            let slot = hits.entry(symbol).or_insert_with(|| (reason, Vec::new()));
            if !slot.1.contains(&reading.object) {
                slot.1.push(reading.object.clone());
            }
        }
    }
    hits.into_iter()
        .map(|(symbol, (reason, objects))| Forbidden {
            symbol: symbol.to_string(),
            reason,
            objects,
        })
        .collect()
}

/// Undefined symbol names in one `arm-none-eabi-nm --undefined-only` listing.
///
/// Each line is an address column (blank for an undefined symbol), a type
/// letter, and the name. `U` is a strong undefined reference, `w` a weak one,
/// and `v` a weak undefined object: all three link to nothing, so all three are
/// read. Lines naming an input file (`model.o:`) carry no type letter and fall
/// out on their own.
pub(crate) fn parse_undefined(listing: &str) -> Vec<String> {
    let mut names = BTreeSet::new();
    for line in listing.lines() {
        let mut fields = line.split_whitespace();
        let Some(kind) = fields.next() else {
            continue;
        };
        if !matches!(kind, "U" | "w" | "v") {
            continue;
        }
        if let Some(name) = fields.next() {
            names.insert(name.to_string());
        }
    }
    names.into_iter().collect()
}
