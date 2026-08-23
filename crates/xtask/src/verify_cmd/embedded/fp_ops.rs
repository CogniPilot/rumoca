//! Counting the single-precision floating-point arithmetic in a flight object.
//!
//! # Why a third number
//!
//! `.text` and `sizeof(<Model>State)` say whether the artifact fits. Neither
//! says whether it runs in time. The step budget on this vehicle is a millisecond
//! and the arithmetic is quadratic-to-quartic in the state dimension, so the
//! number that decides whether the block makes its rate is how many arithmetic
//! FP instructions the step path contains. That number is readable statically,
//! straight out of the disassembly, and it moves for exactly the reasons the
//! rate moves: an unrolled kernel, a re-materialized quadratic form, a
//! contraction that stopped contracting.
//!
//! # The counted set
//!
//! This module is the single owner of what counts. An instruction counts when
//! its mnemonic carries an `f32` component and its stem is in
//! [`COUNTED_ARITHMETIC`]:
//!
//! * the products and fused products: `vmul`, `vnmul`, `vmla`, `vmls`, `vnmla`,
//!   `vnmls`, `vfma`, `vfms`, `vfnma`, `vfnms`;
//! * the sums: `vadd`, `vsub`;
//! * the long-latency pair: `vdiv`, `vsqrt`.
//!
//! The negated forms (`vnmul`, `vnmla`, `vnmls`, `vfnma`, `vfnms`) are in the
//! set because they are the same multiplier work with the sign folded in: gcc
//! selects `vnmul.f32` for `-(a*b)` and `vfnms.f32` for a negated fused
//! product, and a count that missed them would report a saving where the
//! compiler only changed which member of the family it picked.
//!
//! Everything else single-precision is traffic rather than work and is listed
//! in [`EXCLUDED_TRAFFIC`]: the moves (`vmov`), the comparisons (`vcmp`,
//! `vcmpe`), the conversions in all their spellings (`vcvt`, `vcvtr`, `vcvtb`,
//! `vcvtt`), the sign manipulations (`vabs`, `vneg`), the selections (`vsel`,
//! `vmaxnm`, `vminnm`) and the roundings (`vrint*`). Loads and stores never
//! reach this decision at all: `vldr` and `vstr` disassemble without a type
//! suffix.
//!
//! # Why an unlisted `f32` mnemonic is a failure
//!
//! A budget that silently ignores a mnemonic nobody classified would report a
//! fall in arithmetic when the compiler merely started spelling the same work
//! differently, which is the one reading this gate exists to prevent. So a
//! single-precision mnemonic in neither list stops the row and names itself:
//! classifying it is a one-line reviewed change here, in the one place the
//! counted set is written down.

use anyhow::{Result, bail};

/// Single-precision arithmetic: the instructions whose count is the budget.
pub(crate) const COUNTED_ARITHMETIC: [&str; 14] = [
    "vadd", "vsub", "vmul", "vnmul", "vdiv", "vsqrt", "vmla", "vmls", "vnmla", "vnmls", "vfma",
    "vfms", "vfnma", "vfnms",
];

/// Single-precision instructions that move, compare, convert, or select rather
/// than compute. Listed rather than assumed, so a mnemonic nobody has thought
/// about cannot fall through the count as if it were free.
pub(crate) const EXCLUDED_TRAFFIC: [&str; 19] = [
    "vmov", "vcmp", "vcmpe", "vcvt", "vcvtr", "vcvtb", "vcvtt", "vabs", "vneg", "vsel", "vmaxnm",
    "vminnm", "vrinta", "vrintm", "vrintn", "vrintp", "vrintr", "vrintx", "vrintz",
];

/// Thumb condition-code suffixes. A predicated mnemonic inside an `IT` block
/// (`vmulgt.f32`) does the same work as the unpredicated one when it executes,
/// so the suffix is stripped before the stem is looked up rather than being
/// treated as a mnemonic nobody classified.
const CONDITION_SUFFIXES: [&str; 16] = [
    "eq", "ne", "cs", "hs", "cc", "lo", "mi", "pl", "vs", "vc", "hi", "ls", "ge", "lt", "gt", "le",
];

/// How one disassembled mnemonic counts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Class {
    /// Single-precision arithmetic: the work the budget is about.
    Arithmetic,
    /// A single-precision instruction that moves, compares, converts, or
    /// selects: traffic, not work.
    Traffic,
    /// Anything carrying no `f32` component: integer code, addressing, control
    /// flow, and the double-precision forms the symbol policy owns.
    Untyped,
    /// A single-precision mnemonic in neither list, which stops the row.
    Unclassified,
}

/// What one object's disassembly contained.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Tally {
    /// Every instruction line the listing carried, arithmetic or not. Zero of
    /// these means nothing was disassembled, which is a failure rather than a
    /// measurement of zero.
    pub(crate) instructions: u64,
    /// The [`COUNTED_ARITHMETIC`] subset.
    pub(crate) arithmetic: u64,
}

/// Count one `arm-none-eabi-objdump -d` listing.
///
/// Fails on a single-precision mnemonic nobody classified, and on a listing
/// with no instruction in it at all. `context` names the object being read, so
/// either failure says which one.
pub(crate) fn tally(listing: &str, context: &str) -> Result<Tally> {
    let mut instructions = 0_u64;
    let mut arithmetic = 0_u64;
    for mnemonic in listing.lines().filter_map(mnemonic) {
        instructions += 1;
        match classify(mnemonic) {
            Class::Arithmetic => arithmetic += 1,
            Class::Traffic | Class::Untyped => {}
            Class::Unclassified => bail!(
                "`{mnemonic}` in {context} is a single-precision instruction that the \
                 floating-point budget does not classify, so the count would silently omit it. \
                 Add it to COUNTED_ARITHMETIC or EXCLUDED_TRAFFIC in the gate's fp_ops module, \
                 whichever it is."
            ),
        }
    }
    if instructions == 0 {
        bail!(
            "the disassembly of {context} carried no instruction, so nothing was counted. An \
             empty listing is a failed measurement, not a floating-point count of zero."
        );
    }
    Ok(Tally {
        instructions,
        arithmetic,
    })
}

/// The mnemonic on one disassembly line, or `None` when the line is not an
/// instruction.
///
/// `objdump -d` writes an instruction as an address column, the raw encoding,
/// the mnemonic, and the operands, separated by tabs:
/// `   0:\tee00 1a20 \tvmla.f32\ts2, s0, s1`. The section banners, the symbol
/// headers (`00000000 <step>:`) and the blank lines carry no tab-separated
/// address column of hex digits and fall out here.
fn mnemonic(line: &str) -> Option<&str> {
    let mut fields = line.split('\t');
    let address = fields.next()?.trim().strip_suffix(':')?;
    let is_address = !address.is_empty()
        && address
            .chars()
            .all(|character| character.is_ascii_hexdigit());
    if !is_address {
        return None;
    }
    // The raw encoding column, which `-d` always prints and this gate never
    // suppresses.
    fields.next()?;
    let mnemonic = fields.next()?.trim();
    (!mnemonic.is_empty()).then_some(mnemonic)
}

/// Classify one mnemonic as written by the disassembler.
pub(crate) fn classify(mnemonic: &str) -> Class {
    let Some((stem, suffixes)) = mnemonic.split_once('.') else {
        return Class::Untyped;
    };
    if !suffixes.split('.').any(|suffix| suffix == "f32") {
        return Class::Untyped;
    }
    let stem = stem.to_ascii_lowercase();
    for candidate in [stem.as_str(), unpredicated(&stem)] {
        if COUNTED_ARITHMETIC.contains(&candidate) {
            return Class::Arithmetic;
        }
        if EXCLUDED_TRAFFIC.contains(&candidate) {
            return Class::Traffic;
        }
    }
    Class::Unclassified
}

/// `stem` without a trailing Thumb condition code, or `stem` itself when it
/// carries none. Only consulted after the unpredicated spelling failed to
/// match, so `vmls`, whose last two letters are also a condition code, is
/// judged as itself.
fn unpredicated(stem: &str) -> &str {
    for suffix in CONDITION_SUFFIXES {
        if let Some(base) = stem.strip_suffix(suffix)
            && base.len() >= 3
        {
            return base;
        }
    }
    stem
}
