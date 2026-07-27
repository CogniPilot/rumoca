#![no_main]

//! Fuzz the Modelica syntax layer.
//!
//! `parse_to_syntax` is documented as infallible: it always returns a
//! best-effort tree plus the recovered parse errors. Any panic, abort, or
//! stack overflow reached from here is a contract violation — the LSP calls
//! this on every keystroke over buffers the user is mid-edit on.

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };
    let syntax = rumoca_phase_parse::parse_to_syntax(source, "fuzz.mo");
    // Walking the tree is part of the contract: a tree that cannot be traversed
    // is as much a defect as no tree at all.
    let _ = syntax.best_effort().classes.len();
});
