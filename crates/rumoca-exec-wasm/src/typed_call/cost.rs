//! The cost model behind the size thresholds of the native emitter
//! (SPEC_0040 SOLVE-C103). Every threshold of the emitter is derived here from
//! a stated property of the target, so a row cites one model, not a number.
//!
//! The unrolling bounds are not here: the typed program owns them (SOLVE-C100,
//! SOLVE-C102).

/// Cell moves one bulk-memory instruction (`memory.copy`, `memory.fill`) costs
/// beyond its data: wasm engines lower them to a call into the runtime, about
/// thirty cycles of fixed cost, while one eight-byte move is about one cycle.
/// A transfer of at most this many cells is cheaper as inline moves.
pub(super) const CALL_COST_CELLS: u32 = 32;

/// Bytes of an inline transfer (copy, fill, or a fill loop) that beats a bulk
/// call.
pub(super) const INLINE_MOVE_BYTES: u32 = CALL_COST_CELLS * 8;

/// The locals one wasm function may declare, the limit the engines enforce
/// (V8 `kV8MaxWasmFunctionLocals`). Promotion of scratch cells to locals stops
/// at it: below it the engine's register allocator, which works on live
/// ranges, decides what stays in a register.
pub(super) const ENGINE_LOCAL_LIMIT: u32 = 50_000;

/// Accesses the web analysis of one function may handle. Its walk keeps one bit
/// per definition per control-flow state, so this bound keeps a state under
/// 256 KiB; a larger function keeps its cells in memory.
pub(super) const ANALYSIS_ACCESSES: usize = 1 << 21;
