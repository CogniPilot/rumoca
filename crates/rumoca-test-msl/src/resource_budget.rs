//! Per-model resource ceilings for the MSL parity harness.
//!
//! # Acceptance contract
//!
//! This module is a *rejection*, so it states first what it accepts (SPEC 0008).
//! For one model attempt the harness accepts:
//!
//! * **Any** Solve IR whose serialized (JSON) form is at most
//!   [`SOLVE_IR_SIZE_LIMIT_MB_DEFAULT`] MB — 32 MB, which is orders of
//!   magnitude above what an MSL model produces in practice and still under the
//!   ~34 MB a *single* CMM `log_map` call reaches. Nothing about the model's
//!   shape, node kinds, op counts, or nesting depth is judged: only the byte
//!   count of the artifact the pipeline has to carry.
//! * **Any** compile that finishes within
//!   [`MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT`] seconds of total wall time
//!   summed across every compile phase — 40 s, four times the 10 s per-phase
//!   budget a single phase is allowed, so a model may be slow in every phase at
//!   once and still be accepted.
//! * Both ceilings raised (never lowered) per run through the parity config,
//!   exactly like `--sim-timeout-secs`: a config that asks for a *smaller*
//!   budget is clamped back to the default, so a mis-written config can never
//!   make the gate easier than the committed baseline was measured with.
//!
//! What is rejected is only the complement: a model whose Solve IR does not fit
//! in the declared budget, or whose compile does not fit in the declared wall.
//! A rejection is
//!
//! * **loud** — the attempt fails with a rendered message naming the observed
//!   size/time, the ceiling, and the flag that raises it;
//! * **attributed** — it carries [`ModelFailureBucket::ResourceBudget`], whose
//!   owner is `Performance`, plus a stable `EMSL_BUDGET_*` error code, so the
//!   cohort is greppable and never lands in `Unclassified`;
//! * **never a hang** — [`SolveIrSizeBudget::measure_serialized`] streams into a
//!   counting sink and stops at the first byte past the ceiling. Measured on
//!   the CMM repro: a model with four `LieGroups.SE23.Quat.log_map` calls
//!   serializes to 142 009 382 bytes (135 MiB) in 332 ms unbudgeted, and is
//!   rejected in 83 ms having touched 32 MB — the same 83 ms the one-call
//!   (~34 MB) model costs, so the measurement time does not grow with the
//!   blowup it is there to catch.
//!
//! # Where the ceilings are enforced
//!
//! The Solve-IR ceiling is enforced wherever the harness itself serializes
//! Solve IR (`rumoca-sim-worker`, which measures on every run whether or not an
//! artifact was requested) and, post hoc, over any Solve-IR artifact a lane
//! wrote to disk. The persistent model-worker lane keeps Solve-IR artifacts off
//! for speed, so there it is the on-disk check that is vacuous and the worker's
//! own memory ceiling that bounds the model; the compile-wall ceiling applies to
//! every lane because every lane reports `compile_seconds`.

use std::io::{self, Write};

use rumoca_worker::ModelFailureBucket;
use serde::Serialize;

/// Default per-model Solve-IR serialized-size ceiling, in MB.
///
/// Motivating case: a single `LieGroups.SE23.Quat.log_map` call from the cached
/// CMM snapshot scalarizes into a Solve IR whose JSON form is ~34 MB, and four
/// of them into ~142 MB. Nothing in MSL legitimately needs a fraction of that,
/// so 32 MB leaves ample headroom over real models while catching a
/// scalarization blowup before it becomes an OOM or an unattributed watchdog
/// kill.
pub const SOLVE_IR_SIZE_LIMIT_MB_DEFAULT: u64 = 32;

/// Default per-model *total* compile wall ceiling, in seconds.
///
/// The per-phase watchdog already kills a model that spends more than 10 s in
/// any one phase. It cannot see a model that spends 9 s in each of five phases,
/// which is the shape a lowering blowup actually has. This ceiling is the
/// per-phase budget times the four-phase compile pipeline, so it only fires on
/// a model that is slow *everywhere*.
pub const MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT: f64 = 40.0;

/// Stable code for a Solve-IR serialized-size overrun.
pub const SOLVE_IR_SIZE_BUDGET_ERROR_CODE: &str = "EMSL_BUDGET_SOLVE_IR_SIZE";

/// Stable code for a total-compile-wall overrun.
pub const COMPILE_WALL_BUDGET_ERROR_CODE: &str = "EMSL_BUDGET_COMPILE_WALL";

const BYTES_PER_MB: u64 = 1024 * 1024;

/// Clamp a configured ceiling so it can only *raise* the budget.
///
/// Mirrors the `--sim-timeout-secs` policy: a run may buy a bigger budget for a
/// diagnostic lane, but a config can never shrink a ceiling below the value the
/// committed baseline was measured with. Non-finite and non-positive values are
/// ignored rather than silently treated as zero.
#[must_use]
pub fn raise_only_mb(configured: Option<u64>, floor_mb: u64) -> u64 {
    configured.map_or(floor_mb, |value| value.max(floor_mb))
}

/// Raise-only clamp for a seconds-valued ceiling. See [`raise_only_mb`].
#[must_use]
pub fn raise_only_secs(configured: Option<f64>, floor_secs: f64) -> f64 {
    configured
        .filter(|value| value.is_finite() && *value > 0.0)
        .map_or(floor_secs, |value| value.max(floor_secs))
}

/// A Solve IR that did not fit the declared serialized-size budget.
///
/// `observed_bytes_at_least` is a *lower bound*: measurement stops at the first
/// byte past the ceiling, so the true size is unknown and deliberately so —
/// finding it out is exactly the cost the ceiling exists to avoid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveIrSizeBudgetExceeded {
    /// Bytes written before measurement stopped (`> limit_bytes`).
    pub observed_bytes_at_least: u64,
    /// The ceiling that was exceeded, in bytes.
    pub limit_bytes: u64,
}

impl SolveIrSizeBudgetExceeded {
    /// The typed failure family for this overrun.
    #[must_use]
    pub fn failure_bucket(self) -> ModelFailureBucket {
        ModelFailureBucket::ResourceBudget
    }

    /// The stable SPEC_0008 code for this overrun.
    #[must_use]
    pub fn error_code(self) -> &'static str {
        SOLVE_IR_SIZE_BUDGET_ERROR_CODE
    }
}

impl std::fmt::Display for SolveIrSizeBudgetExceeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{SOLVE_IR_SIZE_BUDGET_ERROR_CODE}] Solve IR exceeds the {} MB per-model \
             serialized-size ceiling (stopped measuring at {} MB); \
             raise it with the parity config `solve_ir_size_limit_mb` if this model \
             is genuinely this large",
            self.limit_bytes / BYTES_PER_MB,
            self.observed_bytes_at_least / BYTES_PER_MB,
        )
    }
}

/// A compile whose total wall time did not fit the declared budget.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CompileWallBudgetExceeded {
    /// Observed total compile wall seconds.
    pub observed_secs: f64,
    /// The ceiling that was exceeded, in seconds.
    pub limit_secs: f64,
}

impl CompileWallBudgetExceeded {
    /// The typed failure family for this overrun.
    #[must_use]
    pub fn failure_bucket(self) -> ModelFailureBucket {
        ModelFailureBucket::ResourceBudget
    }

    /// The stable SPEC_0008 code for this overrun.
    #[must_use]
    pub fn error_code(self) -> &'static str {
        COMPILE_WALL_BUDGET_ERROR_CODE
    }
}

impl std::fmt::Display for CompileWallBudgetExceeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{COMPILE_WALL_BUDGET_ERROR_CODE}] compile took {:.3}s, over the {:.3}s per-model \
             total compile wall ceiling; raise it with the parity config \
             `model_compile_wall_limit_secs` if this model is genuinely this slow",
            self.observed_secs, self.limit_secs,
        )
    }
}

/// Report a compile-wall overrun, or `None` when the compile is accepted.
///
/// A missing or non-finite measurement is *accepted*: the ceiling rejects
/// measured overruns, never absent data (which would turn a reporting gap into
/// a model failure).
#[must_use]
pub fn check_compile_wall_secs(
    observed_secs: Option<f64>,
    limit_secs: f64,
) -> Option<CompileWallBudgetExceeded> {
    let observed_secs = observed_secs.filter(|secs| secs.is_finite())?;
    (observed_secs > limit_secs).then_some(CompileWallBudgetExceeded {
        observed_secs,
        limit_secs,
    })
}

/// A per-model ceiling on the serialized size of one model's Solve IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveIrSizeBudget {
    limit_bytes: u64,
}

impl Default for SolveIrSizeBudget {
    fn default() -> Self {
        Self::from_mb(SOLVE_IR_SIZE_LIMIT_MB_DEFAULT)
    }
}

impl SolveIrSizeBudget {
    /// A budget of `limit_mb` megabytes, clamped up to the committed default so
    /// a caller cannot tighten the gate below its baseline.
    #[must_use]
    pub fn from_mb(limit_mb: u64) -> Self {
        Self {
            limit_bytes: limit_mb.max(SOLVE_IR_SIZE_LIMIT_MB_DEFAULT) * BYTES_PER_MB,
        }
    }

    /// The ceiling in bytes.
    #[must_use]
    pub fn limit_bytes(self) -> u64 {
        self.limit_bytes
    }

    /// The ceiling in megabytes.
    #[must_use]
    pub fn limit_mb(self) -> u64 {
        self.limit_bytes / BYTES_PER_MB
    }

    /// Accept an already-known byte count, or report the overrun.
    ///
    /// Used for the post-hoc check over a Solve-IR artifact a lane wrote to
    /// disk, where the size is a `metadata().len()` and no serialization is
    /// needed.
    pub fn check_bytes(self, bytes: u64) -> Result<u64, SolveIrSizeBudgetExceeded> {
        if bytes > self.limit_bytes {
            return Err(SolveIrSizeBudgetExceeded {
                observed_bytes_at_least: bytes,
                limit_bytes: self.limit_bytes,
            });
        }
        Ok(bytes)
    }

    /// Serialize `model` into `sink` and return the byte count, stopping at the
    /// first byte past the ceiling.
    ///
    /// The bound is on *work done*, not just on the reported number: an
    /// oversized model costs one ceiling's worth of serialization and then
    /// fails, so this can be called unconditionally without turning the
    /// pathological case into a hang. Pass [`std::io::sink`] to measure without
    /// keeping the bytes.
    pub fn measure_serialized<T, W>(
        self,
        model: &T,
        sink: W,
    ) -> Result<u64, SolveIrBudgetMeasureError>
    where
        T: Serialize + ?Sized,
        W: Write,
    {
        let mut writer = BudgetedWriter {
            inner: sink,
            written: 0,
            limit_bytes: self.limit_bytes,
            exceeded: false,
        };
        let outcome = serde_json::to_writer(&mut writer, model);
        if writer.exceeded {
            return Err(SolveIrBudgetMeasureError::BudgetExceeded(
                SolveIrSizeBudgetExceeded {
                    observed_bytes_at_least: writer.written,
                    limit_bytes: self.limit_bytes,
                },
            ));
        }
        outcome.map_err(|error| SolveIrBudgetMeasureError::Serialize(error.to_string()))?;
        writer
            .flush()
            .map_err(|error| SolveIrBudgetMeasureError::Serialize(error.to_string()))?;
        Ok(writer.written)
    }
}

/// Why a budgeted Solve-IR serialization did not produce a size.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveIrBudgetMeasureError {
    /// The model is larger than the declared ceiling.
    BudgetExceeded(SolveIrSizeBudgetExceeded),
    /// Serialization or the underlying sink failed for an unrelated reason.
    /// Kept separate so an IO fault is never reported as a budget overrun.
    Serialize(String),
}

impl std::fmt::Display for SolveIrBudgetMeasureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BudgetExceeded(exceeded) => exceeded.fmt(f),
            Self::Serialize(message) => write!(f, "failed to serialize Solve IR: {message}"),
        }
    }
}

impl SolveIrBudgetMeasureError {
    /// The typed failure family, or `None` when this was not a budget overrun.
    #[must_use]
    pub fn budget_failure_bucket(&self) -> Option<ModelFailureBucket> {
        match self {
            Self::BudgetExceeded(exceeded) => Some(exceeded.failure_bucket()),
            Self::Serialize(_) => None,
        }
    }
}

/// A writer that counts bytes and refuses to pass the ceiling.
struct BudgetedWriter<W: Write> {
    inner: W,
    written: u64,
    limit_bytes: u64,
    exceeded: bool,
}

impl<W: Write> Write for BudgetedWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // Accept the bytes that still fit, then refuse. Writing the final
        // partial chunk keeps a truncated artifact honest about where it stops
        // and makes `written` a real lower bound rather than a rounded one.
        let remaining = self.limit_bytes.saturating_sub(self.written);
        let allowed = usize::try_from(remaining)
            .unwrap_or(usize::MAX)
            .min(buf.len());
        if allowed > 0 {
            self.inner.write_all(&buf[..allowed])?;
            self.written += allowed as u64;
        }
        if allowed < buf.len() {
            self.written += (buf.len() - allowed) as u64;
            self.exceeded = true;
            return Err(io::Error::other(
                "Solve IR serialized-size budget exceeded".to_string(),
            ));
        }
        Ok(allowed)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Serialize)]
    struct Payload {
        rows: Vec<u64>,
    }

    fn payload(rows: usize) -> Payload {
        Payload {
            rows: (0..rows as u64).collect(),
        }
    }

    #[test]
    fn a_model_inside_the_ceiling_is_accepted_and_its_exact_size_reported() {
        let budget = SolveIrSizeBudget::default();
        let model = payload(1_000);
        let bytes = budget
            .measure_serialized(&model, io::sink())
            .expect("a small Solve IR is accepted");
        let exact = serde_json::to_vec(&model).expect("serializes");
        assert_eq!(bytes, exact.len() as u64);
        assert!(bytes < budget.limit_bytes());
    }

    #[test]
    fn measurement_stops_at_the_ceiling_instead_of_materializing_the_model() {
        // A one-MB ceiling is clamped up to the committed default, so build the
        // budget through the raise-only path and feed it a model that is
        // genuinely larger than the default ceiling would allow.
        let budget = SolveIrSizeBudget::from_mb(SOLVE_IR_SIZE_LIMIT_MB_DEFAULT);
        // ~8 bytes per element at minimum; 8M elements is far past 32 MB.
        let model = payload(8_000_000);
        let error = budget
            .measure_serialized(&model, io::sink())
            .expect_err("an oversized Solve IR is rejected");
        let SolveIrBudgetMeasureError::BudgetExceeded(exceeded) = error else {
            panic!("an oversized model must report a budget overrun, got {error}");
        };
        assert!(
            exceeded.observed_bytes_at_least > budget.limit_bytes(),
            "the reported lower bound must be past the ceiling"
        );
        assert!(
            exceeded.observed_bytes_at_least < budget.limit_bytes() + 64 * 1024,
            "measurement must stop just past the ceiling, not run to completion; \
             stopped at {} bytes",
            exceeded.observed_bytes_at_least
        );
        assert_eq!(
            exceeded.failure_bucket(),
            ModelFailureBucket::ResourceBudget
        );
        assert_eq!(exceeded.error_code(), SOLVE_IR_SIZE_BUDGET_ERROR_CODE);
    }

    #[test]
    fn the_rendered_overrun_names_the_ceiling_and_the_flag_that_raises_it() {
        let exceeded = SolveIrSizeBudgetExceeded {
            observed_bytes_at_least: 40 * BYTES_PER_MB,
            limit_bytes: 32 * BYTES_PER_MB,
        };
        let rendered = exceeded.to_string();
        assert!(
            rendered.contains(SOLVE_IR_SIZE_BUDGET_ERROR_CODE),
            "{rendered}"
        );
        assert!(rendered.contains("32 MB"), "{rendered}");
        assert!(rendered.contains("40 MB"), "{rendered}");
        assert!(rendered.contains("solve_ir_size_limit_mb"), "{rendered}");
    }

    #[test]
    fn an_on_disk_artifact_is_checked_without_reserializing() {
        let budget = SolveIrSizeBudget::default();
        assert_eq!(
            budget.check_bytes(budget.limit_bytes()),
            Ok(budget.limit_bytes())
        );
        assert_eq!(
            budget.check_bytes(budget.limit_bytes() + 1),
            Err(SolveIrSizeBudgetExceeded {
                observed_bytes_at_least: budget.limit_bytes() + 1,
                limit_bytes: budget.limit_bytes(),
            })
        );
    }

    #[test]
    fn ceilings_are_raise_only() {
        assert_eq!(raise_only_mb(None, SOLVE_IR_SIZE_LIMIT_MB_DEFAULT), 32);
        assert_eq!(raise_only_mb(Some(4), SOLVE_IR_SIZE_LIMIT_MB_DEFAULT), 32);
        assert_eq!(
            raise_only_mb(Some(128), SOLVE_IR_SIZE_LIMIT_MB_DEFAULT),
            128
        );
        assert_eq!(
            SolveIrSizeBudget::from_mb(1).limit_mb(),
            SOLVE_IR_SIZE_LIMIT_MB_DEFAULT,
            "a config asking for a tighter ceiling must be clamped back to the baseline"
        );

        assert_eq!(
            raise_only_secs(None, MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT),
            40.0
        );
        assert_eq!(
            raise_only_secs(Some(5.0), MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT),
            40.0
        );
        assert_eq!(
            raise_only_secs(Some(120.0), MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT),
            120.0
        );
        assert_eq!(
            raise_only_secs(Some(f64::NAN), MODEL_COMPILE_WALL_LIMIT_SECS_DEFAULT),
            40.0
        );
    }

    #[test]
    fn compile_wall_rejects_measured_overruns_and_accepts_absent_data() {
        assert_eq!(check_compile_wall_secs(None, 40.0), None);
        assert_eq!(check_compile_wall_secs(Some(f64::NAN), 40.0), None);
        assert_eq!(check_compile_wall_secs(Some(39.9), 40.0), None);
        let exceeded =
            check_compile_wall_secs(Some(41.0), 40.0).expect("a measured overrun is rejected");
        assert_eq!(
            exceeded.failure_bucket(),
            ModelFailureBucket::ResourceBudget
        );
        assert_eq!(exceeded.error_code(), COMPILE_WALL_BUDGET_ERROR_CODE);
        assert!(
            exceeded
                .to_string()
                .contains("model_compile_wall_limit_secs"),
            "the overrun must name the flag that raises it: {exceeded}"
        );
    }

    #[test]
    fn a_sink_failure_is_not_reported_as_a_budget_overrun() {
        struct Failing;
        impl Write for Failing {
            fn write(&mut self, _: &[u8]) -> io::Result<usize> {
                Err(io::Error::other("disk gone"))
            }
            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        let error = SolveIrSizeBudget::default()
            .measure_serialized(&payload(4), Failing)
            .expect_err("an IO fault fails");
        assert!(
            matches!(error, SolveIrBudgetMeasureError::Serialize(_)),
            "an IO fault must not be attributed to the resource budget, got {error}"
        );
        assert_eq!(error.budget_failure_bucket(), None);
    }
}
