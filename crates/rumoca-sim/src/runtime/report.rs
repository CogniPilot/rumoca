use std::time::{Duration, Instant};

#[derive(Debug, Clone, Copy)]
pub struct RuntimeTraceContext {
    pub enabled: bool,
    pub solver: &'static str,
    pub regularization: f64,
    pub profile: &'static str,
    start: Option<Instant>,
}

impl RuntimeTraceContext {
    pub fn new(
        enabled: bool,
        solver: &'static str,
        regularization: f64,
        profile: &'static str,
    ) -> Self {
        Self {
            enabled,
            solver,
            regularization,
            profile,
            start: trace_instant_now_if(enabled),
        }
    }

    pub fn elapsed_secs(self) -> f64 {
        self.start.map_or(0.0, |t0| t0.elapsed().as_secs_f64())
    }
}

#[inline]
fn trace_instant_now_if(enabled: bool) -> Option<Instant> {
    if !enabled {
        return None;
    }
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RuntimeProgressSnapshot {
    pub steps: usize,
    pub root_hits: usize,
    pub t: f64,
    pub output_idx: usize,
    pub output_len: usize,
}

pub fn runtime_progress_snapshot(
    steps: usize,
    root_hits: usize,
    t: f64,
    output_idx: usize,
    output_len: usize,
) -> RuntimeProgressSnapshot {
    RuntimeProgressSnapshot {
        steps,
        root_hits,
        t,
        output_idx,
        output_len,
    }
}

pub fn trace_runtime_start(ctx: RuntimeTraceContext, h0: f64, max_wall_seconds: Option<f64>) {
    if ctx.enabled {
        eprintln!(
            "[sim-trace] {} start eps={} profile={} h0={} max_wall={:?}",
            ctx.solver, ctx.regularization, ctx.profile, h0, max_wall_seconds
        );
    }
}

pub fn trace_runtime_timeout(ctx: RuntimeTraceContext, snap: RuntimeProgressSnapshot) {
    if ctx.enabled {
        eprintln!(
            "[sim-trace] {} timeout eps={} profile={} elapsed={:.3}s steps={} roots={} t={} output_idx={}/{}",
            ctx.solver,
            ctx.regularization,
            ctx.profile,
            ctx.elapsed_secs(),
            snap.steps,
            snap.root_hits,
            snap.t,
            snap.output_idx,
            snap.output_len
        );
    }
}

pub fn trace_runtime_step_fail(
    ctx: RuntimeTraceContext,
    snap: RuntimeProgressSnapshot,
    err: impl std::fmt::Display,
) {
    if ctx.enabled {
        eprintln!(
            "[sim-trace] {} step-fail eps={} profile={} elapsed={:.3}s steps={} roots={} t={} err={}",
            ctx.solver,
            ctx.regularization,
            ctx.profile,
            ctx.elapsed_secs(),
            snap.steps,
            snap.root_hits,
            snap.t,
            err
        );
    }
}

pub fn trace_runtime_progress(
    ctx: RuntimeTraceContext,
    snap: RuntimeProgressSnapshot,
    t_limit: f64,
    last_log: &mut Option<Instant>,
) {
    if !ctx.enabled {
        return;
    }
    if !snap.steps.is_multiple_of(200)
        && last_log
            .as_ref()
            .is_some_and(|last| last.elapsed() < Duration::from_secs(1))
    {
        return;
    }
    eprintln!(
        "[sim-trace] {} progress eps={} profile={} elapsed={:.3}s steps={} roots={} t={} t_limit={} output_idx={}/{}",
        ctx.solver,
        ctx.regularization,
        ctx.profile,
        ctx.elapsed_secs(),
        snap.steps,
        snap.root_hits,
        snap.t,
        t_limit,
        snap.output_idx,
        snap.output_len
    );
    *last_log = trace_instant_now_if(true);
}

pub fn trace_runtime_done(ctx: RuntimeTraceContext, steps: usize, root_hits: usize, final_t: f64) {
    if ctx.enabled {
        eprintln!(
            "[sim-trace] {} done eps={} profile={} elapsed={:.3}s steps={} roots={} final_t={}",
            ctx.solver,
            ctx.regularization,
            ctx.profile,
            ctx.elapsed_secs(),
            steps,
            root_hits,
            final_t
        );
    }
}
