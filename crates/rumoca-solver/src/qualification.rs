//! Private qualification observer; absent from ordinary builds.
use std::cell::Cell;

#[derive(Clone, Copy)]
struct Context {
    call: u64,
    kind: &'static str,
    time: u64,
    block: usize,
    iteration: usize,
}

thread_local! {
    static NEXT: Cell<u64> = const { Cell::new(0) };
    static CURRENT: Cell<Context> = const { Cell::new(Context {
        call: 0, kind: "none", time: 0, block: 0, iteration: 0,
    }) };
}

pub(crate) struct Guard(Context);
impl Drop for Guard {
    fn drop(&mut self) {
        CURRENT.with(|current| current.set(self.0));
    }
}

pub(crate) fn enter(kind: &'static str, time: f64, block: usize, iteration: usize) -> Guard {
    let call = NEXT.with(|next| {
        let value = next.get();
        next.set(value + 1);
        value
    });
    CURRENT.with(|current| {
        let previous = current.replace(Context {
            call,
            kind,
            time: time.to_bits(),
            block,
            iteration,
        });
        Guard(previous)
    })
}

pub(crate) fn factor(unchanged: bool, ready: bool) {
    CURRENT.with(|current| {
        let c = current.get();
        eprintln!(
            "G2_LU {{\"call\":{},\"kind\":\"{}\",\"time_bits\":\"{:016x}\",\"block\":{},\"iteration\":{},\"unchanged\":{},\"ready\":{}}}",
            c.call, c.kind, c.time, c.block, c.iteration, unchanged, ready,
        );
    });
}
