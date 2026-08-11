//! Opt-in dynamic invocation counts for compiler-issued typed owners.

use cranelift_jit::JITBuilder;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::OnceLock;

pub(super) const SYMBOL: &str = "rumoca_host_profile_typed_owner";

thread_local! {
    static COUNTS: RefCell<HashMap<usize, Vec<u64>>> = RefCell::new(HashMap::new());
}

pub(super) fn enabled() -> bool {
    std::env::var_os("RUMOCA_PROFILE_NATIVE_CALLS").is_some()
}

pub(super) fn register_symbol(builder: &mut JITBuilder) {
    builder.symbol(SYMBOL, rumoca_host_profile_typed_owner as *const u8);
}

pub(super) struct ProfileSession {
    table: usize,
}

impl ProfileSession {
    pub(super) const fn new(table: usize) -> Self {
        Self { table }
    }
}

impl Drop for ProfileSession {
    fn drop(&mut self) {
        COUNTS.with(|counts| {
            let Some(counts) = counts.borrow_mut().remove(&self.table) else {
                return;
            };
            for (owner, count) in counts
                .into_iter()
                .enumerate()
                .filter(|(_, count)| *count != 0)
            {
                eprintln!(
                    "rumoca-native-call-profile table={} owner={owner} count={count}",
                    self.table,
                );
            }
        });
    }
}

extern "C" fn rumoca_host_profile_typed_owner(table: u64, owner: u64) {
    let Ok(table) = usize::try_from(table) else {
        return;
    };
    let Ok(owner) = usize::try_from(owner) else {
        return;
    };
    static TRACE_OWNER: OnceLock<Option<usize>> = OnceLock::new();
    let trace_owner = TRACE_OWNER.get_or_init(|| {
        std::env::var("RUMOCA_PROFILE_NATIVE_CALL_TRACE")
            .ok()
            .and_then(|value| value.parse().ok())
    });
    if *trace_owner == Some(owner) {
        eprintln!("rumoca-native-call-trace table={table} owner={owner}");
    }
    COUNTS.with(|counts| {
        let mut counts = counts.borrow_mut();
        let owners = counts.entry(table).or_default();
        if owners.len() <= owner {
            owners.resize(owner + 1, 0);
        }
        owners[owner] = owners[owner].saturating_add(1);
    });
}
