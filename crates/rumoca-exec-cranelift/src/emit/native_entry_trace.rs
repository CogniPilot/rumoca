//! First-dispatch diagnostics, using the caller's ordinary tracing subscriber.

use std::cell::Cell;

pub(super) struct EntryUse(Cell<bool>);

impl Default for EntryUse {
    fn default() -> Self {
        Self(Cell::new(tracing::enabled!(
            target: "rumoca_exec_cranelift::native_entry",
            tracing::Level::DEBUG
        )))
    }
}

impl EntryUse {
    pub(super) fn record(&self, family: &'static str) {
        if self.0.replace(false) {
            tracing::debug!(target: "rumoca_exec_cranelift::native_entry", family, "first native entry dispatch");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;
    use std::sync::{Arc, Mutex};

    #[derive(Clone)]
    struct Capture(Arc<Mutex<Vec<u8>>>);
    impl io::Write for Capture {
        fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
            self.0.lock().unwrap().extend_from_slice(bytes);
            Ok(bytes.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn enabled_entries_report_only_the_first_dispatch_and_disabled_entries_are_inert() {
        let capture = Capture(Arc::new(Mutex::new(Vec::new())));
        let writer = capture.clone();
        let subscriber = tracing_subscriber::fmt()
            .with_max_level(tracing::Level::DEBUG)
            .with_ansi(false)
            .with_writer(move || writer.clone())
            .finish();
        let disabled = EntryUse::default();
        tracing::subscriber::with_default(subscriber, || {
            disabled.record("disabled");
            let entry = EntryUse::default();
            entry.record("residual_whole");
            entry.record("residual_selected");
            EntryUse::default().record("jacobian_row");
            let span = rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("native_entry.mo"),
                0,
                1,
            );
            let source = rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
                vec![vec![
                    rumoca_ir_solve::LinearOp::Const { dst: 0, value: 3.0 },
                    rumoca_ir_solve::LinearOp::StoreOutput { src: 0 },
                ]],
                vec![span],
                vec![0],
            )
            .unwrap();
            let compiled = crate::compile_expression_scalar_program_block(&source).unwrap();
            let mut value = [0.0];
            for _ in 0..2 {
                compiled.call(&[], &[], 0.0, &mut value).unwrap();
                assert_eq!(value, [3.0]);
            }
        });
        let data = capture.0.lock().unwrap();
        let text = std::str::from_utf8(&data).unwrap();
        assert_eq!(text.matches("first native entry dispatch").count(), 3);
        assert_eq!(text.matches("residual_whole").count(), 2);
        assert!(text.contains("residual_whole"));
        assert!(text.contains("jacobian_row"));
        assert!(!text.contains("residual_selected"));
        assert!(!text.contains("disabled"));
    }
}
