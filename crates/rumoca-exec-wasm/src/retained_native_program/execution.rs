//! Actual native entry outcomes remain bound to the prepared product and lease.

use rumoca_ir_solve::{NativeDerivedOutput, SolveValueKind};

use super::{NativeInvocationLease, transport::NativeTransport};
use crate::WasmCompileError;

/// A zero-status kernel outcome, retaining exclusive entry until consumption.
/// It grants no FMI lifecycle or accepted-transaction publication authority.
pub struct NativeInvocationOutcome<'call> {
    lease: NativeInvocationLease<'call>,
}

impl<'call> NativeInvocationLease<'call> {
    pub fn execute(self, time: f64) -> Result<NativeInvocationOutcome<'call>, WasmCompileError> {
        #[cfg(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        ))]
        {
            let transport = &mut *self.transport;
            transport.storage.work_y.clone_from(&transport.storage.y);
            transport
                .storage
                .work_lanes
                .clone_from(&transport.storage.lanes);
            let status = transport.product.code.call(
                &mut transport.storage.work_y,
                &transport.storage.p,
                time,
                &mut transport.storage.scratch,
                &mut transport.storage.work_lanes,
            )?;
            if status != 0 {
                return Err(transport.product.compiled().status_error(status));
            }
            validate_outputs(transport)?;
            Ok(NativeInvocationOutcome { lease: self })
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        )))]
        {
            let _ = time;
            Err(WasmCompileError::Input(
                "retained native WASM execution requires a concrete wasm32 engine".into(),
            ))
        }
    }
}

impl NativeInvocationOutcome<'_> {
    pub fn product(&self) -> &super::CompiledRetainedNativeProgramWasm {
        self.lease.product()
    }

    pub fn y(&self) -> &[f64] {
        #[cfg(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        ))]
        {
            &self.lease.transport.storage.work_y
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        )))]
        {
            unreachable!("native outcomes have no native-host constructor")
        }
    }

    /// Commit the checked value buffers. FMI publication remains the caller's
    /// existing transaction, which can restore its same-resource snapshot.
    pub fn commit_buffers(self) {
        #[cfg(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        ))]
        {
            let transport = &mut *self.lease.transport;
            transport.storage.y.clone_from(&transport.storage.work_y);
            transport
                .storage
                .lanes
                .clone_from(&transport.storage.work_lanes);
            transport.storage.outputs_present = true;
        }
    }

    /// Read original output descriptors and actual typed cells, without a
    /// duplicate output inventory or an Integer-to-Float64 conversion.
    pub fn derived_outputs(&self) -> impl Iterator<Item = (&NativeDerivedOutput, SolveValueKind)> {
        let transport = &*self.lease.transport;
        transport
            .product
            .inputs()
            .schedule()
            .derived_outputs()
            .iter()
            .map(move |output| (output, read_output(transport, output)))
    }
}

#[cfg(any(
    all(target_arch = "wasm32", target_os = "unknown"),
    all(feature = "host-engine", not(target_arch = "wasm32")),
    all(
        feature = "fixed-native-guest-receiver",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    )
))]
fn validate_outputs(transport: &NativeTransport) -> Result<(), WasmCompileError> {
    for output in transport.product.inputs().schedule().derived_outputs() {
        if super::cells::decode(output_bits(transport, output), output.lane()).is_none() {
            return Err(WasmCompileError::Backend(
                "native entry returned a non-Boolean typed output cell".into(),
            ));
        }
    }
    Ok(())
}

fn output_bits(transport: &NativeTransport, output: &NativeDerivedOutput) -> u64 {
    let offset = transport.product.inputs().schedule().input_lane_bytes() + output.lane_offset();
    #[cfg(any(
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32")),
        all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    ))]
    let word = transport.storage.work_lanes[offset / 8];
    #[cfg(not(any(
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32")),
        all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    )))]
    let word = transport.storage.lanes[offset / 8];
    super::cells::bits(std::slice::from_ref(&word), offset % 8, output.lane())
}

fn read_output(transport: &NativeTransport, output: &NativeDerivedOutput) -> SolveValueKind {
    super::cells::decode(output_bits(transport, output), output.lane())
        .expect("typed output decoding follows the complete checked Boolean validation")
}
