//! Bounded packed tensor reads into the existing scalar-register ABI.
//!
//! Large direct tensor operands use compute::matrix's input views instead;
//! materializing image-sized register files is deliberately unsupported.
use super::*;
use rumoca_ir_solve::TensorInputKind;

pub(super) const MAX_PACKED_VALUES: usize = 4096;

pub(super) fn validate_local_tensor_loads(rows: &[Vec<LinearOp>]) -> Result<(), String> {
    for op in rows.iter().flatten() {
        if let LinearOp::TensorLoad { count, lanes, .. } = op {
            let values = count
                .checked_mul(*lanes)
                .ok_or("WASM tensor load width overflow")?;
            if !matches!(lanes, 1 | 2) || values > MAX_PACKED_VALUES {
                return Err(
                    "WASM packed tensor loads support one/two lanes and at most4096 values".into(),
                );
            }
        }
    }
    Ok(())
}

impl BodyEmitter<'_> {
    pub(super) fn emit_tensor_load(&mut self, op: LinearOp) -> Result<(), String> {
        let LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            seed_start,
            lanes,
        } = op
        else {
            return Err("WASM tensor-load emitter requires TensorLoad".into());
        };
        let pointer = match input {
            TensorInputKind::Y => Y_PTR_PARAM,
            TensorInputKind::P => P_PTR_PARAM,
        };
        for element in 0..count {
            let offset = element
                .checked_mul(lanes)
                .and_then(|value| u32::try_from(value).ok())
                .ok_or("WASM tensor register overflow")?;
            let dst = dst_start
                .checked_add(offset)
                .ok_or("WASM tensor register overflow")?;
            let index = input_start
                .checked_add(element)
                .ok_or("WASM tensor input range overflow")?;
            self.emit_array_load(dst, index, pointer)?;
            if lanes == 2 {
                let tangent = dst
                    .checked_add(1)
                    .ok_or("WASM tensor tangent register overflow")?;
                match seed_start {
                    Some(start) => self.emit_array_load(
                        tangent,
                        start
                            .checked_add(element)
                            .ok_or("WASM tensor seed range overflow")?,
                        SEED_PTR_PARAM,
                    )?,
                    None => self.emit_const(tangent, 0.0)?,
                }
            }
        }
        Ok(())
    }
}
