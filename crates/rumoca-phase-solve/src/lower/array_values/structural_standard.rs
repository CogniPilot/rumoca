use super::super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_structural_standard_array_values(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        _scope: &Scope,
        _call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if !is_polyphase_symmetric_orientation_call(call_name) {
            return Ok(None);
        }
        let Some(arg) = args.first() else {
            return Ok(None);
        };
        let const_scope = self.local_const_bindings.clone();
        let m = self.eval_compile_time_int(arg, &const_scope, "symmetricOrientation dimension")?;
        let values = symmetric_orientation_values(m).ok_or_else(|| LowerError::Unsupported {
            reason: format!("symmetricOrientation dimension {m} is unsupported"),
        })?;
        Ok(Some(
            values
                .into_iter()
                .map(|value| self.emit_const(value))
                .collect(),
        ))
    }
}

fn is_polyphase_symmetric_orientation_call(call_name: &str) -> bool {
    matches!(
        call_name,
        "Modelica.Electrical.Polyphase.Functions.symmetricOrientation"
            | "Electrical.Polyphase.Functions.symmetricOrientation"
            | "Polyphase.Functions.symmetricOrientation"
    )
}

fn symmetric_orientation_values(m: i64) -> Option<Vec<f64>> {
    if !(1..=1024).contains(&m) {
        return None;
    }
    if m % 2 == 0 {
        if m == 2 {
            return Some(vec![0.0, std::f64::consts::FRAC_PI_2]);
        }
        let half = m / 2;
        let base = symmetric_orientation_values(half)?;
        let offset = std::f64::consts::PI / m as f64;
        let mut values = Vec::with_capacity(m as usize);
        values.extend(base.iter().copied());
        values.extend(base.into_iter().map(|value| value - offset));
        return Some(values);
    }
    Some(
        (1..=m)
            .map(|k| (k - 1) as f64 * 2.0 * std::f64::consts::PI / m as f64)
            .collect(),
    )
}
