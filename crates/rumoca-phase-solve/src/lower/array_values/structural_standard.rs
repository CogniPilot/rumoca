use super::{fallible_vec_with_capacity, *};

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
        let span = arg
            .require_span("symmetricOrientation dimension argument")?
            .span();
        let values = symmetric_orientation_values(m, span)?;
        let mut regs = crate::lower_vec_with_capacity(
            values.len(),
            "symmetricOrientation register count",
            span,
        )?;
        for value in values {
            regs.push(self.emit_const_at(value, span)?);
        }
        Ok(Some(regs))
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

fn symmetric_orientation_values(m: i64, span: rumoca_core::Span) -> Result<Vec<f64>, LowerError> {
    if !(1..=1024).contains(&m) {
        return Err(unsupported_at(
            format!("symmetricOrientation dimension {m} is unsupported"),
            span,
        ));
    }
    if m % 2 == 0 {
        if m == 2 {
            return Ok(vec![0.0, std::f64::consts::FRAC_PI_2]);
        }
        let half = m / 2;
        let base = symmetric_orientation_values(half, span)?;
        let offset = std::f64::consts::PI / m as f64;
        let capacity = usize::try_from(m).map_err(|_| {
            LowerError::contract_violation(
                "symmetricOrientation value count overflows host index range",
                span,
            )
        })?;
        let mut values =
            fallible_vec_with_capacity(capacity, "symmetricOrientation value count", span)?;
        values.extend(base.iter().copied());
        values.extend(base.into_iter().map(|value| value - offset));
        return Ok(values);
    }
    let capacity = usize::try_from(m).map_err(|_| {
        LowerError::contract_violation(
            "symmetricOrientation value count overflows host index range",
            span,
        )
    })?;
    let mut values =
        fallible_vec_with_capacity(capacity, "symmetricOrientation value count", span)?;
    for k in 1..=m {
        values.push((k - 1) as f64 * 2.0 * std::f64::consts::PI / m as f64);
    }
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symmetric_orientation_rejects_unsupported_dimension_with_span() -> Result<(), String> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_structural_standard_source_15.mo",
            ),
            4,
            12,
        );

        let Err(err) = symmetric_orientation_values(0, span) else {
            return Err("unsupported symmetricOrientation dimension succeeded".to_string());
        };

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "symmetricOrientation dimension 0 is unsupported"
        );
        Ok(())
    }

    #[test]
    fn structural_symmetric_orientation_rejects_unspanned_dimension_argument() {
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);
        let args = vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            span: rumoca_core::Span::DUMMY,
        }];

        let err = builder
            .lower_structural_standard_array_values(
                "Modelica.Electrical.Polyphase.Functions.symmetricOrientation",
                &args,
                &Scope::new(),
                0,
            )
            .expect_err("unspanned symmetricOrientation dimension should fail");

        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: missing source provenance for symmetricOrientation dimension argument"
        );
    }
}
