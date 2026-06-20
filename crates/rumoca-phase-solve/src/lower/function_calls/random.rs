use super::*;

struct ProjectedRandomArrayCtx<'a> {
    generator: RandomGenerator,
    kind: RandomIntrinsicKind,
    projection: &'a FunctionOutputProjection,
    span: rumoca_core::Span,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_impure_random_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let (named_args, positional_args) = split_named_and_positional_call_args(call_name, args)?;
        match call_name {
            // MLS §12.3: impure function calls are event/initial computations.
            // Represent MSL's hidden-state random utilities as explicit
            // discrete solve-IR ops so continuous rows cannot treat them as
            // pure algebraic functions and so event iteration can cache each
            // call site at a fixed event instant.
            "Modelica.Math.Random.Utilities.automaticGlobalSeed" | "automaticGlobalSeed" => {
                let call_site = self.alloc_call_site()?;
                let counter = call_site.checked_add(1).ok_or_else(|| {
                    random_contract_error("automaticGlobalSeed call-site counter overflow")
                })?;
                self.emit_const_at(
                    rumoca_eval_dae::deterministic_automatic_global_seed(counter) as f64,
                    span,
                )
                .map(Some)
            }
            "Modelica.Math.Random.Utilities.automaticLocalSeed" | "automaticLocalSeed" => {
                let seed = i64::from(rumoca_eval_dae::modelica_strings_hash_string(
                    required_automatic_local_seed_path(args.first(), span)?,
                ));
                self.emit_const_at(seed as f64, span).map(Some)
            }
            "Modelica.Math.Random.Utilities.initializeImpureRandom" | "initializeImpureRandom" => {
                let seed = self.lower_named_or_positional_arg(
                    &named_args,
                    &positional_args,
                    NamedOrPositionalArg {
                        name: "seed",
                        idx: 0,
                        default: 0.0,
                    },
                    span,
                    scope,
                    call_depth,
                )?;
                self.emit_impure_random_init(seed, span).map(Some)
            }
            "Modelica.Math.Random.Utilities.impureRandom" | "impureRandom" => {
                let id = self.lower_named_or_positional_arg(
                    &named_args,
                    &positional_args,
                    NamedOrPositionalArg {
                        name: "id",
                        idx: 0,
                        default: 0.0,
                    },
                    span,
                    scope,
                    call_depth,
                )?;
                self.emit_impure_random(id, span).map(Some)
            }
            "Modelica.Math.Random.Utilities.impureRandomInteger" | "impureRandomInteger" => {
                let id = self.lower_named_or_positional_arg(
                    &named_args,
                    &positional_args,
                    NamedOrPositionalArg {
                        name: "id",
                        idx: 0,
                        default: 0.0,
                    },
                    span,
                    scope,
                    call_depth,
                )?;
                let imin = self.lower_named_or_positional_arg(
                    &named_args,
                    &positional_args,
                    NamedOrPositionalArg {
                        name: "imin",
                        idx: 1,
                        default: 1.0,
                    },
                    span,
                    scope,
                    call_depth,
                )?;
                let imax = self.lower_named_or_positional_arg(
                    &named_args,
                    &positional_args,
                    NamedOrPositionalArg {
                        name: "imax",
                        idx: 2,
                        default: 268_435_456.0,
                    },
                    span,
                    scope,
                    call_depth,
                )?;
                self.emit_impure_random_integer(id, imin, imax, span)
                    .map(Some)
            }
            _ => Ok(None),
        }
    }

    pub(in crate::lower) fn lower_projected_random_intrinsic(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let span = projection.span;
        let Some((generator, kind)) = random_intrinsic_kind(projection.base_function_name.as_str())
        else {
            return Ok(None);
        };
        match kind {
            RandomIntrinsicKind::InitialState => {
                if projection.output_name != "state" {
                    return Ok(None);
                }
                let Some(state_index) = zero_based_projection_index(projection)? else {
                    return Ok(None);
                };
                let local_seed = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
                let state_len = random_projection_state_len(generator, projection, args)?;
                self.emit_random_initial_state(
                    generator,
                    local_seed,
                    global_seed,
                    state_len,
                    state_index,
                    random_args_span(args, span),
                )
                .map(Some)
            }
            RandomIntrinsicKind::Random => match projection.output_name.as_str() {
                "result" if projection.indices.is_empty() => {
                    let state_values =
                        self.lower_random_state_argument(args, span, scope, call_depth)?;
                    self.emit_random_result(generator, &state_values, random_args_span(args, span))
                        .map(Some)
                }
                "stateOut" => {
                    let Some(state_index) = zero_based_projection_index(projection)? else {
                        return Ok(None);
                    };
                    let state_values =
                        self.lower_random_state_argument(args, span, scope, call_depth)?;
                    self.emit_random_state(
                        generator,
                        &state_values,
                        state_index,
                        random_args_span(args, span),
                    )
                    .map(Some)
                }
                _ => Ok(None),
            },
        }
    }

    pub(super) fn lower_random_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some((generator, kind)) = random_intrinsic_kind(call_name) else {
            return Ok(None);
        };
        match kind {
            // Scalar context for an array-valued initialState means the first
            // component. Array contexts call lower_random_initial_state_values.
            RandomIntrinsicKind::InitialState => {
                let local_seed = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
                let state_len = random_generator_state_len(generator);
                self.emit_random_initial_state(
                    generator,
                    local_seed,
                    global_seed,
                    state_len,
                    0,
                    random_args_span(args, span),
                )
                .map(Some)
            }
            RandomIntrinsicKind::Random => {
                let state_values =
                    self.lower_random_state_argument(args, span, scope, call_depth)?;
                self.emit_random_result(generator, &state_values, random_args_span(args, span))
                    .map(Some)
            }
        }
    }

    pub(in crate::lower) fn lower_random_array_values(
        &mut self,
        call_name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let Some(projection) = self.lookup_function_output_projection(call_name, span)?
            && let Some((generator, kind)) =
                random_intrinsic_kind(projection.base_function_name.as_str())
        {
            return self.lower_projected_random_array_values(
                ProjectedRandomArrayCtx {
                    generator,
                    kind,
                    projection: &projection,
                    span,
                },
                args,
                scope,
                call_depth,
            );
        }
        let call_name = call_name.as_str();
        let Some((generator, kind)) = random_intrinsic_kind(call_name) else {
            return Ok(None);
        };
        let values = match kind {
            RandomIntrinsicKind::InitialState => {
                let local_seed = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
                let state_len = random_generator_state_len(generator);
                let span = random_args_span(args, span);
                let mut values = crate::lower_vec_with_capacity(
                    state_len,
                    "random initial state value count",
                    span,
                )?;
                for state_index in 0..state_len {
                    values.push(self.emit_random_initial_state(
                        generator,
                        local_seed,
                        global_seed,
                        state_len,
                        state_index,
                        span,
                    )?);
                }
                values
            }
            RandomIntrinsicKind::Random => {
                let state_values =
                    self.lower_random_state_argument(args, span, scope, call_depth)?;
                let span = random_args_span(args, span);
                let capacity = checked_random_value_capacity_at(state_values.len(), 1, span)?;
                let mut values =
                    crate::lower_vec_with_capacity(capacity, "random output value count", span)?;
                values.push(self.emit_random_result(generator, &state_values, span)?);
                for state_index in 0..state_values.len() {
                    values.push(self.emit_random_state(
                        generator,
                        &state_values,
                        state_index,
                        span,
                    )?);
                }
                values
            }
        };
        Ok(Some(values))
    }

    fn lower_projected_random_array_values(
        &mut self,
        ctx: ProjectedRandomArrayCtx<'_>,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        match (ctx.kind, ctx.projection.output_name.as_str()) {
            (RandomIntrinsicKind::InitialState, "state") => {
                let local_seed = self.lower_optional_arg(args, 0, ctx.span, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, ctx.span, scope, call_depth)?;
                let state_len = match random_state_len_arg(args, ctx.span)? {
                    Some(len) => len,
                    None => random_generator_state_len(ctx.generator),
                };
                let span = random_args_span(args, ctx.span);
                let mut values = crate::lower_vec_with_capacity(
                    state_len,
                    "projected random state value count",
                    span,
                )?;
                for state_index in 0..state_len {
                    values.push(self.emit_random_initial_state(
                        ctx.generator,
                        local_seed,
                        global_seed,
                        state_len,
                        state_index,
                        span,
                    )?);
                }
                Ok(Some(values))
            }
            (RandomIntrinsicKind::Random, "stateOut") => {
                let state_values =
                    self.lower_random_state_argument(args, ctx.span, scope, call_depth)?;
                let span = random_args_span(args, ctx.span);
                let mut values = crate::lower_vec_with_capacity(
                    state_values.len(),
                    "projected random stateOut value count",
                    span,
                )?;
                for state_index in 0..state_values.len() {
                    values.push(self.emit_random_state(
                        ctx.generator,
                        &state_values,
                        state_index,
                        span,
                    )?);
                }
                Ok(Some(values))
            }
            _ => Ok(None),
        }
    }

    fn lower_random_state_argument(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(state_expr) = args.first() {
            let values = self.lower_array_like_values(state_expr, scope, call_depth)?;
            if !values.is_empty() {
                return Ok(values);
            }
        }
        Ok(vec![self.emit_const_at(1.0, random_args_span(args, span))?])
    }

    pub(super) fn lower_optional_arg(
        &mut self,
        args: &[rumoca_core::Expression],
        idx: usize,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(expr) = args.get(idx) {
            self.lower_expr(expr, scope, call_depth)
        } else {
            self.emit_const_at(0.0, random_args_span(args, span))
        }
    }

    fn lower_named_or_positional_arg(
        &mut self,
        named_args: &IndexMap<String, &rumoca_core::Expression>,
        positional_args: &[&rumoca_core::Expression],
        arg: NamedOrPositionalArg<'_>,
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(expr) = named_args
            .get(arg.name)
            .copied()
            .or_else(|| positional_args.get(arg.idx).copied())
        {
            self.lower_expr(expr, scope, call_depth)
        } else {
            let span = positional_args
                .first()
                .and_then(|expr| expr.span())
                .unwrap_or(span);
            self.emit_const_at(arg.default, span)
        }
    }

    pub(super) fn lower_uniform_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
        let y_min = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
        let y_max = self.lower_optional_arg(args, 2, span, scope, call_depth)?;
        let span = random_args_span(args, span);
        let width = self.emit_binary_at(BinaryOp::Sub, y_max, y_min, span)?;
        let offset = self.emit_binary_at(BinaryOp::Mul, u, width, span)?;
        self.emit_binary_at(BinaryOp::Add, y_min, offset, span)
    }

    pub(super) fn lower_normal_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
        let mu = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
        let sigma = if args.len() > 2 {
            self.lower_optional_arg(args, 2, span, scope, call_depth)?
        } else {
            self.emit_const_at(1.0, random_args_span(args, span))?
        };
        let span = random_args_span(args, span);
        let unit = self.emit_standard_normal_quantile(u, span)?;
        let scaled = self.emit_binary_at(BinaryOp::Mul, sigma, unit, span)?;
        self.emit_binary_at(BinaryOp::Add, mu, scaled, span)
    }

    pub(super) fn lower_weibull_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
        let lambda = self.lower_optional_arg(args, 1, span, scope, call_depth)?;
        let k = self.lower_optional_arg(args, 2, span, scope, call_depth)?;
        let span = random_args_span(args, span);
        let one = self.emit_const_at(1.0, span)?;
        let p = self.emit_open_probability(u, span)?;
        let survival = self.emit_binary_at(BinaryOp::Sub, one, p, span)?;
        let log_survival = self.emit_unary_at(UnaryOp::Log, survival, span)?;
        let neg_log = self.emit_unary_at(UnaryOp::Neg, log_survival, span)?;
        let inv_k = self.emit_binary_at(BinaryOp::Div, one, k, span)?;
        let powered = self.emit_binary_at(BinaryOp::Pow, neg_log, inv_k, span)?;
        self.emit_binary_at(BinaryOp::Mul, lambda, powered, span)
    }

    pub(in crate::lower) fn emit_standard_normal_quantile(
        &mut self,
        p: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        const P_LOW: f64 = 0.02425;
        const A: [f64; 6] = [
            -3.969_683_028_665_376e1,
            2.209_460_984_245_205e2,
            -2.759_285_104_469_687e2,
            1.383_577_518_672_69e2,
            -3.066_479_806_614_716e1,
            2.506_628_277_459_239,
        ];
        const B: [f64; 6] = [
            -5.447_609_879_822_406e1,
            1.615_858_368_580_409e2,
            -1.556_989_798_598_866e2,
            6.680_131_188_771_972e1,
            -1.328_068_155_288_572e1,
            1.0,
        ];
        const C: [f64; 6] = [
            -7.784_894_002_430_293e-3,
            -3.223_964_580_411_365e-1,
            -2.400_758_277_161_838,
            -2.549_732_539_343_734,
            4.374_664_141_464_968,
            2.938_163_982_698_783,
        ];
        const D: [f64; 5] = [
            7.784_695_709_041_462e-3,
            3.224_671_290_700_398e-1,
            2.445_134_137_142_996,
            3.754_408_661_907_416,
            1.0,
        ];

        let p = self.emit_open_probability(p, span)?;
        let half = self.emit_const_at(0.5, span)?;
        let q = self.emit_binary_at(BinaryOp::Sub, p, half, span)?;
        let r = self.emit_binary_at(BinaryOp::Mul, q, q, span)?;
        let central_poly = self.emit_polynomial(r, &A, span)?;
        let central_num = self.emit_binary_at(BinaryOp::Mul, central_poly, q, span)?;
        let central_den = self.emit_polynomial(r, &B, span)?;
        let central = self.emit_binary_at(BinaryOp::Div, central_num, central_den, span)?;

        let minus_two = self.emit_const_at(-2.0, span)?;
        let lower_log = self.emit_unary_at(UnaryOp::Log, p, span)?;
        let lower_log_scaled = self.emit_binary_at(BinaryOp::Mul, minus_two, lower_log, span)?;
        let lower_q = self.emit_unary_at(UnaryOp::Sqrt, lower_log_scaled, span)?;
        let lower = self.emit_rational_tail(lower_q, &C, &D, span)?;

        let one = self.emit_const_at(1.0, span)?;
        let one_minus_p = self.emit_binary_at(BinaryOp::Sub, one, p, span)?;
        let upper_log = self.emit_unary_at(UnaryOp::Log, one_minus_p, span)?;
        let upper_log_scaled = self.emit_binary_at(BinaryOp::Mul, minus_two, upper_log, span)?;
        let upper_q = self.emit_unary_at(UnaryOp::Sqrt, upper_log_scaled, span)?;
        let upper_tail = self.emit_rational_tail(upper_q, &C, &D, span)?;
        let upper = self.emit_unary_at(UnaryOp::Neg, upper_tail, span)?;

        let low_cutoff = self.emit_const_at(P_LOW, span)?;
        let high_cutoff = self.emit_const_at(1.0 - P_LOW, span)?;
        let low = self.emit_compare_at(CompareOp::Lt, p, low_cutoff, span)?;
        let high = self.emit_compare_at(CompareOp::Gt, p, high_cutoff, span)?;
        let high_or_central = self.emit_select_at(high, upper, central, span)?;
        self.emit_select_at(low, lower, high_or_central, span)
    }

    fn emit_open_probability(
        &mut self,
        p: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let eps = self.emit_const_at(1.0e-15, span)?;
        let one_minus_eps = self.emit_const_at(1.0 - 1.0e-15, span)?;
        let above_zero = self.emit_binary_at(BinaryOp::Max, p, eps, span)?;
        self.emit_binary_at(BinaryOp::Min, above_zero, one_minus_eps, span)
    }

    fn emit_rational_tail(
        &mut self,
        q: Reg,
        numerator: &[f64; 6],
        denominator: &[f64; 5],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let num = self.emit_polynomial(q, numerator, span)?;
        let den = self.emit_polynomial(q, denominator, span)?;
        self.emit_binary_at(BinaryOp::Div, num, den, span)
    }

    fn emit_polynomial<const N: usize>(
        &mut self,
        x: Reg,
        coeffs: &[f64; N],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let mut acc = self.emit_const_at(coeffs[0], span)?;
        for coeff in coeffs.iter().skip(1) {
            let product = self.emit_binary_at(BinaryOp::Mul, acc, x, span)?;
            let next = self.emit_const_at(*coeff, span)?;
            acc = self.emit_binary_at(BinaryOp::Add, product, next, span)?;
        }
        Ok(acc)
    }
}

fn zero_based_projection_index(
    projection: &FunctionOutputProjection,
) -> Result<Option<usize>, LowerError> {
    projection
        .indices
        .first()
        .map(|index| {
            index.checked_sub(1).ok_or_else(|| {
                random_contract_error(format!(
                    "random output projection `{}` uses zero subscript",
                    projection.output_name
                ))
            })
        })
        .transpose()
}

fn checked_random_value_capacity_at(
    len: usize,
    extra: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    len.checked_add(extra)
        .ok_or_else(|| random_contract_error_with_span("random output value count overflow", span))
}

fn random_args_span(
    args: &[rumoca_core::Expression],
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    args.first()
        .and_then(rumoca_core::Expression::span)
        .unwrap_or(owner_span)
}

fn random_contract_error(reason: impl Into<String>) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: reason.into(),
    }
}

fn random_contract_error_with_span(
    reason: impl Into<String>,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::contract_violation(reason, span)
}

fn required_automatic_local_seed_path(
    expr: Option<&rumoca_core::Expression>,
    call_span: rumoca_core::Span,
) -> Result<&str, LowerError> {
    literal_string(expr).ok_or_else(|| {
        let span = expr
            .and_then(rumoca_core::Expression::span)
            .unwrap_or(call_span);
        unsupported_at(
            "automaticLocalSeed requires a String path literal; getInstanceName must be lowered before solve IR",
            span,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn projection_with_index(index: usize) -> FunctionOutputProjection {
        FunctionOutputProjection {
            base_function_name: rumoca_core::VarName::new("random"),
            output_name: "stateOut".to_string(),
            output_field: None,
            scope_indices: Vec::new(),
            indices: vec![index],
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn zero_based_projection_index_rejects_zero_without_dummy_span() {
        let err = zero_based_projection_index(&projection_with_index(0))
            .expect_err("zero random projection index must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("random output projection `stateOut` uses zero subscript")
        );
    }

    #[test]
    fn checked_random_value_capacity_does_not_fabricate_dummy_span() {
        let err = checked_random_value_capacity_at(usize::MAX, 1, rumoca_core::Span::DUMMY)
            .expect_err("random output value count overflow must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: random output value count overflow"
        );
    }
}
