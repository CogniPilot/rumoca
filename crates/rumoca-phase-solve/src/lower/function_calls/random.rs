use super::*;

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
                let counter = self.alloc_call_site().saturating_add(1);
                Ok(Some(self.emit_const(
                    rumoca_eval_dae::deterministic_automatic_global_seed(counter) as f64,
                )))
            }
            "Modelica.Math.Random.Utilities.automaticLocalSeed" | "automaticLocalSeed" => {
                let seed = i64::from(rumoca_eval_dae::modelica_strings_hash_string(
                    required_automatic_local_seed_path(args.first(), span)?,
                ));
                Ok(Some(self.emit_const(seed as f64)))
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
                    scope,
                    call_depth,
                )?;
                Ok(Some(self.emit_impure_random_init(seed)))
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
                    scope,
                    call_depth,
                )?;
                Ok(Some(self.emit_impure_random(id)))
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
                    scope,
                    call_depth,
                )?;
                Ok(Some(self.emit_impure_random_integer(id, imin, imax)))
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
        let Some((generator, kind)) = random_intrinsic_kind(projection.base_function_name.as_str())
        else {
            return Ok(None);
        };
        match kind {
            RandomIntrinsicKind::InitialState => {
                if projection.output_name != "state" {
                    return Ok(None);
                }
                let Some(state_index) = projection.indices.first().copied() else {
                    return Ok(None);
                };
                let local_seed = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, scope, call_depth)?;
                let state_len = random_projection_state_len(generator, projection, args)?;
                Ok(Some(self.emit_random_initial_state(
                    generator,
                    local_seed,
                    global_seed,
                    state_len,
                    state_index.saturating_sub(1),
                )))
            }
            RandomIntrinsicKind::Random => match projection.output_name.as_str() {
                "result" if projection.indices.is_empty() => {
                    let state_values = self.lower_random_state_argument(args, scope, call_depth)?;
                    Ok(Some(self.emit_random_result(generator, &state_values)))
                }
                "stateOut" => {
                    let Some(state_index) = projection.indices.first().copied() else {
                        return Ok(None);
                    };
                    let state_values = self.lower_random_state_argument(args, scope, call_depth)?;
                    Ok(Some(self.emit_random_state(
                        generator,
                        &state_values,
                        state_index.saturating_sub(1),
                    )))
                }
                _ => Ok(None),
            },
        }
    }

    pub(super) fn lower_random_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
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
                let local_seed = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, scope, call_depth)?;
                let state_len = random_generator_state_len(generator);
                Ok(Some(self.emit_random_initial_state(
                    generator,
                    local_seed,
                    global_seed,
                    state_len,
                    0,
                )))
            }
            RandomIntrinsicKind::Random => {
                let state_values = self.lower_random_state_argument(args, scope, call_depth)?;
                Ok(Some(self.emit_random_result(generator, &state_values)))
            }
        }
    }

    pub(in crate::lower) fn lower_random_array_values(
        &mut self,
        call_name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        if let Some(projection) = self.lookup_function_output_projection(call_name)
            && let Some((generator, kind)) =
                random_intrinsic_kind(projection.base_function_name.as_str())
        {
            return self.lower_projected_random_array_values(
                generator,
                kind,
                &projection,
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
                let local_seed = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, scope, call_depth)?;
                let state_len = random_generator_state_len(generator);
                (0..state_len)
                    .map(|state_index| {
                        self.emit_random_initial_state(
                            generator,
                            local_seed,
                            global_seed,
                            state_len,
                            state_index,
                        )
                    })
                    .collect()
            }
            RandomIntrinsicKind::Random => {
                let state_values = self.lower_random_state_argument(args, scope, call_depth)?;
                let mut values = Vec::with_capacity(state_values.len().saturating_add(1));
                values.push(self.emit_random_result(generator, &state_values));
                values.extend((0..state_values.len()).map(|state_index| {
                    self.emit_random_state(generator, &state_values, state_index)
                }));
                values
            }
        };
        Ok(Some(values))
    }

    fn lower_projected_random_array_values(
        &mut self,
        generator: RandomGenerator,
        kind: RandomIntrinsicKind,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        match (kind, projection.output_name.as_str()) {
            (RandomIntrinsicKind::InitialState, "state") => {
                let local_seed = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let global_seed = self.lower_optional_arg(args, 1, scope, call_depth)?;
                let state_len = match random_state_len_arg(args)? {
                    Some(len) => len,
                    None => random_generator_state_len(generator),
                };
                Ok(Some(
                    (0..state_len)
                        .map(|state_index| {
                            self.emit_random_initial_state(
                                generator,
                                local_seed,
                                global_seed,
                                state_len,
                                state_index,
                            )
                        })
                        .collect(),
                ))
            }
            (RandomIntrinsicKind::Random, "stateOut") => {
                let state_values = self.lower_random_state_argument(args, scope, call_depth)?;
                Ok(Some(
                    (0..state_values.len())
                        .map(|state_index| {
                            self.emit_random_state(generator, &state_values, state_index)
                        })
                        .collect(),
                ))
            }
            _ => Ok(None),
        }
    }

    fn lower_random_state_argument(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if let Some(state_expr) = args.first() {
            let values = self.lower_array_like_values(state_expr, scope, call_depth)?;
            if !values.is_empty() {
                return Ok(values);
            }
        }
        Ok(vec![self.emit_const(1.0)])
    }

    pub(super) fn lower_optional_arg(
        &mut self,
        args: &[rumoca_core::Expression],
        idx: usize,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(expr) = args.get(idx) {
            self.lower_expr(expr, scope, call_depth)
        } else {
            Ok(self.emit_const(0.0))
        }
    }

    fn lower_named_or_positional_arg(
        &mut self,
        named_args: &IndexMap<String, &rumoca_core::Expression>,
        positional_args: &[&rumoca_core::Expression],
        arg: NamedOrPositionalArg<'_>,
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
            Ok(self.emit_const(arg.default))
        }
    }

    pub(super) fn lower_uniform_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, scope, call_depth)?;
        let y_min = self.lower_optional_arg(args, 1, scope, call_depth)?;
        let y_max = self.lower_optional_arg(args, 2, scope, call_depth)?;
        let span = self.emit_binary(BinaryOp::Sub, y_max, y_min);
        let offset = self.emit_binary(BinaryOp::Mul, u, span);
        Ok(self.emit_binary(BinaryOp::Add, y_min, offset))
    }

    pub(super) fn lower_normal_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, scope, call_depth)?;
        let mu = self.lower_optional_arg(args, 1, scope, call_depth)?;
        let sigma = if args.len() > 2 {
            self.lower_optional_arg(args, 2, scope, call_depth)?
        } else {
            self.emit_const(1.0)
        };
        let unit = self.emit_standard_normal_quantile(u);
        let scaled = self.emit_binary(BinaryOp::Mul, sigma, unit);
        Ok(self.emit_binary(BinaryOp::Add, mu, scaled))
    }

    pub(super) fn lower_weibull_quantile(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let u = self.lower_optional_arg(args, 0, scope, call_depth)?;
        let lambda = self.lower_optional_arg(args, 1, scope, call_depth)?;
        let k = self.lower_optional_arg(args, 2, scope, call_depth)?;
        let one = self.emit_const(1.0);
        let p = self.emit_open_probability(u);
        let survival = self.emit_binary(BinaryOp::Sub, one, p);
        let log_survival = self.emit_unary(UnaryOp::Log, survival);
        let neg_log = self.emit_unary(UnaryOp::Neg, log_survival);
        let inv_k = self.emit_binary(BinaryOp::Div, one, k);
        let powered = self.emit_binary(BinaryOp::Pow, neg_log, inv_k);
        Ok(self.emit_binary(BinaryOp::Mul, lambda, powered))
    }

    pub(super) fn emit_standard_normal_quantile(&mut self, p: Reg) -> Reg {
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

        let p = self.emit_open_probability(p);
        let half = self.emit_const(0.5);
        let q = self.emit_binary(BinaryOp::Sub, p, half);
        let r = self.emit_binary(BinaryOp::Mul, q, q);
        let central_poly = self.emit_polynomial(r, &A);
        let central_num = self.emit_binary(BinaryOp::Mul, central_poly, q);
        let central_den = self.emit_polynomial(r, &B);
        let central = self.emit_binary(BinaryOp::Div, central_num, central_den);

        let minus_two = self.emit_const(-2.0);
        let lower_log = self.emit_unary(UnaryOp::Log, p);
        let lower_log_scaled = self.emit_binary(BinaryOp::Mul, minus_two, lower_log);
        let lower_q = self.emit_unary(UnaryOp::Sqrt, lower_log_scaled);
        let lower = self.emit_rational_tail(lower_q, &C, &D);

        let one = self.emit_const(1.0);
        let one_minus_p = self.emit_binary(BinaryOp::Sub, one, p);
        let upper_log = self.emit_unary(UnaryOp::Log, one_minus_p);
        let upper_log_scaled = self.emit_binary(BinaryOp::Mul, minus_two, upper_log);
        let upper_q = self.emit_unary(UnaryOp::Sqrt, upper_log_scaled);
        let upper_tail = self.emit_rational_tail(upper_q, &C, &D);
        let upper = self.emit_unary(UnaryOp::Neg, upper_tail);

        let low_cutoff = self.emit_const(P_LOW);
        let high_cutoff = self.emit_const(1.0 - P_LOW);
        let low = self.emit_compare(CompareOp::Lt, p, low_cutoff);
        let high = self.emit_compare(CompareOp::Gt, p, high_cutoff);
        let high_or_central = self.emit_select(high, upper, central);
        self.emit_select(low, lower, high_or_central)
    }

    fn emit_open_probability(&mut self, p: Reg) -> Reg {
        let eps = self.emit_const(1.0e-15);
        let one_minus_eps = self.emit_const(1.0 - 1.0e-15);
        let above_zero = self.emit_binary(BinaryOp::Max, p, eps);
        self.emit_binary(BinaryOp::Min, above_zero, one_minus_eps)
    }

    fn emit_rational_tail(&mut self, q: Reg, numerator: &[f64; 6], denominator: &[f64; 5]) -> Reg {
        let num = self.emit_polynomial(q, numerator);
        let den = self.emit_polynomial(q, denominator);
        self.emit_binary(BinaryOp::Div, num, den)
    }

    fn emit_polynomial<const N: usize>(&mut self, x: Reg, coeffs: &[f64; N]) -> Reg {
        let mut acc = self.emit_const(coeffs[0]);
        for coeff in coeffs.iter().skip(1) {
            let product = self.emit_binary(BinaryOp::Mul, acc, x);
            let next = self.emit_const(*coeff);
            acc = self.emit_binary(BinaryOp::Add, product, next);
        }
        acc
    }
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
