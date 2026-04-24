use rumoca_eval_dae::runtime::VarEnv;
use rumoca_eval_dae::runtime::sim_float::SimFloat;
use rumoca_ir_dae as dae;

use rumoca_phase_structural::scalarize::build_output_names;
use crate::simulation::dae_prepare::{expr_contains_der_of, expr_refers_to_var};
use crate::simulation::pipeline::MassMatrix;

pub fn sim_introspect_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

pub fn sim_introspect_params_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT_PARAMS").is_ok()
}

pub fn sim_trace_enabled() -> bool {
    sim_introspect_enabled() || std::env::var("RUMOCA_SIM_TRACE").is_ok()
}

pub fn sim_introspect_eq_limit() -> usize {
    std::env::var("RUMOCA_SIM_INTROSPECT_EQ_LIMIT")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or(120)
}

pub fn sim_introspect_expr_chars() -> usize {
    std::env::var("RUMOCA_SIM_INTROSPECT_EXPR_CHARS")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or(260)
}

pub fn sim_introspect_var_limit() -> usize {
    std::env::var("RUMOCA_SIM_INTROSPECT_VAR_LIMIT")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .filter(|&n| n > 0)
        .unwrap_or(80)
}

pub fn truncate_debug(s: &str, max_chars: usize) -> String {
    if s.chars().count() <= max_chars {
        return s.to_string();
    }
    let mut out = String::with_capacity(max_chars + 1);
    for (i, ch) in s.chars().enumerate() {
        if i >= max_chars {
            break;
        }
        out.push(ch);
    }
    out.push('…');
    out
}

fn expr_contains_var_ref(expr: &dae::Expression, name: &dae::VarName) -> bool {
    if expr_refers_to_var(expr, name) {
        return true;
    }
    match expr {
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_ref(lhs, name) || expr_contains_var_ref(rhs, name)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_var_ref(rhs, name),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_var_ref(a, name))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, e)| expr_contains_var_ref(c, name) || expr_contains_var_ref(e, name))
                || expr_contains_var_ref(else_branch, name)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_var_ref(e, name))
        }
        dae::Expression::Index { base, .. } => expr_contains_var_ref(base, name),
        _ => false,
    }
}

fn der_state_names_in_eq(dae: &dae::Dae, eq: &dae::Equation) -> Vec<String> {
    dae.states
        .keys()
        .filter(|name| expr_contains_der_of(&eq.rhs, name))
        .map(|name| name.as_str().to_string())
        .collect()
}

#[derive(Debug, Clone)]
pub struct DivisionByZeroExprSite {
    pub numerator: f64,
    pub denominator: f64,
    pub divisor_expr: String,
}

#[derive(Debug, Clone)]
pub struct DivisionByZeroEquationSite {
    pub equation_set: &'static str,
    pub equation_index: usize,
    pub origin: String,
    pub rhs_expr: String,
    pub expr_site: DivisionByZeroExprSite,
}

fn format_divisor_expr(expr: &dae::Expression) -> String {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            name.as_str().to_string()
        }
        _ => truncate_debug(&format!("{expr:?}"), 140),
    }
}

fn expr_contains_runtime_unknown(dae: &dae::Dae, candidate: &dae::Expression) -> bool {
    dae.states
        .keys()
        .chain(dae.algebraics.keys())
        .chain(dae.outputs.keys())
        .chain(dae.inputs.keys())
        .chain(dae.discrete_reals.keys())
        .chain(dae.discrete_valued.keys())
        .chain(dae.derivative_aliases.keys())
        .any(|name| expr_contains_var_ref(candidate, name))
}

fn division_by_zero_site(
    numerator: f64,
    denominator: f64,
    divisor_expr: &dae::Expression,
) -> Option<DivisionByZeroExprSite> {
    if denominator != 0.0 {
        return None;
    }
    Some(DivisionByZeroExprSite {
        numerator,
        denominator,
        divisor_expr: format_divisor_expr(divisor_expr),
    })
}

#[derive(Clone, Copy)]
struct DivisionByZeroCallbacks<FEvalScalar, FEvalBool> {
    eval_scalar: FEvalScalar,
    eval_bool: FEvalBool,
}

impl<FEvalScalar, FEvalBool> DivisionByZeroCallbacks<FEvalScalar, FEvalBool>
where
    FEvalScalar: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FEvalBool: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<bool>,
{
    fn new(eval_scalar: FEvalScalar, eval_bool: FEvalBool) -> Self {
        Self {
            eval_scalar,
            eval_bool,
        }
    }

    fn scalar(self, expr: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
        (self.eval_scalar)(expr, env)
    }

    fn boolean(self, expr: &dae::Expression, env: &VarEnv<f64>) -> Option<bool> {
        (self.eval_bool)(expr, env)
    }
}

struct DivisionByZeroScanner<'a, FEvalScalar, FEvalBool> {
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    callbacks: DivisionByZeroCallbacks<FEvalScalar, FEvalBool>,
}

impl<'a, FEvalScalar, FEvalBool> DivisionByZeroScanner<'a, FEvalScalar, FEvalBool>
where
    FEvalScalar: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FEvalBool: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<bool>,
{
    fn new(
        dae: &'a dae::Dae,
        env: &'a VarEnv<f64>,
        callbacks: DivisionByZeroCallbacks<FEvalScalar, FEvalBool>,
    ) -> Self {
        Self {
            dae,
            env,
            callbacks,
        }
    }

    fn scalar_expr(&self, expr: &dae::Expression) -> Option<f64> {
        if let Some(value) = self.callbacks.scalar(expr, self.env) {
            return Some(value);
        }
        match expr {
            dae::Expression::Literal(dae::Literal::Real(value)) => Some(*value),
            dae::Expression::Literal(dae::Literal::Integer(value)) => Some(*value as f64),
            dae::Expression::Literal(dae::Literal::Boolean(value)) => {
                Some(<f64 as SimFloat>::from_bool(*value))
            }
            dae::Expression::Unary { op, rhs } => {
                let value = self.scalar_expr(rhs)?;
                Some(match op {
                    rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => {
                        -value
                    }
                    rumoca_ir_core::OpUnary::Plus(_)
                    | rumoca_ir_core::OpUnary::DotPlus(_)
                    | rumoca_ir_core::OpUnary::Empty => value,
                    rumoca_ir_core::OpUnary::Not(_) => {
                        <f64 as SimFloat>::from_bool(!value.to_bool())
                    }
                })
            }
            dae::Expression::Binary { op, lhs, rhs } => {
                let lhs_value = self.scalar_expr(lhs)?;
                let rhs_value = self.scalar_expr(rhs)?;
                Some(match op {
                    rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => {
                        lhs_value + rhs_value
                    }
                    rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => {
                        lhs_value - rhs_value
                    }
                    rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => {
                        lhs_value * rhs_value
                    }
                    rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
                        lhs_value / rhs_value
                    }
                    rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
                        lhs_value.powf(rhs_value)
                    }
                    rumoca_ir_core::OpBinary::And(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value.to_bool() && rhs_value.to_bool())
                    }
                    rumoca_ir_core::OpBinary::Or(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value.to_bool() || rhs_value.to_bool())
                    }
                    rumoca_ir_core::OpBinary::Lt(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value < rhs_value)
                    }
                    rumoca_ir_core::OpBinary::Le(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value <= rhs_value)
                    }
                    rumoca_ir_core::OpBinary::Gt(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value > rhs_value)
                    }
                    rumoca_ir_core::OpBinary::Ge(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value >= rhs_value)
                    }
                    rumoca_ir_core::OpBinary::Eq(_) => {
                        <f64 as SimFloat>::from_bool(lhs_value.eq_approx(rhs_value))
                    }
                    rumoca_ir_core::OpBinary::Neq(_) => {
                        <f64 as SimFloat>::from_bool(!lhs_value.eq_approx(rhs_value))
                    }
                    rumoca_ir_core::OpBinary::Empty | rumoca_ir_core::OpBinary::Assign(_) => 0.0,
                })
            }
            dae::Expression::If {
                branches,
                else_branch,
            } => self
                .selected_if_branch(branches, else_branch)
                .and_then(|branch| self.scalar_expr(branch)),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::NoEvent,
                args,
            } if !args.is_empty() => self.scalar_expr(&args[0]),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Smooth,
                args,
            } if args.len() >= 2 => self.scalar_expr(&args[1]),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Homotopy | dae::BuiltinFunction::Scalar,
                args,
            } if !args.is_empty() => self.scalar_expr(&args[0]),
            dae::Expression::Array { elements, .. } if elements.len() == 1 => {
                self.scalar_expr(&elements[0])
            }
            dae::Expression::Tuple { elements } if elements.len() == 1 => {
                self.scalar_expr(&elements[0])
            }
            _ => None,
        }
    }

    fn bool_expr(&self, expr: &dae::Expression) -> Option<bool> {
        self.callbacks
            .boolean(expr, self.env)
            .or_else(|| self.scalar_expr(expr).map(|value| value.to_bool()))
    }

    fn selected_if_branch<'b>(
        &self,
        branches: &'b [(dae::Expression, dae::Expression)],
        else_branch: &'b dae::Expression,
    ) -> Option<&'b dae::Expression> {
        for (condition, branch) in branches {
            if self.bool_expr(condition)? {
                return Some(branch);
            }
        }
        Some(else_branch)
    }

    fn binary(
        &self,
        lhs: &dae::Expression,
        rhs: &dae::Expression,
    ) -> Option<DivisionByZeroExprSite> {
        self.expr(lhs).or_else(|| self.expr(rhs)).or_else(|| {
            if expr_contains_runtime_unknown(self.dae, rhs) {
                return None;
            }
            division_by_zero_site(self.scalar_expr(lhs)?, self.scalar_expr(rhs)?, rhs)
        })
    }

    fn builtin(&self, args: &[dae::Expression]) -> Option<DivisionByZeroExprSite> {
        args.iter().find_map(|arg| self.expr(arg)).or_else(|| {
            let [numerator_expr, denominator_expr, ..] = args else {
                return None;
            };
            if expr_contains_runtime_unknown(self.dae, denominator_expr) {
                return None;
            }
            division_by_zero_site(
                self.scalar_expr(numerator_expr)?,
                self.scalar_expr(denominator_expr)?,
                denominator_expr,
            )
        })
    }

    fn if_expr(
        &self,
        branches: &[(dae::Expression, dae::Expression)],
        else_branch: &dae::Expression,
    ) -> Option<DivisionByZeroExprSite> {
        for (condition, branch) in branches {
            if let Some(site) = self.expr(condition) {
                return Some(site);
            }
            if self.bool_expr(condition)? {
                return self.expr(branch);
            }
        }
        self.expr(else_branch)
    }

    fn first_element(&self, elements: &[dae::Expression]) -> Option<DivisionByZeroExprSite> {
        elements.first().and_then(|first| self.expr(first))
    }

    fn range(
        &self,
        start: &dae::Expression,
        step: Option<&dae::Expression>,
        end: &dae::Expression,
    ) -> Option<DivisionByZeroExprSite> {
        self.expr(start)
            .or_else(|| step.and_then(|value| self.expr(value)))
            .or_else(|| self.expr(end))
    }

    fn subscripts(&self, subscripts: &[dae::Subscript]) -> Option<DivisionByZeroExprSite> {
        subscripts.iter().find_map(|sub| match sub {
            dae::Subscript::Expr(sub_expr) => self.expr(sub_expr),
            dae::Subscript::Index(_) | dae::Subscript::Colon => None,
        })
    }

    fn index(
        &self,
        base: &dae::Expression,
        subscripts: &[dae::Subscript],
    ) -> Option<DivisionByZeroExprSite> {
        self.expr(base).or_else(|| self.subscripts(subscripts))
    }

    fn array_comprehension(
        &self,
        expr: &dae::Expression,
        indices: &[dae::ComprehensionIndex],
        filter: Option<&dae::Expression>,
    ) -> Option<DivisionByZeroExprSite> {
        self.expr(expr)
            .or_else(|| indices.iter().find_map(|index| self.expr(&index.range)))
            .or_else(|| filter.and_then(|value| self.expr(value)))
    }

    fn expr(&self, expr: &dae::Expression) -> Option<DivisionByZeroExprSite> {
        match expr {
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_),
                lhs,
                rhs,
            } => self.binary(lhs, rhs),
            dae::Expression::Binary { lhs, rhs, .. } => self.expr(lhs).or_else(|| self.expr(rhs)),
            dae::Expression::Unary { rhs, .. } => self.expr(rhs),
            dae::Expression::BuiltinCall { function, args } => {
                if matches!(
                    function,
                    dae::BuiltinFunction::Div
                        | dae::BuiltinFunction::Mod
                        | dae::BuiltinFunction::Rem
                ) {
                    return self.builtin(args);
                }
                args.iter().find_map(|arg| self.expr(arg))
            }
            dae::Expression::FunctionCall { args, .. } => {
                args.iter().find_map(|arg| self.expr(arg))
            }
            dae::Expression::If {
                branches,
                else_branch,
            } => self.if_expr(branches, else_branch),
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                self.first_element(elements)
            }
            dae::Expression::Range { start, step, end } => self.range(start, step.as_deref(), end),
            dae::Expression::Index { base, subscripts } => self.index(base, subscripts),
            dae::Expression::FieldAccess { base, .. } => self.expr(base),
            dae::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => self.array_comprehension(expr, indices, filter.as_deref()),
            dae::Expression::VarRef { subscripts, .. } => self.subscripts(subscripts),
            dae::Expression::Literal(_) | dae::Expression::Empty => None,
        }
    }

    fn equation_site(
        &self,
        equation_set: &'static str,
        equation_index: usize,
        equation: &dae::Equation,
    ) -> Option<DivisionByZeroEquationSite> {
        self.expr(&equation.rhs)
            .map(|expr_site| DivisionByZeroEquationSite {
                equation_set,
                equation_index,
                origin: equation.origin.clone(),
                rhs_expr: truncate_debug(&format!("{:?}", equation.rhs), 220),
                expr_site,
            })
    }

    fn initial_site(&self) -> Option<DivisionByZeroEquationSite> {
        let scan = |equation_set: &'static str, equations: &[dae::Equation]| {
            equations
                .iter()
                .enumerate()
                .find_map(|(equation_index, equation)| {
                    self.equation_site(equation_set, equation_index, equation)
                })
        };
        scan("f_x", &self.dae.f_x)
            .or_else(|| scan("initial_equations", &self.dae.initial_equations))
    }
}

pub fn find_initial_division_by_zero_site_with_callbacks<FEvalScalar, FEvalBool>(
    dae: &dae::Dae,
    env: &VarEnv<f64>,
    eval_scalar: FEvalScalar,
    eval_bool: FEvalBool,
) -> Option<DivisionByZeroEquationSite>
where
    FEvalScalar: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FEvalBool: Copy + Fn(&dae::Expression, &VarEnv<f64>) -> Option<bool>,
{
    DivisionByZeroScanner::new(
        dae,
        env,
        DivisionByZeroCallbacks::new(eval_scalar, eval_bool),
    )
    .initial_site()
}

pub fn dump_missing_state_equation_diagnostics(dae: &dae::Dae, missing_state: &str) {
    if !sim_introspect_enabled() {
        return;
    }
    let target = dae::VarName::new(missing_state);
    let eq_limit = sim_introspect_eq_limit();
    let mut der_hits = 0usize;
    let mut ref_hits = 0usize;
    for eq in &dae.f_x {
        if expr_contains_der_of(&eq.rhs, &target) {
            der_hits += 1;
        }
        if expr_contains_var_ref(&eq.rhs, &target) {
            ref_hits += 1;
        }
    }
    let state_meta = dae
        .states
        .get(&target)
        .map(|v| format!("size={} dims={:?} start={:?}", v.size(), v.dims, v.start))
        .unwrap_or_else(|| "<not in dae.states>".to_string());
    eprintln!(
        "[sim-introspect] MissingStateEquation state={} meta={} der_hits={} ref_hits={} total_eqs={}",
        missing_state,
        state_meta,
        der_hits,
        ref_hits,
        dae.f_x.len()
    );
    for (i, eq) in dae.f_x.iter().take(eq_limit).enumerate() {
        let has_der = expr_contains_der_of(&eq.rhs, &target);
        let has_ref = expr_contains_var_ref(&eq.rhs, &target);
        if !has_der && !has_ref {
            continue;
        }
        let rhs = truncate_debug(&format!("{:?}", eq.rhs), 320);
        eprintln!(
            "[sim-introspect] missing-state eq[{i}] origin={} sc={} has_der={} has_ref={} rhs={}",
            eq.origin, eq.scalar_count, has_der, has_ref, rhs
        );
    }
}

fn dump_transformed_dae_summary(dae: &dae::Dae) {
    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let n_z: usize = dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    let n_eq = dae.f_x.len();
    eprintln!(
        "[sim-introspect] transformed DAE: balance={} states={} algebraics+outputs={} eqs={}",
        rumoca_eval_dae::analysis::balance(dae),
        n_x,
        n_z,
        n_eq
    );
    eprintln!(
        "[sim-introspect] discrete partitions: f_z={} f_m={} discrete_reals={} discrete_valued={}",
        dae.f_z.len(),
        dae.f_m.len(),
        dae.discrete_reals.len(),
        dae.discrete_valued.len()
    );
}

fn dump_transformed_unknowns(dae: &dae::Dae) {
    let expanded_unknown_names = build_output_names(dae);
    let var_limit = sim_introspect_var_limit();
    for (i, name) in expanded_unknown_names.iter().take(var_limit).enumerate() {
        eprintln!("[sim-introspect] unknown[{i}] {name}");
    }
    if expanded_unknown_names.len() > var_limit {
        eprintln!(
            "[sim-introspect] ... omitted {} unknowns (set RUMOCA_SIM_INTROSPECT_VAR_LIMIT to increase)",
            expanded_unknown_names.len() - var_limit
        );
    }
}

fn dump_transformed_state_rows(dae: &dae::Dae, mass_matrix: &MassMatrix) {
    let mut scalar_row = 0usize;
    for (i, (name, var)) in dae.states.iter().enumerate() {
        let size = var.size();
        let row_start = scalar_row;
        let row_end = row_start + size.saturating_sub(1);
        let der_rows = (row_start..(row_start + size))
            .filter(|&row| {
                dae.f_x
                    .get(row)
                    .is_some_and(|eq| expr_contains_der_of(&eq.rhs, name))
            })
            .count();
        let origin = dae
            .f_x
            .get(row_start)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-row>");
        let mass_diag = mass_matrix
            .get(row_start)
            .and_then(|row| row.get(row_start))
            .copied()
            .unwrap_or(1.0);
        let offdiag_terms = mass_matrix
            .get(row_start)
            .map(|row| {
                row.iter()
                    .enumerate()
                    .filter(|(col, coeff)| *col != row_start && coeff.abs() > 1.0e-12)
                    .count()
            })
            .unwrap_or(0);
        eprintln!(
            "[sim-introspect] state[{i}] {} size={} rows={}..{} der_rows={}/{} mass_row0_diag={} mass_row0_offdiag={} origin_row0={}",
            name.as_str(),
            size,
            row_start,
            row_end,
            der_rows,
            size,
            mass_diag,
            offdiag_terms,
            origin
        );
        scalar_row += size;
    }
}

fn dump_fx_equations(dae: &dae::Dae, eq_limit: usize, expr_chars: usize) {
    for (i, eq) in dae.f_x.iter().take(eq_limit).enumerate() {
        let der_states = der_state_names_in_eq(dae, eq);
        let rhs = truncate_debug(&format!("{:?}", eq.rhs), expr_chars);
        eprintln!(
            "[sim-introspect] eq[{i}] origin={} sc={} der_states={:?} rhs={}",
            eq.origin, eq.scalar_count, der_states, rhs
        );
    }
    if dae.f_x.len() > eq_limit {
        eprintln!(
            "[sim-introspect] ... omitted {} equations (set RUMOCA_SIM_INTROSPECT_EQ_LIMIT to increase)",
            dae.f_x.len() - eq_limit
        );
    }
}

fn dump_partition_equations(
    label: &str,
    equations: &[dae::Equation],
    eq_limit: usize,
    expr_chars: usize,
) {
    for (i, eq) in equations.iter().take(eq_limit).enumerate() {
        let rhs = truncate_debug(&format!("{:?}", eq.rhs), expr_chars);
        eprintln!(
            "[sim-introspect] {label}[{i}] origin={} sc={} lhs={:?} rhs={}",
            eq.origin, eq.scalar_count, eq.lhs, rhs
        );
    }
    if equations.len() > eq_limit {
        eprintln!(
            "[sim-introspect] ... omitted {} {label} equations (set RUMOCA_SIM_INTROSPECT_EQ_LIMIT to increase)",
            equations.len() - eq_limit
        );
    }
}

pub fn dump_transformed_dae_for_solver(dae: &dae::Dae, mass_matrix: &MassMatrix) {
    if !sim_introspect_enabled() {
        return;
    }
    dump_transformed_dae_summary(dae);
    dump_transformed_unknowns(dae);
    dump_transformed_state_rows(dae, mass_matrix);
    let eq_limit = sim_introspect_eq_limit();
    let expr_chars = sim_introspect_expr_chars();
    dump_fx_equations(dae, eq_limit, expr_chars);
    dump_partition_equations("f_z", &dae.f_z, eq_limit, expr_chars);
    dump_partition_equations("f_m", &dae.f_m, eq_limit, expr_chars);
}

pub fn dump_initial_vector_for_solver(names: &[String], y0: &[f64]) {
    if !sim_introspect_enabled() {
        return;
    }
    let show = y0.len().min(40);
    eprintln!(
        "[sim-introspect] initial vector y0 size={} (showing {})",
        y0.len(),
        show
    );
    for (i, value) in y0.iter().copied().enumerate().take(show) {
        let name = names
            .get(i)
            .map(std::string::String::as_str)
            .unwrap_or("<unnamed>");
        eprintln!("[sim-introspect] y0[{i}] {} = {}", name, value);
    }
    if y0.len() > show {
        eprintln!(
            "[sim-introspect] ... omitted {} initial entries",
            y0.len() - show
        );
    }
}

pub fn dump_initial_residual_summary(dae: &dae::Dae, rhs: &[f64], n_x: usize) {
    if !sim_introspect_enabled() {
        return;
    }
    let n_total = rhs.len();
    let ode_max = rhs[..n_x].iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
    let alg_max = rhs[n_x..].iter().map(|v| v.abs()).fold(0.0_f64, f64::max);
    let alg_l2 = rhs[n_x..].iter().map(|v| v * v).sum::<f64>().sqrt();
    eprintln!(
        "[sim-introspect] initial residual summary: n_x={} n_total={} ode_max_abs={} alg_max_abs={} alg_l2={}",
        n_x, n_total, ode_max, alg_max, alg_l2
    );

    let mut worst: Vec<(usize, f64)> = rhs
        .iter()
        .enumerate()
        .skip(n_x)
        .map(|(i, v)| (i, v.abs()))
        .collect();
    worst.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    for (i, abs_v) in worst.into_iter().take(8) {
        let eq = &dae.f_x[i];
        eprintln!(
            "[sim-introspect] initial residual top eq[{i}] abs={} origin={} rhs={}",
            abs_v,
            eq.origin,
            truncate_debug(&format!("{:?}", eq.rhs), 220)
        );
    }
}

pub fn dump_parameter_vector(dae: &dae::Dae, params: &[f64]) {
    if !sim_introspect_enabled() || !sim_introspect_params_enabled() {
        return;
    }

    let mut names = Vec::new();
    for (name, var) in &dae.parameters {
        let size = var.size();
        if size <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 0..size {
                names.push(format!("{}[{}]", name.as_str(), i + 1));
            }
        }
    }

    let mapped = names.len().min(params.len());
    let finite = params.iter().filter(|v| v.is_finite()).count();
    let non_finite = params.len().saturating_sub(finite);
    let large = params.iter().filter(|v| v.abs() >= 1.0e6).count();
    eprintln!(
        "[sim-introspect] parameter vector size={} mapped={} finite={} non_finite={} |abs|>=1e6={}",
        params.len(),
        mapped,
        finite,
        non_finite,
        large
    );

    let mut ranked: Vec<(usize, f64)> = params
        .iter()
        .copied()
        .enumerate()
        .map(|(idx, value)| (idx, value.abs()))
        .collect();
    ranked.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    for (idx, abs_value) in ranked.into_iter().take(20) {
        let value = params[idx];
        let name = names
            .get(idx)
            .map(std::string::String::as_str)
            .unwrap_or("<unmapped-param>");
        eprintln!(
            "[sim-introspect] parameter top idx={} name={} value={} abs={}",
            idx, name, value, abs_value
        );
    }

    if let Ok(raw_match) = std::env::var("RUMOCA_SIM_INTROSPECT_PARAMS_MATCH") {
        let needles: Vec<String> = raw_match
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(str::to_string)
            .collect();
        if !needles.is_empty() {
            eprintln!(
                "[sim-introspect] parameter match filter: {}",
                needles.join(",")
            );
            for (idx, (name, value)) in names
                .iter()
                .zip(params.iter().copied())
                .enumerate()
                .filter(|(_, (name, _))| needles.iter().any(|needle| name.contains(needle)))
            {
                eprintln!(
                    "[sim-introspect] parameter match idx={} name={} value={}",
                    idx, name, value
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::scalar_eval::{eval_scalar_bool_expr_fast, eval_scalar_expr_fast};
    use rumoca_core::Span;

    #[test]
    fn find_initial_division_by_zero_site_handles_fast_if_condition() {
        let mut dae = dae::Dae::default();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::VarRef {
                        name: dae::VarName::new("flag"),
                        subscripts: vec![],
                    },
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Div(Default::default()),
                        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                    },
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            },
            Span::DUMMY,
            "fast_if_div0",
        ));

        let mut env = VarEnv::<f64>::new();
        env.set("flag", 1.0);
        let site = find_initial_division_by_zero_site_with_callbacks(
            &dae,
            &env,
            eval_scalar_expr_fast,
            eval_scalar_bool_expr_fast,
        )
        .expect("division-by-zero site");
        assert_eq!(site.origin, "fast_if_div0");
        assert_eq!(site.expr_site.denominator, 0.0);
    }

    #[test]
    fn find_initial_division_by_zero_site_skips_unsupported_condition_without_eval_fallback() {
        let mut dae = dae::Dae::default();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::FunctionCall {
                        name: dae::VarName::new("unsupportedCond"),
                        args: vec![],
                        is_constructor: false,
                    },
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Div(Default::default()),
                        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                    },
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            },
            Span::DUMMY,
            "unsupported_if_div0",
        ));

        let env = VarEnv::<f64>::new();
        assert!(
            find_initial_division_by_zero_site_with_callbacks(
                &dae,
                &env,
                eval_scalar_expr_fast,
                eval_scalar_bool_expr_fast,
            )
            .is_none()
        );
    }

    #[test]
    fn find_initial_division_by_zero_site_handles_singleton_tuple_wrapper() {
        let mut dae = dae::Dae::default();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Tuple {
                elements: vec![dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Div(Default::default()),
                    lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                }],
            },
            Span::DUMMY,
            "tuple_div0",
        ));

        let env = VarEnv::<f64>::new();
        let site = find_initial_division_by_zero_site_with_callbacks(
            &dae,
            &env,
            eval_scalar_expr_fast,
            eval_scalar_bool_expr_fast,
        )
        .expect("division-by-zero site");
        assert_eq!(site.origin, "tuple_div0");
        assert_eq!(site.expr_site.denominator, 0.0);
    }

    #[test]
    fn find_initial_division_by_zero_site_handles_range_step_expression() {
        let mut dae = dae::Dae::default();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Range {
                start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                step: Some(Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Div(Default::default()),
                    lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                })),
                end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            },
            Span::DUMMY,
            "range_div0",
        ));

        let env = VarEnv::<f64>::new();
        let site = find_initial_division_by_zero_site_with_callbacks(
            &dae,
            &env,
            eval_scalar_expr_fast,
            eval_scalar_bool_expr_fast,
        )
        .expect("division-by-zero site");
        assert_eq!(site.origin, "range_div0");
        assert_eq!(site.expr_site.denominator, 0.0);
    }

    #[test]
    fn find_initial_division_by_zero_site_handles_index_base_expression() {
        let mut dae = dae::Dae::default();
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Index {
                base: Box::new(dae::Expression::Tuple {
                    elements: vec![dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Div(Default::default()),
                        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                    }],
                }),
                subscripts: vec![dae::Subscript::Index(1)],
            },
            Span::DUMMY,
            "index_div0",
        ));

        let env = VarEnv::<f64>::new();
        let site = find_initial_division_by_zero_site_with_callbacks(
            &dae,
            &env,
            eval_scalar_expr_fast,
            eval_scalar_bool_expr_fast,
        )
        .expect("division-by-zero site");
        assert_eq!(site.origin, "index_div0");
        assert_eq!(site.expr_site.denominator, 0.0);
    }

    #[test]
    fn division_by_zero_scanner_skips_unsupported_if_condition_when_callback_declines() {
        let expr = dae::Expression::If {
            branches: vec![(
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("unsupportedCond"),
                    args: vec![],
                    is_constructor: false,
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Div(Default::default()),
                    lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                },
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        };
        let dae = dae::Dae::default();
        let env = VarEnv::<f64>::new();
        let site = DivisionByZeroScanner::new(
            &dae,
            &env,
            DivisionByZeroCallbacks::new(eval_scalar_expr_fast, eval_scalar_bool_expr_fast),
        )
        .expr(&expr);
        assert!(site.is_none());
    }
}
