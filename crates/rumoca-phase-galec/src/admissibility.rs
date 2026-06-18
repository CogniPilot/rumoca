use std::collections::HashSet;

use rumoca_core::{BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Statement, VarName};
use rumoca_ir_dae as dae;

#[derive(Debug, Clone, Copy)]
pub enum GalecProfile {
    Efmi10,
}

#[derive(Debug)]
pub struct GalecAdmissibleDae<'a> {
    pub dae: &'a dae::Dae,
    pub profile: GalecProfile,
}

#[derive(Debug, Default)]
pub struct GalecAdmissibilityReport {
    pub violations: Vec<GalecViolation>,
}

#[derive(Debug)]
pub struct GalecViolation {
    pub message: String,
}

#[derive(Debug, thiserror::Error)]
#[error("DAE model is not GALEC-admissible")]
pub struct GalecAdmissibilityError {
    pub report: GalecAdmissibilityReport,
}

pub fn check_galec_admissible(
    dae: &dae::Dae,
    profile: GalecProfile,
) -> Result<GalecAdmissibleDae<'_>, GalecAdmissibilityError> {
    let mut report = GalecAdmissibilityReport::default();

    if let Err(error) = dae.validate_shape_contract() {
        push(&mut report, format!("DAE shape contract failed: {error:?}"));
    }

    check_sample_clock(dae, &mut report);
    check_discrete_only_profile(dae, &mut report);
    check_unsupported_partitions(dae, &mut report);
    check_variables(dae, &mut report);
    check_equations(dae, &mut report);

    if !report.violations.is_empty() {
        return Err(GalecAdmissibilityError { report });
    }

    Ok(GalecAdmissibleDae { dae, profile })
}

pub fn check_galec_statements_admissible(
    statements: &[Statement],
    _profile: GalecProfile,
) -> Result<(), GalecAdmissibilityError> {
    let mut report = GalecAdmissibilityReport::default();

    for statement in statements {
        check_supported_stmt(statement, &mut report);
    }

    if !report.violations.is_empty() {
        return Err(GalecAdmissibilityError { report });
    }

    Ok(())
}

pub(crate) fn fixed_sample_period_variable(dae: &dae::Dae) -> Option<(&VarName, &dae::Variable)> {
    let [schedule] = dae.clocks.schedules.as_slice() else {
        return None;
    };

    let mut matches = dae.variables.constants.iter().filter(|(_, variable)| {
        variable.fixed != Some(false)
            && unit_is_seconds(variable.unit.as_deref())
            && literal_real_value(variable.start.as_ref())
                .is_some_and(|value| floats_equal(value, schedule.period_seconds))
    });

    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn check_sample_clock(dae: &dae::Dae, report: &mut GalecAdmissibilityReport) {
    match dae.clocks.schedules.as_slice() {
        [schedule] => {
            if !schedule.period_seconds.is_finite() || schedule.period_seconds <= 0.0 {
                push(
                    report,
                    "GALEC sample period must be a positive finite fixed period".to_string(),
                );
            }
            if schedule.phase_seconds != 0.0 {
                push(
                    report,
                    "initial GALEC profile only supports zero-phase periodic clocks".to_string(),
                );
            }
            if fixed_sample_period_variable(dae).is_none() {
                push(
                    report,
                    "eFMI Algorithm Code requires the fixed sample period to be represented by exactly one constant variable with a seconds unit and matching literal start value"
                        .to_string(),
                );
            }
        }
        [] => push(
            report,
            "GALEC Algorithm Code requires exactly one fixed sample clock".to_string(),
        ),
        _ => push(
            report,
            "initial GALEC profile supports exactly one fixed sample clock".to_string(),
        ),
    }

    if !dae.clocks.constructor_exprs.is_empty() || !dae.clocks.triggered_conditions.is_empty() {
        push(
            report,
            "dynamic clock constructors are not supported by the initial GALEC profile".to_string(),
        );
    }
}

fn check_discrete_only_profile(dae: &dae::Dae, report: &mut GalecAdmissibilityReport) {
    if !dae.variables.states.is_empty() {
        push(
            report,
            "continuous DAE states are not supported by the initial GALEC profile; provide an already-discrete sampled model"
                .to_string(),
        );
    }

    if !dae.continuous.equations.is_empty() {
        push(
            report,
            "continuous DAE equations are not supported by the initial GALEC profile; provide discrete update equations instead"
                .to_string(),
        );
    }
}

fn check_unsupported_partitions(dae: &dae::Dae, report: &mut GalecAdmissibilityReport) {
    if !dae.continuous.for_equations.is_empty() || !dae.initialization.for_equations.is_empty() {
        push(
            report,
            "for-equation metadata is not supported until bounded GALEC loop lowering exists"
                .to_string(),
        );
    }

    if !dae.conditions.equations.is_empty() || !dae.conditions.relations.is_empty() {
        push(
            report,
            "DAE condition partitions are not supported by the initial GALEC profile".to_string(),
        );
    }

    if !dae.events.synthetic_root_conditions.is_empty()
        || !dae.events.scheduled_time_events.is_empty()
        || !dae.events.event_actions.is_empty()
    {
        push(
            report,
            "DAE event partitions are not supported by the initial GALEC profile".to_string(),
        );
    }

    if dae.metadata.interface_flow_count != 0
        || dae.metadata.overconstrained_interface_count != 0
        || dae.metadata.oc_break_edge_scalar_count != 0
    {
        push(
            report,
            "flow and overconstrained interface metadata is not supported by GALEC lowering"
                .to_string(),
        );
    }

    if !dae.symbols.functions.is_empty() {
        push(
            report,
            "user-defined DAE functions are not supported by the initial GALEC profile".to_string(),
        );
    }

    if !dae.symbols.enum_literal_ordinals.is_empty() {
        push(
            report,
            "enum-valued DAE metadata is not supported by the initial GALEC profile".to_string(),
        );
    }
}

fn check_variables(dae: &dae::Dae, report: &mut GalecAdmissibilityReport) {
    for variable in all_variables(dae) {
        for dim in &variable.dims {
            if *dim <= 0 {
                push(
                    report,
                    format!(
                        "variable `{}` has non-positive GALEC array dimension `{dim}`",
                        variable.name.as_str()
                    ),
                );
            }
        }

        check_literal_metadata(variable, report);
        check_range_metadata(variable, report);
    }

    for variable in block_variables(dae) {
        if variable.start.is_none() {
            push(
                report,
                format!(
                    "variable `{}` has no start value for the eFMI Algorithm Code manifest",
                    variable.name.as_str()
                ),
            );
        }
    }
}

fn check_literal_metadata(variable: &dae::Variable, report: &mut GalecAdmissibilityReport) {
    for (attribute, expr) in [
        ("start", variable.start.as_ref()),
        ("min", variable.min.as_ref()),
        ("max", variable.max.as_ref()),
        ("nominal", variable.nominal.as_ref()),
    ] {
        if let Some(expr) = expr
            && !matches!(expr, Expression::Literal { .. })
        {
            push(
                report,
                format!(
                    "variable `{}` has non-literal `{attribute}` metadata; initial GALEC manifest lowering only supports literal metadata",
                    variable.name.as_str()
                ),
            );
        }
    }
}

fn check_range_metadata(variable: &dae::Variable, report: &mut GalecAdmissibilityReport) {
    let Some(start) = literal_value(variable.start.as_ref()) else {
        return;
    };

    match start {
        GalecLiteralValue::Boolean => {
            if variable.min.is_some() || variable.max.is_some() || variable.nominal.is_some() {
                push(
                    report,
                    format!(
                        "Boolean variable `{}` must not define min, max, or nominal metadata in eFMI Algorithm Code",
                        variable.name.as_str()
                    ),
                );
            }
        }
        GalecLiteralValue::Integer(start) => {
            let min = integer_metadata(variable, "min", variable.min.as_ref(), report);
            let max = integer_metadata(variable, "max", variable.max.as_ref(), report);
            if variable.nominal.is_some() {
                push(
                    report,
                    format!(
                        "Integer variable `{}` must not define nominal metadata in eFMI Algorithm Code",
                        variable.name.as_str()
                    ),
                );
            }
            check_ordered_i64(variable, min, max, report);
            check_start_range_i64(variable, start, min, max, report);
        }
        GalecLiteralValue::Real(start) => {
            let min = real_metadata(variable, "min", variable.min.as_ref(), report);
            let max = real_metadata(variable, "max", variable.max.as_ref(), report);
            let nominal = real_metadata(variable, "nominal", variable.nominal.as_ref(), report);

            check_ordered_f64(variable, min, max, report);
            check_start_range_f64(variable, start, min, max, report);
            if let Some(nominal) = nominal
                && (!nominal.is_finite() || nominal <= 0.0)
            {
                push(
                    report,
                    format!(
                        "Real variable `{}` has nominal metadata that is not > 0",
                        variable.name.as_str()
                    ),
                );
            }
        }
    }
}

fn integer_metadata(
    variable: &dae::Variable,
    attribute: &str,
    expr: Option<&Expression>,
    report: &mut GalecAdmissibilityReport,
) -> Option<i64> {
    match literal_value(expr) {
        Some(GalecLiteralValue::Integer(value)) => Some(value),
        Some(_) => {
            push(
                report,
                format!(
                    "Integer variable `{}` has non-integer `{attribute}` metadata",
                    variable.name.as_str()
                ),
            );
            None
        }
        None => None,
    }
}

fn check_ordered_i64(
    variable: &dae::Variable,
    min: Option<i64>,
    max: Option<i64>,
    report: &mut GalecAdmissibilityReport,
) {
    if let (Some(min), Some(max)) = (min, max)
        && max < min
    {
        push(
            report,
            format!(
                "variable `{}` has max metadata below min",
                variable.name.as_str()
            ),
        );
    }
}

fn check_ordered_f64(
    variable: &dae::Variable,
    min: Option<f64>,
    max: Option<f64>,
    report: &mut GalecAdmissibilityReport,
) {
    if let (Some(min), Some(max)) = (min, max)
        && max < min
    {
        push(
            report,
            format!(
                "variable `{}` has max metadata below min",
                variable.name.as_str()
            ),
        );
    }
}

fn check_start_range_i64(
    variable: &dae::Variable,
    start: i64,
    min: Option<i64>,
    max: Option<i64>,
    report: &mut GalecAdmissibilityReport,
) {
    if min.is_some_and(|min| start < min) || max.is_some_and(|max| start > max) {
        push(
            report,
            format!(
                "variable `{}` has start metadata outside min/max",
                variable.name.as_str()
            ),
        );
    }
}

fn check_start_range_f64(
    variable: &dae::Variable,
    start: f64,
    min: Option<f64>,
    max: Option<f64>,
    report: &mut GalecAdmissibilityReport,
) {
    if min.is_some_and(|min| start < min) || max.is_some_and(|max| start > max) {
        push(
            report,
            format!(
                "variable `{}` has start metadata outside min/max",
                variable.name.as_str()
            ),
        );
    }
}

fn check_equations(dae: &dae::Dae, report: &mut GalecAdmissibilityReport) {
    let roles = RoleSets::new(dae);
    let mut step_assigned = HashSet::new();

    for equation in dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
    {
        check_explicit_equation(
            equation,
            &roles.step_targets,
            &roles.protected_targets,
            &mut step_assigned,
            "DoStep",
            report,
        );
    }

    let mut startup_assigned = HashSet::new();
    let mut recalibrate_assigned = HashSet::new();
    for equation in &dae.initialization.equations {
        let lhs = check_explicit_equation(
            equation,
            &roles.startup_targets,
            &roles.protected_targets,
            &mut startup_assigned,
            "Startup",
            report,
        );

        if let Some(lhs) = lhs
            && roles.dependent_parameters.contains(lhs)
        {
            check_recalibrate_equation(equation, &roles, &mut recalibrate_assigned, report);
        }
    }

    for dependent in &roles.dependent_parameters {
        if !recalibrate_assigned.contains(dependent) {
            push(
                report,
                format!(
                    "dependent parameter `{}` requires an explicit Recalibrate assignment",
                    dependent.as_str()
                ),
            );
        }
    }
}

fn check_recalibrate_equation(
    equation: &dae::Equation,
    roles: &RoleSets,
    assigned: &mut HashSet<VarName>,
    report: &mut GalecAdmissibilityReport,
) {
    let Some(lhs_ref) = &equation.lhs else {
        return;
    };
    let lhs = lhs_ref.var_name();

    if !assigned.insert(lhs.clone()) {
        push(
            report,
            format!(
                "Recalibrate assigns `{}` more than once; GALEC lowering requires sorted single-assignment equations",
                lhs.as_str()
            ),
        );
    }

    let mut refs = HashSet::new();
    collect_var_refs(&equation.rhs, &mut refs);
    for reference in refs {
        if !roles.recalibrate_inputs.contains(&reference) {
            push(
                report,
                format!(
                    "Recalibrate assignment for `{}` references non-parameter variable `{}`",
                    lhs.as_str(),
                    reference.as_str()
                ),
            );
        }
    }
}

fn check_explicit_equation<'a>(
    equation: &'a dae::Equation,
    allowed_targets: &HashSet<VarName>,
    protected_targets: &HashSet<VarName>,
    assigned: &mut HashSet<VarName>,
    method: &str,
    report: &mut GalecAdmissibilityReport,
) -> Option<&'a VarName> {
    let Some(lhs_ref) = &equation.lhs else {
        push(
            report,
            format!(
                "{method} equation `{}` is not explicitly solved for a GALEC assignment target",
                equation.origin
            ),
        );
        return None;
    };
    let lhs = lhs_ref.var_name();

    if protected_targets.contains(lhs) {
        push(
            report,
            format!(
                "{method} equation assigns protected GALEC target `{}`",
                lhs.as_str()
            ),
        );
    }

    if !allowed_targets.contains(lhs) {
        push(
            report,
            format!(
                "{method} equation assigns `{}` outside the variables that method may update",
                lhs.as_str()
            ),
        );
    }

    if !assigned.insert(lhs.clone()) {
        push(
            report,
            format!(
                "{method} assigns `{}` more than once; GALEC lowering requires sorted single-assignment equations",
                lhs.as_str()
            ),
        );
    }

    check_supported_expr(&equation.rhs, report);
    Some(lhs)
}

fn check_supported_expr(expr: &Expression, report: &mut GalecAdmissibilityReport) {
    match expr {
        Expression::Literal { value, .. } => {
            if matches!(value, Literal::String(_)) {
                push(
                    report,
                    "string literals are not supported in GALEC expressions".to_string(),
                );
            }
        }
        Expression::VarRef { subscripts, .. } => {
            if !subscripts.is_empty() {
                push(
                    report,
                    "subscripted variable references require GALEC bounds analysis before lowering"
                        .to_string(),
                );
            }
        }
        Expression::Unary { op, rhs, .. } => {
            if !matches!(op, OpUnary::Minus | OpUnary::Not) {
                push(
                    report,
                    format!("unary operator `{op}` is not supported by the initial GALEC profile"),
                );
            }
            check_supported_expr(rhs, report);
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            if !matches!(
                op,
                OpBinary::Add
                    | OpBinary::Sub
                    | OpBinary::Mul
                    | OpBinary::Div
                    | OpBinary::Eq
                    | OpBinary::Neq
                    | OpBinary::Lt
                    | OpBinary::Le
                    | OpBinary::Gt
                    | OpBinary::Ge
                    | OpBinary::And
                    | OpBinary::Or
            ) {
                push(
                    report,
                    format!("binary operator `{op}` is not supported by the initial GALEC profile"),
                );
            }
            check_supported_expr(lhs, report);
            check_supported_expr(rhs, report);
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            if branches.is_empty() {
                push(
                    report,
                    "if expressions must have at least one branch for GALEC lowering".to_string(),
                );
            }

            for (condition, value) in branches {
                check_supported_expr(condition, report);
                check_supported_expr(value, report);
            }
            check_supported_expr(else_branch, report);
        }
        Expression::BuiltinCall { function, .. } if is_source_temporal_builtin(*function) => {
            push(
                report,
                format!("source temporal builtin `{function:?}` cannot survive into GALEC IR"),
            );
        }
        Expression::BuiltinCall { function, .. } => push(
            report,
            format!("builtin `{function:?}` is not supported by the initial GALEC profile"),
        ),
        other => push(
            report,
            format!(
                "expression form `{}` is not supported by the initial GALEC profile",
                expression_kind(other)
            ),
        ),
    }
}

#[derive(Default)]
struct RoleSets {
    startup_targets: HashSet<VarName>,
    step_targets: HashSet<VarName>,
    protected_targets: HashSet<VarName>,
    dependent_parameters: HashSet<VarName>,
    recalibrate_inputs: HashSet<VarName>,
}

impl RoleSets {
    fn new(dae: &dae::Dae) -> Self {
        let mut roles = Self::default();

        roles
            .protected_targets
            .extend(dae.variables.inputs.keys().cloned());
        roles
            .protected_targets
            .extend(tunable_parameters(dae).cloned());
        roles
            .protected_targets
            .extend(dae.variables.constants.keys().cloned());

        roles
            .startup_targets
            .extend(dae.variables.outputs.keys().cloned());
        roles
            .startup_targets
            .extend(dae.variables.discrete_reals.keys().cloned());
        roles
            .startup_targets
            .extend(dae.variables.discrete_valued.keys().cloned());
        roles
            .startup_targets
            .extend(dae.variables.algebraics.keys().cloned());
        roles
            .startup_targets
            .extend(dependent_parameters(dae).cloned());

        roles
            .step_targets
            .extend(dae.variables.outputs.keys().cloned());
        roles
            .step_targets
            .extend(dae.variables.discrete_reals.keys().cloned());
        roles
            .step_targets
            .extend(dae.variables.discrete_valued.keys().cloned());
        roles
            .step_targets
            .extend(dae.variables.algebraics.keys().cloned());

        roles
            .dependent_parameters
            .extend(dependent_parameters(dae).cloned());
        roles
            .recalibrate_inputs
            .extend(tunable_parameters(dae).cloned());
        roles
            .recalibrate_inputs
            .extend(dae.variables.constants.keys().cloned());
        roles
            .recalibrate_inputs
            .extend(roles.dependent_parameters.iter().cloned());

        roles
    }
}

fn tunable_parameters(dae: &dae::Dae) -> impl Iterator<Item = &VarName> {
    dae.variables
        .parameters
        .iter()
        .filter_map(|(name, variable)| {
            (variable.causality != dae::VariableCausality::CalculatedParameter).then_some(name)
        })
}

fn dependent_parameters(dae: &dae::Dae) -> impl Iterator<Item = &VarName> {
    dae.variables
        .parameters
        .iter()
        .filter_map(|(name, variable)| {
            (variable.causality == dae::VariableCausality::CalculatedParameter).then_some(name)
        })
}

fn all_variables(dae: &dae::Dae) -> impl Iterator<Item = &dae::Variable> {
    dae.variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.inputs.values())
        .chain(dae.variables.outputs.values())
        .chain(dae.variables.parameters.values())
        .chain(dae.variables.constants.values())
        .chain(dae.variables.discrete_reals.values())
        .chain(dae.variables.discrete_valued.values())
}

fn block_variables(dae: &dae::Dae) -> impl Iterator<Item = &dae::Variable> {
    dae.variables
        .states
        .values()
        .chain(dae.variables.inputs.values())
        .chain(dae.variables.outputs.values())
        .chain(dae.variables.parameters.values())
        .chain(dae.variables.constants.values())
        .chain(dae.variables.discrete_reals.values())
        .chain(dae.variables.discrete_valued.values())
}

fn collect_var_refs(expr: &Expression, refs: &mut HashSet<VarName>) {
    match expr {
        Expression::VarRef { name, .. } => {
            refs.insert(name.var_name().clone());
        }
        Expression::Unary { rhs, .. } => collect_var_refs(rhs, refs),
        Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs(lhs, refs);
            collect_var_refs(rhs, refs);
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_refs(arg, refs);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_var_refs(condition, refs);
                collect_var_refs(value, refs);
            }
            collect_var_refs(else_branch, refs);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_var_refs(element, refs);
            }
        }
        Expression::Range {
            start, step, end, ..
        } => {
            collect_var_refs(start, refs);
            if let Some(step) = step {
                collect_var_refs(step, refs);
            }
            collect_var_refs(end, refs);
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            collect_var_refs(expr, refs);
            if let Some(filter) = filter {
                collect_var_refs(filter, refs);
            }
        }
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            collect_var_refs(base, refs);
        }
        Expression::Literal { .. } | Expression::Empty { .. } => {}
    }
}

fn is_source_temporal_builtin(function: BuiltinFunction) -> bool {
    matches!(
        function,
        BuiltinFunction::Der
            | BuiltinFunction::Pre
            | BuiltinFunction::Edge
            | BuiltinFunction::Change
            | BuiltinFunction::Reinit
            | BuiltinFunction::Sample
            | BuiltinFunction::Initial
            | BuiltinFunction::Terminal
    )
}

#[derive(Debug, Clone, Copy)]
enum GalecLiteralValue {
    Real(f64),
    Integer(i64),
    Boolean,
}

fn literal_value(expr: Option<&Expression>) -> Option<GalecLiteralValue> {
    match expr? {
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(GalecLiteralValue::Real(*value)),
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(GalecLiteralValue::Integer(*value)),
        Expression::Literal {
            value: Literal::Boolean(_),
            ..
        } => Some(GalecLiteralValue::Boolean),
        _ => None,
    }
}

fn real_metadata(
    variable: &dae::Variable,
    attribute: &str,
    expr: Option<&Expression>,
    report: &mut GalecAdmissibilityReport,
) -> Option<f64> {
    match literal_value(expr) {
        Some(GalecLiteralValue::Real(value)) => Some(value),
        Some(GalecLiteralValue::Integer(value)) => Some(value as f64),
        Some(GalecLiteralValue::Boolean) => {
            push(
                report,
                format!(
                    "Real variable `{}` has non-numeric `{attribute}` metadata",
                    variable.name.as_str()
                ),
            );
            None
        }
        None => None,
    }
}

fn literal_real_value(expr: Option<&Expression>) -> Option<f64> {
    match literal_value(expr)? {
        GalecLiteralValue::Real(value) => Some(value),
        _ => None,
    }
}

fn unit_is_seconds(unit: Option<&str>) -> bool {
    matches!(unit, None | Some("s") | Some("second") | Some("seconds"))
}

fn floats_equal(lhs: f64, rhs: f64) -> bool {
    (lhs - rhs).abs() <= f64::EPSILON.max(rhs.abs() * f64::EPSILON)
}

fn expression_kind(expr: &Expression) -> &'static str {
    match expr {
        Expression::Binary { .. } => "binary",
        Expression::Unary { .. } => "unary",
        Expression::VarRef { .. } => "variable reference",
        Expression::BuiltinCall { .. } => "builtin call",
        Expression::FunctionCall { .. } => "function call",
        Expression::Literal { .. } => "literal",
        Expression::If { .. } => "if expression",
        Expression::Array { .. } => "array",
        Expression::Tuple { .. } => "tuple",
        Expression::Range { .. } => "range",
        Expression::ArrayComprehension { .. } => "array comprehension",
        Expression::Index { .. } => "index",
        Expression::FieldAccess { .. } => "field access",
        Expression::Empty { .. } => "empty expression",
    }
}

fn push(report: &mut GalecAdmissibilityReport, message: String) {
    report.violations.push(GalecViolation { message });
}

fn statement_kind(statement: &Statement) -> &'static str {
    match statement {
        Statement::Empty { .. } => "empty statement",
        Statement::Assignment { .. } => "assignment",
        Statement::Return { .. } => "return",
        Statement::Break { .. } => "break",
        Statement::For { .. } => "for statement",
        Statement::While { .. } => "while statement",
        Statement::If { .. } => "if statement",
        Statement::When { .. } => "when statement",
        Statement::FunctionCall { .. } => "function call",
        Statement::Reinit { .. } => "reinit",
        Statement::Assert { .. } => "assert",
    }
}

fn check_supported_stmt(statement: &Statement, report: &mut GalecAdmissibilityReport) {
    match statement {
        Statement::Assignment { value, .. } => {
            check_supported_expr(value, report);
        }

        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            if cond_blocks.is_empty() {
                push(
                    report,
                    "if statements must have at least one branch for GALEC lowering".to_string(),
                );
            }

            for block in cond_blocks {
                check_supported_expr(&block.cond, report);

                for statement in &block.stmts {
                    check_supported_stmt(statement, report);
                }
            }

            if let Some(else_block) = else_block {
                for statement in else_block {
                    check_supported_stmt(statement, report);
                }
            }
        }

        other => {
            push(
                report,
                format!(
                    "statement form `{}` is not supported by the initial GALEC profile",
                    statement_kind(other),
                ),
            );
        }
    }
}
