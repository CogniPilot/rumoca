use std::collections::{HashMap, HashSet};

use rumoca_core::{
    BuiltinFunction, ComponentReference, Expression, ExpressionRewriter, Function, Literal, OpBinary,
    OpUnary, Reference, Span, Statement, StatementBlock, VarName,
    component_reference_from_flat_name,
};
use rumoca_ir_dae as dae;

pub(crate) const SNAPSHOT_PREFIX: &str = "galec_previous_";
pub(crate) const SNAPSHOT_ORIGIN: &str = "GALEC previous-value snapshot";

#[derive(Debug, thiserror::Error)]
pub enum GalecPrepareError {
    #[error("cannot map previous-value slot `{0}` to a DAE variable")]
    MissingPreviousValueSource(String),
    #[error("structured pre() requires a variable reference, got `{0}`")]
    UnsupportedPreviousValue(String),
}

/// Convert solver-facing Appendix-B conventions into a DAE view suitable for
/// fixed-period GALEC Algorithm Code. The input is cloned so solver codegen and
/// simulation continue to consume the canonical DAE unchanged.
pub fn prepare_for_galec(source: &dae::Dae) -> Result<dae::Dae, GalecPrepareError> {
    let mut prepared = source.clone();
    let sample_condition = fixed_sample_condition(&prepared);

    for equation in prepared
        .discrete
        .real_updates
        .iter_mut()
        .chain(prepared.discrete.valued_updates.iter_mut())
    {
        if let Some((condition_name, condition_index)) = &sample_condition {
            equation.rhs =
                remove_fixed_sample_wrapper(&equation.rhs, condition_name, *condition_index);
        }
        equation.rhs = InitialRewriter { value: false }.rewrite_expression(&equation.rhs);
    }
    for equation in &mut prepared.initialization.equations {
        equation.rhs = InitialRewriter { value: true }.rewrite_expression(&equation.rhs);
    }

    let has_structured_step = prepare_structured_algorithms(&mut prepared)?;

        inline_function_calls(&mut prepared);
    optimize_wrap_patterns(&mut prepared);
    // All function calls have been inlined; remove definitions so the
    // admissibility gate does not reject them as unsupported user functions.
    prepared.symbols.functions.clear();


    remove_solver_condition_metadata(&mut prepared, sample_condition.as_ref());
    add_dependent_parameter_initializers(&mut prepared);
    add_discrete_startup_initializers(&mut prepared);
    if !has_structured_step {
        add_previous_value_snapshots(&mut prepared)?;
    }

    // Enum references have already been converted to integer ordinals by the
    // DAE phase. The symbol table is source provenance, not runtime behavior.
    prepared.symbols.enum_literal_ordinals.clear();
    Ok(prepared)
}

fn prepare_structured_algorithms(dae: &mut dae::Dae) -> Result<bool, GalecPrepareError> {
    let mut found_sampled_algorithm = false;
    let mut snapshot_definitions = Vec::new();

    for algorithm in &mut dae.algorithms.model {
        let mut runtime_statements = Vec::new();
        let mut found_sampled_branch = false;
        for statement in &algorithm.statements {
            match statement {
                Statement::When { blocks, .. } => {
                    for block in blocks {
                        if contains_sample_call(&block.cond) {
                            runtime_statements.extend(block.stmts.clone());
                            found_sampled_branch = true;
                        }
                    }
                }
                other => runtime_statements.push(other.clone()),
            }
        }
        if !found_sampled_branch {
            continue;
        }

        found_sampled_algorithm = true;
        let mut assigned = HashSet::new();
        let mut snapshots = StructuredSnapshotPlan::default();
        algorithm.statements =
            normalize_structured_statements(&runtime_statements, &mut assigned, &mut snapshots)?;
        if !snapshots.ordered.is_empty() {
            let mut prefix = snapshots
                .ordered
                .iter()
                .map(|(source, snapshot)| snapshot_assignment(source, snapshot))
                .collect::<Vec<_>>();
            prefix.append(&mut algorithm.statements);
            algorithm.statements = prefix;
            snapshot_definitions.extend(snapshots.ordered);
        }
    }

    for (source, snapshot) in snapshot_definitions {
        add_structured_snapshot_variable(dae, &source, snapshot)?;
    }
    Ok(found_sampled_algorithm)
}

fn contains_sample_call(expr: &Expression) -> bool {
    if is_sample_call(expr) {
        return true;
    }
    match expr {
        Expression::Unary { rhs, .. } => contains_sample_call(rhs),
        Expression::Binary { lhs, rhs, .. } => {
            contains_sample_call(lhs) || contains_sample_call(rhs)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_sample_call(condition) || contains_sample_call(value)
            }) || contains_sample_call(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_sample_call)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_sample_call)
        }
        _ => false,
    }
}

fn normalize_structured_statements(
    statements: &[Statement],
    assigned: &mut HashSet<VarName>,
    snapshots: &mut StructuredSnapshotPlan,
) -> Result<Vec<Statement>, GalecPrepareError> {
    statements
        .iter()
        .map(|statement| normalize_structured_statement(statement, assigned, snapshots))
        .collect()
}

fn normalize_structured_statement(
    statement: &Statement,
    assigned: &mut HashSet<VarName>,
    snapshots: &mut StructuredSnapshotPlan,
) -> Result<Statement, GalecPrepareError> {
    match statement {
        Statement::Assignment { comp, value, span } => {
            let value = rewrite_structured_expr(value, assigned, snapshots)?;
            assigned.insert(comp.to_var_name());
            Ok(Statement::Assignment {
                comp: comp.clone(),
                value,
                span: *span,
            })
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => {
            let mut assigned_after = assigned.clone();
            let mut normalized_blocks = Vec::with_capacity(cond_blocks.len());
            for block in cond_blocks {
                let condition = rewrite_structured_expr(&block.cond, assigned, snapshots)?;
                let mut branch_assigned = assigned.clone();
                let statements =
                    normalize_structured_statements(&block.stmts, &mut branch_assigned, snapshots)?;
                assigned_after.extend(branch_assigned);
                normalized_blocks.push(StatementBlock {
                    cond: condition,
                    stmts: statements,
                });
            }
            let normalized_else = match else_block {
                Some(statements) => {
                    let mut branch_assigned = assigned.clone();
                    let statements = normalize_structured_statements(
                        statements,
                        &mut branch_assigned,
                        snapshots,
                    )?;
                    assigned_after.extend(branch_assigned);
                    Some(statements)
                }
                None => None,
            };
            *assigned = assigned_after;
            Ok(Statement::If {
                cond_blocks: normalized_blocks,
                else_block: normalized_else,
                span: *span,
            })
        }
        Statement::For {
            indices,
            equations,
            span,
        } => {
            let mut loop_assigned = assigned.clone();
            let equations =
                normalize_structured_statements(equations, &mut loop_assigned, snapshots)?;
            assigned.extend(loop_assigned);
            Ok(Statement::For {
                indices: indices.clone(),
                equations,
                span: *span,
            })
        }
        other => Ok(other.clone()),
    }
}

fn rewrite_structured_expr(
    expr: &Expression,
    assigned: &HashSet<VarName>,
    snapshots: &mut StructuredSnapshotPlan,
) -> Result<Expression, GalecPrepareError> {
    let mut rewriter = StructuredPreRewriter {
        assigned,
        snapshots,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(rewritten),
    }
}

#[derive(Default)]
struct StructuredSnapshotPlan {
    by_source: HashMap<VarName, VarName>,
    ordered: Vec<(VarName, VarName)>,
}

impl StructuredSnapshotPlan {
    fn snapshot_for(&mut self, source: &VarName) -> VarName {
        if let Some(snapshot) = self.by_source.get(source) {
            return snapshot.clone();
        }
        let snapshot = VarName::new(format!(
            "{SNAPSHOT_PREFIX}{}",
            source.as_str().replace('.', "_")
        ));
        self.by_source.insert(source.clone(), snapshot.clone());
        self.ordered.push((source.clone(), snapshot.clone()));
        snapshot
    }
}

struct StructuredPreRewriter<'a> {
    assigned: &'a HashSet<VarName>,
    snapshots: &'a mut StructuredSnapshotPlan,
    error: Option<GalecPrepareError>,
}

impl StructuredPreRewriter<'_> {
    fn rewrite_pre(&mut self, args: &[Expression]) -> Expression {
        let Some(Expression::VarRef {
            name,
            subscripts,
            span,
        }) = args.first()
        else {
            self.error = Some(GalecPrepareError::UnsupportedPreviousValue(format!(
                "{args:?}"
            )));
            return args
                .first()
                .cloned()
                .unwrap_or(Expression::Empty { span: Span::DUMMY });
        };
        if !self.assigned.contains(name.var_name()) {
            return Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span: *span,
            };
        }
        let snapshot = self.snapshots.snapshot_for(name.var_name());
        Expression::VarRef {
            name: Reference::from_var_name(snapshot),
            subscripts: self.rewrite_subscripts(subscripts),
            span: *span,
        }
    }
}

impl ExpressionRewriter for StructuredPreRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args,
                ..
            } => self.rewrite_pre(args),
            Expression::FunctionCall { name, args, .. }
                if rumoca_core::source_temporal_function_short_name(name.as_str())
                    == Some("pre") =>
            {
                self.rewrite_pre(args)
            }
            Expression::BuiltinCall {
                function: BuiltinFunction::Initial,
                args,
                span,
            } if args.is_empty() => Expression::Literal {
                value: Literal::Boolean(false),
                span: *span,
            },
            _ => self.walk_expression(expr),
        }
    }
}

fn snapshot_assignment(source: &VarName, snapshot: &VarName) -> Statement {
    Statement::Assignment {
        comp: component_reference(snapshot),
        value: Expression::VarRef {
            name: Reference::from_var_name(source.clone()),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    }
}

fn component_reference(name: &VarName) -> ComponentReference {
    component_reference_from_flat_name(name, Span::DUMMY)
        .expect("generated GALEC snapshot name must be a valid component reference")
}

fn add_structured_snapshot_variable(
    dae: &mut dae::Dae,
    source: &VarName,
    snapshot: VarName,
) -> Result<(), GalecPrepareError> {
    let source_variable = find_variable(dae, source).cloned().ok_or_else(|| {
        GalecPrepareError::MissingPreviousValueSource(source.as_str().to_string())
    })?;
    let mut variable = source_variable.clone();
    variable.name = snapshot.clone();
    variable.component_ref = None;
    variable.causality = dae::VariableCausality::Local;
    variable.origin = dae::VariableOrigin::Generated;
    variable.description = Some(format!("previous value of {}", source.as_str()));
    if is_discrete_valued(&source_variable) {
        dae.variables.discrete_valued.insert(snapshot, variable);
    } else {
        dae.variables.discrete_reals.insert(snapshot, variable);
    }
    Ok(())
}

fn fixed_sample_condition(dae: &dae::Dae) -> Option<(VarName, i64)> {
    dae.conditions
        .equations
        .iter()
        .enumerate()
        .find(|(_, equation)| is_sample_call(&equation.rhs))
        .and_then(|(index, equation)| {
            equation.lhs.as_ref().and_then(|lhs| {
                dae::component_base_name(lhs.as_str())
                    .map(|base| (VarName::new(base), index as i64 + 1))
            })
        })
}

fn is_sample_call(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            ..
        }
    ) || matches!(
        expr,
        Expression::FunctionCall { name, .. } if name.as_str() == "__rumoca_sample"
    )
}

fn remove_fixed_sample_wrapper(
    expr: &Expression,
    condition_name: &VarName,
    condition_index: i64,
) -> Expression {
    let Expression::If { branches, .. } = expr else {
        return expr.clone();
    };
    let Some((condition, value)) = branches.first() else {
        return expr.clone();
    };
    if is_fixed_sample_activation(condition, condition_name, condition_index) {
        value.clone()
    } else {
        expr.clone()
    }
}

fn is_fixed_sample_activation(
    expr: &Expression,
    condition_name: &VarName,
    condition_index: i64,
) -> bool {
    if is_fixed_sample_edge(expr, condition_name, condition_index) {
        return true;
    }
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            (is_initial_call(lhs) && is_fixed_sample_edge(rhs, condition_name, condition_index))
                || (is_initial_call(rhs)
                    && is_fixed_sample_edge(lhs, condition_name, condition_index))
        }
        _ => false,
    }
}

fn is_initial_call(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args,
            ..
        } if args.is_empty()
    )
}

fn is_fixed_sample_edge(expr: &Expression, condition_name: &VarName, condition_index: i64) -> bool {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            (name.var_name() == condition_name
                || name.as_str() == format!("__pre__.{}", condition_name.as_str()))
                && subscripts.iter().any(|subscript| {
                    matches!(subscript, rumoca_core::Subscript::Index { value, .. } if *value == condition_index)
                })
        }
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => {
            is_fixed_sample_edge(rhs, condition_name, condition_index)
        }
        Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            is_fixed_sample_edge(lhs, condition_name, condition_index)
                && is_fixed_sample_edge(rhs, condition_name, condition_index)
        }
        _ => false,
    }
}

fn remove_solver_condition_metadata(dae: &mut dae::Dae, sample_condition: Option<&(VarName, i64)>) {
    if let Some((condition_name, _)) = sample_condition {
        dae.variables.discrete_valued.shift_remove(condition_name);
        dae.variables.parameters.shift_remove(&VarName::new(format!(
            "__pre__.{}",
            condition_name.as_str()
        )));
    }
    dae.conditions.equations.clear();
    dae.conditions.relations.clear();
}

fn add_dependent_parameter_initializers(dae: &mut dae::Dae) {
    let assigned = dae
        .initialization
        .equations
        .iter()
        .filter_map(|equation| equation.lhs.as_ref().map(|lhs| lhs.var_name().clone()))
        .collect::<HashSet<_>>();
    let additions = dae
        .variables
        .parameters
        .iter()
        .filter(|(name, variable)| {
            variable.causality == dae::VariableCausality::CalculatedParameter
                && !name.as_str().starts_with("__pre__.")
                && !assigned.contains(*name)
        })
        .filter_map(|(name, variable)| {
            variable.start.clone().map(|start| {
                dae::Equation::explicit(
                    Reference::generated(name.as_str()),
                    start,
                    variable.source_span,
                    "GALEC dependent-parameter initializer",
                )
            })
        })
        .collect::<Vec<_>>();
    dae.initialization.equations.extend(additions);
}

fn add_discrete_startup_initializers(dae: &mut dae::Dae) {
    let mut assigned = dae
        .initialization
        .equations
        .iter()
        .filter_map(|equation| equation.lhs.as_ref().map(|lhs| lhs.var_name().clone()))
        .collect::<HashSet<_>>();
    let additions: Vec<dae::Equation> = dae
        .variables
        .outputs
        .iter()
        .chain(dae.variables.discrete_reals.iter())
        .chain(dae.variables.discrete_valued.iter())
        .filter(|(name, _)| !name.as_str().starts_with(SNAPSHOT_PREFIX))
        .filter_map(|(name, variable)| {
            if !assigned.insert(name.clone()) {
                return None;
            }
            variable.start.clone().map(|start| {
                dae::Equation::explicit(
                    Reference::generated(name.as_str()),
                    start,
                    variable.source_span,
                    "GALEC discrete startup initializer",
                )
            })
        })
        .collect();

    let param_additions: Vec<dae::Equation> = dae
        .variables
        .parameters
        .iter()
        .filter(|(name, variable)| {
            variable.causality != dae::VariableCausality::CalculatedParameter
                && !name.as_str().starts_with("__pre__.")
        })
        .filter_map(|(name, variable)| {
            if !assigned.insert(name.clone()) {
                return None;
            }
            variable.start.clone().map(|start| {
                dae::Equation::explicit(
                    Reference::generated(name.as_str()),
                    start,
                    variable.source_span,
                    "GALEC parameter startup initializer",
                )
            })
        })
        .collect();

    dae.initialization.equations.extend(additions);
    dae.initialization.equations.extend(param_additions);
}

fn add_previous_value_snapshots(dae: &mut dae::Dae) -> Result<(), GalecPrepareError> {
    let mut previous_names = Vec::new();
    let mut seen_previous_names = HashSet::new();
    for equation in dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
    {
        let mut refs = Vec::new();
        equation.rhs.collect_var_refs(&mut refs);
        for name in refs
            .into_iter()
            .filter(|name| name.as_str().starts_with("__pre__."))
        {
            if seen_previous_names.insert(name.clone()) {
                previous_names.push(name);
            }
        }
    }

    let mut replacements = HashMap::new();
    let mut real_snapshots = Vec::new();
    let mut valued_snapshots = Vec::new();
    for previous_name in previous_names {
        let source_name = VarName::new(previous_name.as_str().trim_start_matches("__pre__."));
        let source_variable = find_variable(dae, &source_name).cloned().ok_or_else(|| {
            GalecPrepareError::MissingPreviousValueSource(previous_name.as_str().to_string())
        })?;
        let snapshot_name = VarName::new(format!(
            "{SNAPSHOT_PREFIX}{}",
            source_name.as_str().replace('.', "_")
        ));
        replacements.insert(previous_name, snapshot_name.clone());

        let mut snapshot_variable = source_variable.clone();
        snapshot_variable.name = snapshot_name.clone();
        snapshot_variable.component_ref = None;
        snapshot_variable.causality = dae::VariableCausality::Local;
        snapshot_variable.origin = dae::VariableOrigin::Generated;
        snapshot_variable.description = Some(format!("previous value of {}", source_name.as_str()));
        let equation = dae::Equation::explicit(
            Reference::generated(snapshot_name.as_str()),
            Expression::VarRef {
                name: Reference::generated(source_name.as_str()),
                subscripts: Vec::new(),
                span: Span::DUMMY,
            },
            Span::DUMMY,
            SNAPSHOT_ORIGIN,
        );

        if is_discrete_valued(&source_variable) {
            dae.variables
                .discrete_valued
                .insert(snapshot_name, snapshot_variable);
            valued_snapshots.push(equation);
        } else {
            dae.variables
                .discrete_reals
                .insert(snapshot_name, snapshot_variable);
            real_snapshots.push(equation);
        }
    }

    let mut rewriter = PreviousValueRewriter { replacements };
    for equation in dae
        .discrete
        .real_updates
        .iter_mut()
        .chain(dae.discrete.valued_updates.iter_mut())
    {
        equation.rhs = rewriter.rewrite_expression(&equation.rhs);
    }
    real_snapshots.append(&mut dae.discrete.real_updates);
    valued_snapshots.append(&mut dae.discrete.valued_updates);
    dae.discrete.real_updates = real_snapshots;
    dae.discrete.valued_updates = valued_snapshots;
    dae.variables
        .parameters
        .retain(|name, _| !name.as_str().starts_with("__pre__."));
    Ok(())
}

fn find_variable<'a>(dae: &'a dae::Dae, name: &VarName) -> Option<&'a dae::Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.inputs.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.parameters.get(name))
        .or_else(|| dae.variables.constants.get(name))
        .or_else(|| dae.variables.discrete_reals.get(name))
        .or_else(|| dae.variables.discrete_valued.get(name))
}

fn is_discrete_valued(variable: &dae::Variable) -> bool {
    matches!(
        variable.start,
        Some(Expression::Literal {
            value: Literal::Boolean(_) | Literal::Integer(_),
            ..
        })
    )
}

struct InitialRewriter {
    value: bool,
}

impl ExpressionRewriter for InitialRewriter {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Initial,
                args,
                span,
            } if args.is_empty() => Expression::Literal {
                value: Literal::Boolean(self.value),
                span: *span,
            },
            Expression::If {
                branches,
                else_branch,
                ..
            } if branches.first().is_some_and(|(condition, _)| {
                matches!(condition, Expression::BuiltinCall { function: BuiltinFunction::Initial, args, .. } if args.is_empty())
            }) => {
                if self.value {
                    self.rewrite_expression(&branches[0].1)
                } else {
                    self.rewrite_expression(else_branch)
                }
            }
            _ => self.walk_expression(expr),
        }
    }
}

struct PreviousValueRewriter {
    replacements: HashMap<VarName, VarName>,
}

impl ExpressionRewriter for PreviousValueRewriter {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: Span,
    ) -> Expression {
        let replacement = self.replacements.get(name.var_name());
        Expression::VarRef {
            name: replacement
                .cloned()
                .map(Reference::from_var_name)
                .unwrap_or_else(|| name.clone()),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

fn inline_function_calls(dae: &mut dae::Dae) {
    // Build a lookup of single-expression functions we can inline generically.
    let mut inline_map: HashMap<String, (Vec<String>, Expression)> = HashMap::new();
    for (name, func) in &dae.symbols.functions {
        if let Some((param_names, body_rhs)) = extract_inline_body(func) {
            inline_map.insert(name.as_str().to_string(), (param_names, body_rhs));
        }
    }

    let mut inliner = FunctionInliner {
        functions: &inline_map,
    };
    for equation in &mut dae.initialization.equations {
        equation.rhs = inliner.rewrite_expression(&equation.rhs);
    }
    for equation in dae
        .discrete
        .real_updates
        .iter_mut()
        .chain(dae.discrete.valued_updates.iter_mut())
    {
        equation.rhs = inliner.rewrite_expression(&equation.rhs);
    }
    for algorithm in &mut dae.algorithms.model {
        for statement in &mut algorithm.statements {
            *statement = rewrite_statement_expressions(statement, &mut inliner);
        }
    }
}

/// Peephole pass that recognizes `atan2(sin(x), cos(x))` (the generic inlining
/// result of a `wrapPi` body) and rewrites it to the cheaper if-chain:
/// `if x > pi then x - 2*pi elseif x <= -pi then x + 2*pi else x`.
fn optimize_wrap_patterns(dae: &mut dae::Dae) {
    let mut optimizer = WrapPiOptimizer;
    for equation in &mut dae.initialization.equations {
        equation.rhs = optimizer.rewrite_expression(&equation.rhs);
    }
    for equation in dae
        .discrete
        .real_updates
        .iter_mut()
        .chain(dae.discrete.valued_updates.iter_mut())
    {
        equation.rhs = optimizer.rewrite_expression(&equation.rhs);
    }
    for algorithm in &mut dae.algorithms.model {
        for statement in &mut algorithm.statements {
            *statement = rewrite_statement_expressions(statement, &mut optimizer);
        }
    }
}

struct WrapPiOptimizer;

impl ExpressionRewriter for WrapPiOptimizer {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Atan2,
            args,
            span,
        } = expr
        {
            if args.len() == 2 {
                if let (
                    Expression::BuiltinCall {
                        function: BuiltinFunction::Sin,
                        args: sin_args,
                        ..
                    },
                    Expression::BuiltinCall {
                        function: BuiltinFunction::Cos,
                        args: cos_args,
                        ..
                    },
                ) = (&args[0], &args[1])
                {
                    if sin_args.len() == 1
                        && cos_args.len() == 1
                        && sin_args[0] == cos_args[0]
                    {
                        let x = Box::new(self.rewrite_expression(&sin_args[0]));
                        let pi_lit = |span: Span| Expression::Literal {
                            value: Literal::Real(std::f64::consts::PI),
                            span,
                        };
                        let two_pi = Expression::Binary {
                            op: OpBinary::Mul,
                            lhs: Box::new(Expression::Literal {
                                value: Literal::Real(2.0),
                                span: *span,
                            }),
                            rhs: Box::new(pi_lit(*span)),
                            span: *span,
                        };
                        let gt_pi = Expression::Binary {
                            op: OpBinary::Gt,
                            lhs: x.clone(),
                            rhs: Box::new(pi_lit(*span)),
                            span: *span,
                        };
                        let angle_minus_2pi = Expression::Binary {
                            op: OpBinary::Sub,
                            lhs: x.clone(),
                            rhs: Box::new(two_pi.clone()),
                            span: *span,
                        };
                        let neg_pi = Expression::Unary {
                            op: OpUnary::Minus,
                            rhs: Box::new(pi_lit(*span)),
                            span: *span,
                        };
                        let le_neg_pi = Expression::Binary {
                            op: OpBinary::Le,
                            lhs: x.clone(),
                            rhs: Box::new(neg_pi),
                            span: *span,
                        };
                        let angle_plus_2pi = Expression::Binary {
                            op: OpBinary::Add,
                            lhs: x.clone(),
                            rhs: Box::new(two_pi),
                            span: *span,
                        };
                        return Expression::If {
                            branches: vec![(gt_pi, angle_minus_2pi), (le_neg_pi, angle_plus_2pi)],
                            else_branch: x,
                            span: *span,
                        };
                    }
                }
            }
        }
        self.walk_expression(expr)
    }
}


/// its body that computes its (sole) output variable.
fn extract_inline_body(func: &Function) -> Option<(Vec<String>, Expression)> {
    let n_outputs = func.outputs.len();
    if n_outputs != 1 {
        return None;
    }
    let output_name = func.outputs[0].name.as_str();
    let mut iter = func.body.iter();
    let stmt = iter.next()?;
    if iter.next().is_some() {
        return None; // more than one statement
    }
    match stmt {
        Statement::Assignment { comp, value, .. } if comp.to_var_name().as_str() == output_name => {
            let param_names: Vec<String> = func
                .inputs
                .iter()
                .map(|p| p.name.as_str().to_string())
                .collect();
            Some((param_names, value.clone()))
        }
        _ => None,
    }
}

fn rewrite_statement_expressions(
    statement: &Statement,
    inliner: &mut impl ExpressionRewriter,
) -> Statement {
    match statement {
        Statement::Assignment { comp, value, span } => Statement::Assignment {
            comp: comp.clone(),
            value: inliner.rewrite_expression(value),
            span: *span,
        },
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => Statement::If {
            cond_blocks: cond_blocks
                .iter()
                .map(|block| StatementBlock {
                    cond: inliner.rewrite_expression(&block.cond),
                    stmts: block
                        .stmts
                        .iter()
                        .map(|s| rewrite_statement_expressions(s, inliner))
                        .collect(),
                })
                .collect(),
            else_block: else_block.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|s| rewrite_statement_expressions(s, inliner))
                    .collect()
            }),
            span: *span,
        },
        other => other.clone(),
    }
}

struct FunctionInliner<'a> {
    functions: &'a HashMap<String, (Vec<String>, Expression)>,
}

impl ExpressionRewriter for FunctionInliner<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::FunctionCall { name, args, .. } => {
                let name_str = name.as_str();
                if let Some((param_names, body)) = self.functions.get(name_str) {
                    if param_names.len() == args.len() {
                        // First recursively inline any function calls in the argument
                        // expressions, then substitute parameter references.
                        let mut subst: HashMap<String, Expression> = HashMap::new();
                        for (param_name, arg) in param_names.iter().zip(args.iter()) {
                            subst.insert(param_name.clone(), self.rewrite_expression(arg));
                        }
                        let mut substitutor = ParamSubstitutor { subst };
                        let mut inlined = substitutor.rewrite_expression(body);
                        // The result may itself contain function calls — keep inlining.
                        inlined = self.rewrite_expression(&inlined);
                        return inlined;
                    }
                }
                self.walk_expression(expr)
            }
            _ => self.walk_expression(expr),
        }
    }
}

struct ParamSubstitutor {
    subst: HashMap<String, Expression>,
}

impl ExpressionRewriter for ParamSubstitutor {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef { name, subscripts, span }
                if subscripts.is_empty() =>
            {
                if let Some(replacement) = self.subst.get(name.as_str()) {
                    return replacement.clone();
                }
                Expression::VarRef {
                    name: name.clone(),
                    subscripts: subscripts.clone(),
                    span: *span,
                }
            }
            _ => self.walk_expression(expr),
        }
    }
}
