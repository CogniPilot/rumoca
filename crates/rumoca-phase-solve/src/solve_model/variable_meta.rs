use super::*;

pub(super) fn continuous_real_role_is_event_discontinuous(
    role: &str,
    scalar_name: &str,
    event_discontinuous_names: &IndexSet<String>,
) -> bool {
    matches!(role, "algebraic" | "output" | "input")
        && event_discontinuous_names.contains(scalar_name)
}

pub(super) fn event_discontinuous_scalar_names(
    dae_model: &dae::Dae,
) -> Result<IndexSet<String>, SolveModelLowerError> {
    let event_discrete_names = event_discrete_scalar_names(dae_model)?;
    let event_discrete_bases = base_name_index(&event_discrete_names);
    let mut definitions = continuous_definition_expressions(dae_model)?;
    let mut event_discontinuous = IndexSet::new();
    let mut event_discontinuous_bases: IndexMap<String, Vec<String>> = IndexMap::new();
    loop {
        let before = event_discontinuous.len();
        let mut found = Vec::new();
        for (scalar_name, exprs) in &definitions {
            if event_discontinuous.contains(scalar_name) {
                continue;
            }
            if exprs.iter().any(|expr| {
                expression_is_event_discontinuous(
                    expr,
                    ScalarNameLookup {
                        names: &event_discrete_names,
                        base_index: &event_discrete_bases,
                    },
                    ScalarNameLookup {
                        names: &event_discontinuous,
                        base_index: &event_discontinuous_bases,
                    },
                )
            }) {
                found.push(scalar_name.clone());
            }
        }
        for scalar_name in found {
            if let Some(base) = dae::component_base_name(&scalar_name) {
                event_discontinuous_bases
                    .entry(base)
                    .or_default()
                    .push(scalar_name.clone());
            }
            event_discontinuous.insert(scalar_name);
        }
        if event_discontinuous.len() == before {
            return Ok(event_discontinuous);
        }
        definitions.retain(|name, _| !event_discontinuous.contains(name));
    }
}

fn event_discrete_scalar_names(
    dae_model: &dae::Dae,
) -> Result<IndexSet<String>, SolveModelLowerError> {
    let mut names = IndexSet::new();
    for (name, var) in dae_model
        .variables
        .discrete_reals
        .iter()
        .chain(dae_model.variables.discrete_valued.iter())
    {
        names.extend(scalar_names(name.as_str(), var)?);
    }
    Ok(names)
}

fn continuous_definition_expressions(
    dae_model: &dae::Dae,
) -> Result<IndexMap<String, Vec<&rumoca_core::Expression>>, SolveModelLowerError> {
    let mut continuous_vars = IndexMap::new();
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .chain(dae_model.variables.inputs.iter())
    {
        continuous_vars.insert(name.clone(), scalar_names(name.as_str(), var)?);
    }
    let continuous_names = continuous_vars
        .values()
        .flat_map(|names| names.iter().cloned())
        .collect::<IndexSet<_>>();
    let continuous_base_index = base_name_index(&continuous_names);
    let continuous_lookup = ScalarNameLookup {
        names: &continuous_names,
        base_index: &continuous_base_index,
    };
    let mut definitions = IndexMap::new();
    for eq in &dae_model.continuous.equations {
        if let Some(lhs) = eq.lhs.as_ref() {
            add_continuous_lhs_definitions(
                &mut definitions,
                &continuous_vars,
                lhs.var_name(),
                &eq.rhs,
            );
            continue;
        }
        collect_residual_continuous_definitions(&eq.rhs, continuous_lookup, &mut definitions);
    }
    Ok(definitions)
}

fn add_continuous_lhs_definitions<'m>(
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
    continuous_vars: &IndexMap<rumoca_core::VarName, Vec<String>>,
    lhs: &rumoca_core::VarName,
    rhs: &'m rumoca_core::Expression,
) {
    let Some(scalars) = continuous_vars.get(lhs) else {
        return;
    };
    for scalar_name in scalars {
        add_continuous_definition(definitions, scalar_name, rhs);
    }
}

fn add_continuous_definition<'m>(
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
    scalar_name: &str,
    expr: &'m rumoca_core::Expression,
) {
    definitions
        .entry(scalar_name.to_string())
        .or_default()
        .push(expr);
}

fn collect_residual_continuous_definitions<'m>(
    expr: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => collect_sub_residual_definitions(lhs, rhs, continuous_names, definitions),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => collect_if_residual_definitions(branches, else_branch, continuous_names, definitions),
        _ => IndexSet::new(),
    }
}

fn collect_sub_residual_definitions<'m>(
    lhs: &'m rumoca_core::Expression,
    rhs: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    let mut targets = IndexSet::new();
    for scalar_name in continuous_ref_scalar_names(lhs, continuous_names) {
        add_continuous_definition(definitions, &scalar_name, rhs);
        targets.insert(scalar_name);
    }
    for scalar_name in continuous_ref_scalar_names(rhs, continuous_names) {
        add_continuous_definition(definitions, &scalar_name, lhs);
        targets.insert(scalar_name);
    }
    targets
}

fn collect_if_residual_definitions<'m>(
    branches: &'m [(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    let mut guards = Vec::new();
    let mut targets = IndexSet::new();
    for (condition, branch_expr) in branches {
        guards.push(condition);
        targets.extend(collect_residual_continuous_definitions(
            branch_expr,
            continuous_names,
            definitions,
        ));
    }
    targets.extend(collect_residual_continuous_definitions(
        else_branch,
        continuous_names,
        definitions,
    ));
    for scalar_name in &targets {
        for guard in &guards {
            add_continuous_definition(definitions, scalar_name, guard);
        }
    }
    targets
}

fn continuous_ref_scalar_names(
    expr: &rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
) -> Vec<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return Vec::new();
    };
    event_dependency_ref_names(name, subscripts)
        .into_iter()
        .flat_map(|candidate| continuous_names.matching_scalar_names(&candidate))
        .collect()
}

/// Scalar-name membership with subscript-stripped base names indexed once,
/// so per-reference candidate checks avoid re-parsing every set entry.
#[derive(Clone, Copy)]
struct ScalarNameLookup<'a> {
    names: &'a IndexSet<String>,
    base_index: &'a IndexMap<String, Vec<String>>,
}

fn base_name_index(names: &IndexSet<String>) -> IndexMap<String, Vec<String>> {
    let mut index: IndexMap<String, Vec<String>> = IndexMap::new();
    for name in names {
        if let Some(base) = dae::component_base_name(name) {
            index.entry(base).or_default().push(name.clone());
        }
    }
    index
}

impl ScalarNameLookup<'_> {
    fn contains(&self, candidate: &str) -> bool {
        self.names.contains(candidate) || self.base_index.contains_key(candidate)
    }

    fn matching_scalar_names(&self, candidate: &str) -> Vec<String> {
        if self.names.contains(candidate) {
            return vec![candidate.to_string()];
        }
        match self.base_index.get(candidate) {
            Some(names) => names.clone(),
            None => Vec::new(),
        }
    }
}

fn expression_is_event_discontinuous(
    expr: &rumoca_core::Expression,
    event_discrete_names: ScalarNameLookup<'_>,
    event_discontinuous_names: ScalarNameLookup<'_>,
) -> bool {
    let mut checker = EventDiscontinuityChecker {
        event_discrete_names,
        event_discontinuous_names,
        found: false,
        no_event_depth: 0,
    };
    checker.visit_expression(expr);
    checker.found
}

struct EventDiscontinuityChecker<'a> {
    event_discrete_names: ScalarNameLookup<'a>,
    event_discontinuous_names: ScalarNameLookup<'a>,
    found: bool,
    no_event_depth: usize,
}

impl ExpressionVisitor for EventDiscontinuityChecker<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        let names = event_dependency_ref_names(name, subscripts);
        if names.iter().any(|name| {
            self.event_discrete_names.contains(name)
                || self.event_discontinuous_names.contains(name)
        }) {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::NoEvent {
            self.no_event_depth += 1;
            for arg in args {
                self.visit_expression(arg);
            }
            self.no_event_depth -= 1;
            return;
        }
        if self.no_event_depth == 0
            && matches!(
                function,
                rumoca_core::BuiltinFunction::Div
                    | rumoca_core::BuiltinFunction::Mod
                    | rumoca_core::BuiltinFunction::Rem
                    | rumoca_core::BuiltinFunction::Ceil
                    | rumoca_core::BuiltinFunction::Floor
                    | rumoca_core::BuiltinFunction::Integer
                    | rumoca_core::BuiltinFunction::Delay
            )
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_binary(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if self.no_event_depth == 0
            && matches!(
                op,
                rumoca_core::OpBinary::Ge
                    | rumoca_core::OpBinary::Gt
                    | rumoca_core::OpBinary::Le
                    | rumoca_core::OpBinary::Lt
            )
        {
            self.found = true;
            return;
        }
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }
}

fn event_dependency_ref_names(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Vec<String> {
    let base = name
        .as_str()
        .strip_prefix("__pre__.")
        .unwrap_or_else(|| name.as_str())
        .to_string();
    let mut names = vec![base.clone()];
    if !subscripts.is_empty()
        && let Ok(Some(indices)) = crate::checked_literal_positive_indices(subscripts, name.span())
    {
        names.push(dae::format_subscript_key(&base, &indices));
    }
    names
}

pub(super) fn variable_meta_classification(
    role: &str,
    is_state: bool,
) -> (Option<String>, Option<String>, Option<String>) {
    if is_state {
        return (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        );
    }

    match role {
        "algebraic" | "output" | "input" => (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        ),
        "discrete-real" => (
            Some("Real".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        "discrete-valued" => (
            Some("Boolean/Integer/Enum".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        _ => (None, None, None),
    }
}
