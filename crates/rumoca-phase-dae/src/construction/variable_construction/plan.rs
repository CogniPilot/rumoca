use super::*;
use rumoca_core::{ExpressionScope, ExpressionVisitor};

#[cfg(test)]
mod tests;

pub(in crate::construction) struct VariableConstructionPlan {
    variables: Box<[VariablePlan]>,
    /// Deterministic finalization order. Ordinary expressions need only issued
    /// coordinate identity; this order does not claim that referenced
    /// variables are already defined. Recursive calculated parameters are
    /// rejected separately because their bindings require computability.
    definition_components: Box<[rumoca_core::DependencyScc]>,
}

impl VariableConstructionPlan {
    pub(super) fn variable(&self, source_ordinal: usize) -> &VariablePlan {
        &self.variables[source_ordinal]
    }

    pub(super) fn definition_components(&self) -> &[rumoca_core::DependencyScc] {
        &self.definition_components
    }
}

pub(super) struct VariablePlan {
    dependencies: Box<[DependencyUse]>,
    binding_dependencies: Box<[DependencyUse]>,
    requires_function_ids: bool,
}

impl VariablePlan {
    /// A complete insertion is locally valid once every referenced coordinate
    /// identity has been issued. Definitions of those coordinates may still
    /// be open under their own linear forward-attribute capabilities.
    pub(super) fn requires_reservation(&self, source_ordinal: usize) -> bool {
        self.requires_function_ids
            || self
                .dependencies
                .iter()
                .any(|dependency| dependency.target >= source_ordinal)
    }
}

#[derive(Clone, Copy)]
struct DependencyUse {
    target: usize,
    span: Span,
}

pub(in crate::construction) fn plan_variable_construction(
    flat: &flat::Model,
    analysis: &Analysis,
) -> Result<VariableConstructionPlan, ToDaeError> {
    let source_ordinals = flat
        .variables
        .keys()
        .enumerate()
        .filter(|(_, name)| constructed_role(analysis.roles[*name]))
        .map(|(ordinal, name)| (name.clone(), ordinal))
        .collect::<HashMap<_, _>>();
    let mut variables = Vec::with_capacity(flat.variables.len());
    for (name, variable) in &flat.variables {
        let role = analysis.roles[name];
        variables.push(plan_variable(
            variable,
            role,
            analysis.derived_parameters.get(name),
            &source_ordinals,
        ));
    }
    let dependency_graph = variables
        .iter()
        .map(|variable| {
            variable
                .dependencies
                .iter()
                .map(|dependency| dependency.target)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let components = rumoca_core::dependency_first_sccs(&dependency_graph)
        .expect("variable dependencies use Flat source ordinals");
    let binding_dependency_graph = variables
        .iter()
        .map(|variable| {
            variable
                .binding_dependencies
                .iter()
                .map(|dependency| dependency.target)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let binding_components = rumoca_core::dependency_first_sccs(&binding_dependency_graph)
        .expect("variable binding dependencies use Flat source ordinals");
    let calculated_parameters = analysis
        .derived_parameters
        .keys()
        .cloned()
        .collect::<HashSet<_>>();
    reject_recursive_calculated_parameters(
        flat,
        &calculated_parameters,
        &variables,
        &binding_components,
    )?;
    Ok(VariableConstructionPlan {
        variables: variables.into_boxed_slice(),
        definition_components: components.into_boxed_slice(),
    })
}

fn plan_variable(
    variable: &flat::Variable,
    role: PlannedRole,
    derived: Option<&DerivedParameterPlan>,
    source_ordinals: &HashMap<VarName, usize>,
) -> VariablePlan {
    if !constructed_role(role) {
        return VariablePlan {
            dependencies: Box::new([]),
            binding_dependencies: Box::new([]),
            requires_function_ids: false,
        };
    }
    let mut attributes = AttributeDependencyCollector::new(source_ordinals);
    for expression in [
        variable.start.as_ref(),
        variable.min.as_ref(),
        variable.max.as_ref(),
        variable.nominal.as_ref(),
    ]
    .into_iter()
    .flatten()
    {
        attributes.visit_expression(expression);
    }
    let mut binding_collector = AttributeDependencyCollector::new(source_ordinals);
    if let Some(derived) = derived {
        binding_collector.with_shadowed(
            derived
                .domain
                .binders
                .iter()
                .map(|binder| VarName::new(&binder.display_name)),
            |collector| collector.visit_expression(&derived.body),
        );
    } else if matches!(
        role,
        PlannedRole::Parameter | PlannedRole::Constant | PlannedRole::Input
    ) && let Some(binding) = &variable.binding
    {
        binding_collector.visit_expression(binding);
    }
    let requires_function_ids =
        attributes.requires_function_ids || binding_collector.requires_function_ids;
    let binding_dependencies = binding_collector.dependencies.into_boxed_slice();
    let dependencies = merge_dependencies(attributes.dependencies, &binding_dependencies);
    VariablePlan {
        dependencies,
        binding_dependencies,
        requires_function_ids,
    }
}

fn merge_dependencies(
    mut attributes: Vec<DependencyUse>,
    binding: &[DependencyUse],
) -> Box<[DependencyUse]> {
    let mut seen = attributes
        .iter()
        .map(|dependency| dependency.target)
        .collect::<HashSet<_>>();
    attributes.extend(
        binding
            .iter()
            .copied()
            .filter(|dependency| seen.insert(dependency.target)),
    );
    attributes.into_boxed_slice()
}

fn constructed_role(role: PlannedRole) -> bool {
    !matches!(
        role,
        PlannedRole::Clock | PlannedRole::EnumerationLiteral | PlannedRole::Aggregate
    )
}

fn reject_recursive_calculated_parameters(
    flat: &flat::Model,
    calculated_parameters: &HashSet<VarName>,
    variables: &[VariablePlan],
    components: &[rumoca_core::DependencyScc],
) -> Result<(), ToDaeError> {
    for component in components.iter().filter(|component| component.recursive) {
        let has_calculated_parameter = component.members.iter().any(|&ordinal| {
            flat.variables
                .get_index(ordinal)
                .is_some_and(|(name, _)| calculated_parameters.contains(name))
        });
        if !has_calculated_parameter {
            continue;
        }
        let (owner, dependency, span) =
            first_internal_binding_dependency(flat, variables, &component.members);
        return Err(ToDaeError::unsupported_flat(
            "calculated parameter dependency",
            format!(
                "`{owner}` depends recursively on `{dependency}`; calculated parameter bindings require an acyclic dependency proof"
            ),
            span,
        ));
    }
    Ok(())
}

fn first_internal_binding_dependency(
    flat: &flat::Model,
    variables: &[VariablePlan],
    members: &[usize],
) -> (VarName, VarName, Span) {
    for &owner in members {
        if let Some(dependency) = variables[owner]
            .binding_dependencies
            .iter()
            .find(|dependency| members.binary_search(&dependency.target).is_ok())
        {
            return (
                flat.variables.get_index(owner).unwrap().0.clone(),
                flat.variables
                    .get_index(dependency.target)
                    .unwrap()
                    .0
                    .clone(),
                dependency.span,
            );
        }
    }
    unreachable!("recursive component contains an internal dependency")
}

struct AttributeDependencyCollector<'flat> {
    source_ordinals: &'flat HashMap<VarName, usize>,
    dependencies: Vec<DependencyUse>,
    seen_dependencies: HashSet<usize>,
    shadowed: HashMap<VarName, usize>,
    requires_function_ids: bool,
}

impl<'flat> AttributeDependencyCollector<'flat> {
    fn new(source_ordinals: &'flat HashMap<VarName, usize>) -> Self {
        Self {
            source_ordinals,
            dependencies: Vec::new(),
            seen_dependencies: HashSet::new(),
            shadowed: HashMap::new(),
            requires_function_ids: false,
        }
    }

    fn with_shadowed(
        &mut self,
        names: impl IntoIterator<Item = VarName>,
        visit: impl FnOnce(&mut Self),
    ) {
        let names = names.into_iter().collect::<Vec<_>>();
        self.push_shadowed(&names);
        visit(self);
        self.pop_shadowed(&names);
    }

    fn push_shadowed(&mut self, names: &[VarName]) {
        for name in names {
            *self.shadowed.entry(name.clone()).or_default() += 1;
        }
    }

    fn pop_shadowed(&mut self, names: &[VarName]) {
        for name in names {
            let count = self
                .shadowed
                .get_mut(name)
                .expect("lexical scope exits after entry");
            *count -= 1;
            if *count == 0 {
                self.shadowed.remove(name);
            }
        }
    }
}

impl ExpressionVisitor for AttributeDependencyCollector<'_> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::VarRef { name, span, .. } = expression
            && !self.shadowed.contains_key(name.var_name())
            && let Some(&target) = self.source_ordinals.get(name.var_name())
            && self.seen_dependencies.insert(target)
        {
            self.dependencies.push(DependencyUse {
                target,
                span: *span,
            });
        }
        if let Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } = expression
        {
            self.requires_function_ids |= matches!(
                classify_function_call(name, *is_constructor),
                FunctionCallLowering::Registry
            );
        }
        self.walk_expression(expression);
    }

    fn enter_scope(&mut self, scope: ExpressionScope<'_>) {
        let ExpressionScope::ArrayComprehension(indices) = scope;
        let names = indices
            .iter()
            .map(|index| VarName::new(&index.name))
            .collect::<Vec<_>>();
        self.push_shadowed(&names);
    }

    fn exit_scope(&mut self, scope: ExpressionScope<'_>) {
        let ExpressionScope::ArrayComprehension(indices) = scope;
        let names = indices
            .iter()
            .map(|index| VarName::new(&index.name))
            .collect::<Vec<_>>();
        self.pop_shadowed(&names);
    }
}
