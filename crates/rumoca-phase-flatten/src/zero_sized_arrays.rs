//! Zero-sized array materialization (MLS §10.3.4, ARR-036/039).
//!
//! Zero-sized components emit no variables, but references to them survive in
//! equations, asserts, bindings, and start values. This pass materializes a
//! zero-size placeholder variable for every dangling reference so reductions
//! lower to their identity values instead of failing as unresolved.

use crate::Context;
use rumoca_core::ExpressionVisitor;
use rumoca_ir_flat as flat;

pub(crate) fn materialize_referenced_zero_sized_array_variables(
    flat: &mut flat::Model,
    ctx: &Context,
) {
    let mut collector = MissingZeroSizedArrayRefCollector {
        flat,
        ctx,
        refs: Vec::new(),
    };
    collector.collect();

    for name in collector.refs {
        let Some(dims) = zero_sized_array_dims_for_ref(&name, ctx) else {
            continue;
        };
        let var_name = name.var_name().clone();
        if flat.variables.contains_key(&var_name) {
            continue;
        }
        let variable = flat::Variable {
            name: var_name.clone(),
            component_ref: name.component_ref().cloned(),
            dims,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            is_protected: true,
            ..Default::default()
        };
        flat.variable_type_names
            .entry(var_name.clone())
            .or_insert_with(|| "Real".to_string());
        flat.add_variable(var_name, variable);
    }
}

struct MissingZeroSizedArrayRefCollector<'a> {
    flat: &'a flat::Model,
    ctx: &'a Context,
    refs: Vec<rumoca_core::Reference>,
}

impl MissingZeroSizedArrayRefCollector<'_> {
    fn collect(&mut self) {
        for equation in &self.flat.equations {
            self.visit_expression(&equation.residual);
        }
        for equation in &self.flat.initial_equations {
            self.visit_expression(&equation.residual);
        }
        for assertion in &self.flat.assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
        for assertion in &self.flat.initial_assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
        // Variable bindings and start values reference zero-sized arrays just
        // like equations do (e.g. `Real s = sum(z)` with `Real z[0]`).
        let variables: Vec<_> = self.flat.variables.values().collect();
        for variable in variables {
            if let Some(binding) = &variable.binding {
                self.visit_expression(binding);
            }
            if let Some(start) = &variable.start {
                self.visit_expression(start);
            }
        }
    }
}

impl rumoca_core::ExpressionVisitor for MissingZeroSizedArrayRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if subscripts.is_empty()
            && !self.flat.variables.contains_key(name.var_name())
            && zero_sized_array_dims_for_ref(name, self.ctx).is_some()
        {
            self.refs.push(name.clone());
        }
        self.walk_var_ref(name, subscripts);
    }
}

fn zero_sized_array_dims_for_ref(name: &rumoca_core::Reference, ctx: &Context) -> Option<Vec<i64>> {
    let dims = ctx.array_dimensions.get(name.as_str()).or_else(|| {
        name.target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .and_then(|target_name| ctx.array_dimensions.get(target_name))
    })?;
    (!dims.is_empty() && dims.iter().any(|dim| *dim <= 0)).then(|| dims.clone())
}
