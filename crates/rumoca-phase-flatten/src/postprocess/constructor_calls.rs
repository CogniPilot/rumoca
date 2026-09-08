use std::collections::HashSet;

use crate::{ast, flat};
use rumoca_core::{ExpressionRewriter, StatementRewriter};

/// Every record class declaration: the exact targets whose call is a record
/// constructor.
pub(crate) fn record_constructor_def_ids(tree: &ast::ClassTree) -> HashSet<rumoca_core::DefId> {
    tree.def_map
        .keys()
        .copied()
        .filter(|def_id| {
            tree.get_class_by_def_id(*def_id)
                .is_some_and(|class_def| class_def.class_type == rumoca_core::ClassType::Record)
        })
        .collect()
}

/// Mark the record constructor calls of one expression that lives outside the
/// Flat model, with the same target test the Flat-wide pass applies.
pub(crate) fn mark_record_constructor_calls_in_expression(
    expr: &mut rumoca_core::Expression,
    constructor_def_ids: &HashSet<rumoca_core::DefId>,
) {
    ConstructorMarker {
        constructor_def_ids,
    }
    .mark_expr(expr);
}

pub(crate) fn mark_record_constructor_calls(flat: &mut flat::Model, tree: &ast::ClassTree) {
    let constructor_def_ids = record_constructor_def_ids(tree);
    let marker = ConstructorMarker {
        constructor_def_ids: &constructor_def_ids,
    };
    for var in flat.variables.values_mut() {
        marker.mark_opt_expr(&mut var.binding);
        marker.mark_opt_expr(&mut var.start);
        marker.mark_opt_expr(&mut var.min);
        marker.mark_opt_expr(&mut var.max);
        marker.mark_opt_expr(&mut var.nominal);
    }
    for eq in &mut flat.equations {
        marker.mark_expr(&mut eq.residual);
    }
    for eq in &mut flat.initial_equations {
        marker.mark_expr(&mut eq.residual);
    }
    // Structured templates are authoritative peer copies of their scalar
    // residuals. Downstream call canonicalization and argument materialization
    // read them directly, so constructor certification must cover them too.
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        if let Some(template) = &mut family.template {
            for expression in &mut template.body {
                marker.mark_expr(expression);
            }
        }
    }
    for assert_eq in &mut flat.assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for assert_eq in &mut flat.initial_assert_equations {
        marker.mark_expr(&mut assert_eq.condition);
        marker.mark_expr(&mut assert_eq.message);
        marker.mark_opt_expr(&mut assert_eq.level);
    }
    for algorithm in &mut flat.algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for algorithm in &mut flat.initial_algorithms {
        marker.mark_statements(&mut algorithm.statements);
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            marker.mark_expr(&mut branch.condition);
            marker.mark_when_equations(&mut branch.equations);
        }
    }
    for function in flat.functions.values_mut() {
        for param in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            marker.mark_function_param(param);
        }
        marker.mark_statements(&mut function.body);
    }
}

#[derive(Clone, Copy)]
struct ConstructorMarker<'a> {
    constructor_def_ids: &'a HashSet<rumoca_core::DefId>,
}

impl ConstructorMarker<'_> {
    fn mark_opt_expr(self, expr: &mut Option<rumoca_core::Expression>) {
        if let Some(expr) = expr {
            self.mark_expr(expr);
        }
    }

    fn mark_expr(mut self, expr: &mut rumoca_core::Expression) {
        *expr = self.rewrite_expression(expr);
    }

    fn mark_statements(mut self, statements: &mut [rumoca_core::Statement]) {
        for statement in statements {
            *statement = self.rewrite_statement(statement);
        }
    }

    /// Mark every parameter expression visited by downstream callable
    /// canonicalization and argument-slot materialization.
    fn mark_function_param(self, param: &mut rumoca_core::FunctionParam) {
        for expression in [&mut param.default, &mut param.min, &mut param.max]
            .into_iter()
            .flatten()
        {
            self.mark_expr(expression);
        }
        for subscript in &mut param.shape_expr {
            if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
                self.mark_expr(expr);
            }
        }
    }

    fn mark_when_equations(self, equations: &mut [flat::WhenEquation]) {
        for equation in equations {
            match equation {
                flat::WhenEquation::Assign { value, .. }
                | flat::WhenEquation::Reinit { value, .. } => self.mark_expr(value),
                flat::WhenEquation::Assert {
                    condition,
                    message,
                    level,
                    ..
                } => self.mark_assert(condition, message, level),
                flat::WhenEquation::Conditional {
                    branches,
                    else_branch,
                    ..
                } => self.mark_conditional_when_equation(branches, else_branch),
                flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                    self.mark_expr(function);
                }
                flat::WhenEquation::Terminate { message, .. } => self.mark_expr(message),
            }
        }
    }

    fn mark_assert(
        self,
        condition: &mut rumoca_core::Expression,
        message: &mut rumoca_core::Expression,
        level: &mut Option<Box<rumoca_core::Expression>>,
    ) {
        self.mark_expr(condition);
        self.mark_expr(message);
        if let Some(level) = level {
            self.mark_expr(level);
        }
    }

    fn mark_conditional_when_equation(
        self,
        branches: &mut [(rumoca_core::Expression, Vec<flat::WhenEquation>)],
        else_branch: &mut Option<Vec<flat::WhenEquation>>,
    ) {
        for (condition, branch_equations) in branches {
            self.mark_expr(condition);
            self.mark_when_equations(branch_equations);
        }
        if let Some(else_branch) = else_branch {
            self.mark_when_equations(else_branch);
        }
    }

    fn is_constructor_call(
        self,
        name: &rumoca_core::Reference,
        intermediate_class_call: bool,
    ) -> bool {
        match name.target_def_id() {
            Some(def_id) => self.constructor_def_ids.contains(&def_id),
            None => intermediate_class_call && name.is_generated(),
        }
    }
}

impl ExpressionRewriter for ConstructorMarker<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            call_kind,
            span,
        } = expr
        {
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: self.rewrite_expressions(args),
                is_constructor: self.is_constructor_call(name, *is_constructor),
                call_kind: *call_kind,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for ConstructorMarker<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    const RECORD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(91_001);
    const FIELD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(91_002);

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("constructor_template_test.mo"),
            1,
            2,
        )
    }

    fn record_tree() -> ast::ClassTree {
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert(
            "Dimension".to_string(),
            ast::ClassDef {
                class_type: rumoca_core::ClassType::Record,
                def_id: Some(RECORD_DEF_ID),
                ..ast::ClassDef::default()
            },
        );
        tree.def_map.insert(RECORD_DEF_ID, "Dimension".to_string());
        tree.name_map.insert("Dimension".to_string(), RECORD_DEF_ID);
        tree
    }

    fn constructor_call() -> rumoca_core::Expression {
        let component = rumoca_core::ComponentReference::construct(
            false,
            span(),
            vec![rumoca_core::ComponentRefPart {
                ident: "Dimension".to_string(),
                span: span(),
                subs: Vec::new(),
                def_id: RECORD_DEF_ID,
            }],
        )
        .expect("resolved record reference");
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(component),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: span(),
            }],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: span(),
        }
    }

    fn constructor_field() -> rumoca_core::Expression {
        rumoca_core::Expression::FieldAccess {
            base: Box::new(constructor_call()),
            field: "n".to_string(),
            field_def_id: FIELD_DEF_ID,
            span: span(),
        }
    }

    fn family() -> flat::StructuredEquationFamily {
        flat::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: Vec::new(),
            },
            first_equation_index: 0,
            equations_per_point: 1,
            span: span(),
            origin: flat::EquationOrigin::ComponentEquation {
                component: "record constructor template".to_string(),
            },
            regular: None,
            template: Some(rumoca_core::ComprehensionTemplate {
                body: vec![constructor_field()],
                scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
            }),
            interiors_materialized: false,
        }
    }

    fn model_with_constructor() -> (flat::Model, rumoca_core::FunctionInstanceId) {
        let mut model = flat::Model::new();
        let mut constructor =
            rumoca_core::Function::new("Dimension", rumoca_core::DefId::new(61_006), span());
        constructor.def_id = Some(RECORD_DEF_ID);
        constructor.is_constructor = true;
        constructor.add_input(
            crate::test_support::integer_param("n", Vec::new(), span()).with_def_id(FIELD_DEF_ID),
        );
        model.add_function(constructor);
        let instance_id = model.functions[&rumoca_core::VarName::new("Dimension")]
            .instance_id
            .expect("constructor instance identity");
        (model, instance_id)
    }

    fn assert_exact_constructor(
        expression: &rumoca_core::Expression,
        expected: rumoca_core::FunctionInstanceId,
    ) {
        let rumoca_core::Expression::FieldAccess { base, .. } = expression else {
            panic!("expected field access around record constructor");
        };
        let rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } = base.as_ref()
        else {
            panic!("expected record constructor");
        };
        assert!(*is_constructor);
        assert_eq!(
            name.resolved_function()
                .map(|resolved| resolved.instance_id),
            Some(expected)
        );
    }

    #[test]
    fn structured_templates_receive_certified_exact_constructor_identity() {
        let tree = record_tree();
        let (mut model, instance_id) = model_with_constructor();
        model.add_structured_equation(family());
        model.add_initial_structured_equation(family());

        mark_record_constructor_calls(&mut model, &tree);
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        crate::functions::canonicalize_collected_function_calls(&mut model, &class_index)
            .expect("certified template calls canonicalize exactly");
        crate::functions::materialize_flat_function_call_args(&mut model)
            .expect("template calls carry exact identity into materialization");

        for family in model
            .structured_equations
            .iter()
            .chain(model.initial_structured_equations.iter())
        {
            let expression = &family.template.as_ref().expect("template").body[0];
            assert_exact_constructor(expression, instance_id);
        }
    }

    #[test]
    fn function_parameter_metadata_receives_constructor_certification() {
        let tree = record_tree();
        let (mut model, instance_id) = model_with_constructor();
        let mut function = rumoca_core::Function::new("f", rumoca_core::DefId::new(61_007), span());
        let mut bounded = crate::test_support::integer_param("p", Vec::new(), span());
        bounded.default = Some(constructor_field());
        bounded.min = Some(constructor_field());
        bounded.max = Some(constructor_field());
        function.add_input(bounded);
        let mut shaped = crate::test_support::real_param("x", vec![2], span());
        shaped.shape_expr.push(rumoca_core::Subscript::Expr {
            expr: Box::new(constructor_field()),
            span: span(),
        });
        function.add_input(shaped);
        model.add_function(function);

        mark_record_constructor_calls(&mut model, &tree);
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        crate::functions::canonicalize_collected_function_calls(&mut model, &class_index)
            .expect("certified parameter calls canonicalize exactly");
        crate::functions::materialize_flat_function_call_args(&mut model)
            .expect("parameter calls carry exact identity into materialization");

        let function = &model.functions[&rumoca_core::VarName::new("f")];
        for expression in [
            function.inputs[0].default.as_ref().expect("default"),
            function.inputs[0].min.as_ref().expect("min"),
            function.inputs[0].max.as_ref().expect("max"),
        ] {
            assert_exact_constructor(expression, instance_id);
        }
        let rumoca_core::Subscript::Expr { expr, .. } = &function.inputs[1].shape_expr[0] else {
            panic!("expected symbolic shape expression");
        };
        assert_exact_constructor(expr, instance_id);
    }
}
