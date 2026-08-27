use rumoca_core::{ExpressionRewriter, StatementRewriter};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

const SOURCE: &str = r#"
function scale
  input Real x;
  input Real gain = x;
  output Real y;
protected
  Real local = gain;
algorithm
  y := x + local;
end scale;

model Cell
  Real omega;
end Cell;

model ReferenceAudit
  Cell motor[2];
  Real projected[2];
  Real scaled;
equation
  projected = motor.omega;
  scaled = scale(motor[1].omega);
end ReferenceAudit;
"#;

#[derive(Default)]
struct ReferenceAudit {
    failures: Vec<String>,
    function_scope: bool,
}

impl ReferenceAudit {
    fn check_component_reference(
        &mut self,
        reference: &rumoca_core::ComponentReference,
        context: &str,
    ) {
        if reference.span().is_dummy() {
            self.failures
                .push(format!("{context}: component reference has dummy span"));
        }
        for (index, part) in reference.parts().iter().enumerate() {
            if part.def_id.index() == 0 {
                self.failures.push(format!(
                    "{context}: component reference part {index} has DefId(0)"
                ));
            }
            if part.span.is_dummy() {
                self.failures.push(format!(
                    "{context}: component reference part {index} has dummy span"
                ));
            }
        }
    }

    fn check_reference(
        &mut self,
        reference: &rumoca_core::Reference,
        span: rumoca_core::Span,
        context: &str,
        require_instance: bool,
    ) {
        if reference.is_generated() {
            return;
        }
        if span.is_dummy() {
            self.failures.push(format!(
                "{context}: source reference has dummy use-site span"
            ));
        }
        let Some(component_ref) = reference.component_ref() else {
            self.failures.push(format!(
                "{context}: source reference `{}` has no structured identity",
                reference.as_str()
            ));
            return;
        };
        self.check_component_reference(component_ref, context);
        if require_instance
            && reference.instance_id().is_none()
            && reference.resolved_function().is_none()
        {
            self.failures.push(format!(
                "{context}: source reference `{}` has no occurrence identity",
                reference.as_str()
            ));
        }
    }

    fn expression(&mut self, expression: &rumoca_core::Expression) {
        let _ = self.rewrite_expression(expression);
    }

    fn expressions<'a>(
        &mut self,
        expressions: impl IntoIterator<Item = &'a rumoca_core::Expression>,
    ) {
        for expression in expressions {
            self.expression(expression);
        }
    }

    fn statements(&mut self, statements: &[rumoca_core::Statement]) {
        for statement in statements {
            let _ = self.rewrite_statement(statement);
        }
    }
}

impl ExpressionRewriter for ReferenceAudit {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        self.check_reference(name, span, "variable reference", !self.function_scope);
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        self.check_reference(name, span, "function call", false);
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: self.rewrite_expressions(args),
            is_constructor,
            span,
        }
    }
}

impl StatementRewriter for ReferenceAudit {
    fn rewrite_component_reference(
        &mut self,
        reference: &rumoca_core::ComponentReference,
    ) -> rumoca_core::ComponentReference {
        self.check_component_reference(reference, "statement reference");
        reference.clone()
    }
}

fn audit_when_equations(audit: &mut ReferenceAudit, equations: &[flat::WhenEquation]) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                audit.expression(value)
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                audit.expression(condition);
                audit.expression(message);
                audit.expressions(level.iter().map(Box::as_ref));
            }
            flat::WhenEquation::Terminate { message, .. } => audit.expression(message),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    audit.expression(condition);
                    audit_when_equations(audit, equations);
                }
                if let Some(equations) = else_branch {
                    audit_when_equations(audit, equations);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                audit.expression(function);
            }
        }
    }
}

fn audit_function(audit: &mut ReferenceAudit, function: &rumoca_core::Function) {
    audit.function_scope = true;
    for parameter in function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
    {
        audit.expressions(
            parameter
                .default
                .iter()
                .chain(parameter.min.iter())
                .chain(parameter.max.iter()),
        );
        for shape in &parameter.shape_expr {
            if let rumoca_core::Subscript::Expr { expr, .. } = shape {
                audit.expression(expr);
            }
        }
    }
    audit.statements(&function.body);
    if let Some(external) = &function.external {
        audit.expressions(&external.args);
        audit.expressions(
            external
                .annotations
                .iter()
                .map(|annotation| &annotation.value),
        );
    }
    audit.function_scope = false;
}

fn audit_model(flat: &flat::Model) -> Vec<String> {
    let mut audit = ReferenceAudit::default();
    for variable in flat.variables.values() {
        audit.expressions(
            variable
                .binding
                .iter()
                .chain(variable.start.iter())
                .chain(variable.min.iter())
                .chain(variable.max.iter())
                .chain(variable.nominal.iter()),
        );
    }
    audit.expressions(flat.equations.iter().map(|equation| &equation.residual));
    audit.expressions(
        flat.initial_equations
            .iter()
            .map(|equation| &equation.residual),
    );
    for family in flat
        .structured_equations
        .iter()
        .chain(flat.initial_structured_equations.iter())
    {
        if let Some(template) = &family.template {
            audit.expressions(&template.body);
        }
    }
    for assertion in flat
        .assert_equations
        .iter()
        .chain(flat.initial_assert_equations.iter())
    {
        audit.expression(&assertion.condition);
        audit.expression(&assertion.message);
        audit.expressions(assertion.level.iter());
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            audit.expression(&branch.condition);
            audit_when_equations(&mut audit, &branch.equations);
        }
    }
    for algorithm in flat.algorithms.iter().chain(flat.initial_algorithms.iter()) {
        audit.statements(&algorithm.statements);
    }
    for function in flat.functions.values() {
        audit_function(&mut audit, function);
    }
    audit.failures
}

fn flatten_source() -> flat::Model {
    let file_name = "flat_reference_contract_test.mo";
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, file_name).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let instanced = rumoca_phase_instantiate::instantiate(resolved, "ReferenceAudit")
        .expect("fixture instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "ReferenceAudit")
        .expect("fixture typechecks");
    crate::flatten_ref_with_options(
        &tree,
        &overlay,
        "ReferenceAudit",
        crate::FlattenOptions {
            simplify_variable_names: true,
            ..crate::FlattenOptions::default()
        },
    )
    .expect("fixture flattens")
}

#[test]
fn finalized_flat_source_references_are_exact_including_functions() {
    let flat = flatten_source();
    let function = flat
        .functions
        .values()
        .find(|function| function.name.as_str().ends_with("scale"))
        .expect("called function remains in finalized Flat");
    assert!(
        function
            .inputs
            .iter()
            .any(|parameter| parameter.name == "gain" && parameter.default.is_some()),
        "fixture must exercise a function parameter default"
    );
    assert!(
        function
            .locals
            .iter()
            .any(|parameter| parameter.name == "local" && parameter.default.is_some()),
        "fixture must exercise a function local binding"
    );
    assert!(
        !function.body.is_empty(),
        "fixture must exercise function body statements"
    );
    let failures = audit_model(&flat);
    assert!(
        failures.is_empty(),
        "finalized Flat references violated their construction contract:\n{}",
        failures.join("\n")
    );
}
