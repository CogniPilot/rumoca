use super::*;
use std::ops::ControlFlow::Continue;

pub(super) const ER055_DELAY_PARAMETER_EXPRESSION: &str = "ER055";
pub(super) const ER056_OPERATOR_NOT_ALLOWED_IN_FUNCTION: &str = "ER056";
pub(super) const ER057_CARDINALITY_INVALID_TARGET: &str = "ER057";
pub(super) const ER058_EXPANDABLE_FLOW_COMPONENT: &str = "ER058";
pub(super) const ER059_EXPANDABLE_CONNECTOR_MISMATCH: &str = "ER059";

pub(super) fn run_builtin_call_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = BuiltinCallVisitor {
        def,
        class_path: Vec::new(),
        in_function: false,
        diags: &mut diags,
    };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct BuiltinCallVisitor<'a> {
    def: &'a StoredDefinition,
    class_path: Vec<String>,
    in_function: bool,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for BuiltinCallVisitor<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> std::ops::ControlFlow<()> {
        let previous_in_function = self.in_function;

        self.class_path.push(class.name.text.to_string());
        self.in_function = class.class_type == ClassType::Function;

        if let Some(constrainedby) = &class.constrainedby {
            self.visit_type_name(constrainedby, ast::TypeNameContext::ClassConstrainedBy)?;
        }
        for ext in &class.extends {
            self.visit_extend(ext)?;
        }
        for (_, nested) in &class.classes {
            self.visit_class_def(nested)?;
        }
        for (_, comp) in &class.components {
            self.visit_component(comp)?;
        }
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for section in &class.algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        for section in &class.initial_algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }

        self.class_path.pop();
        self.in_function = previous_in_function;
        Continue(())
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ast::FunctionCallContext::Expression)
            && let Some(name) = builtin_name(comp)
        {
            match name {
                "delay" => self.check_delay_call(comp, args),
                "cardinality" => self.check_cardinality_call(comp, args),
                "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample"
                | "inStream" | "actualStream" | "pre" | "sample" | "edge" | "change"
                | "initial" | "terminal" => self.check_function_forbidden_operator(comp, name),
                _ => {}
            }
        }

        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

impl BuiltinCallVisitor<'_> {
    fn current_class(&self) -> Option<&ClassDef> {
        let (first, rest) = self.class_path.split_first()?;
        let mut current = self.def.classes.get(first)?;
        for segment in rest {
            current = current.classes.get(segment)?;
        }
        Some(current)
    }

    fn check_delay_call(&mut self, comp: &ComponentReference, args: &[Expression]) {
        self.check_function_forbidden_operator(comp, "delay");

        let Some(operator_token) = comp.parts.first().map(|part| &part.ident) else {
            return;
        };
        let delay_diag = {
            let Some(class) = self.current_class() else {
                return;
            };
            match args {
                [_, delay_time] if !is_parameter_expression(delay_time, class) => {
                    Some(semantic_error(
                        ER055_DELAY_PARAMETER_EXPRESSION,
                        "delay() time argument must be a parameter expression when delayMax is omitted (MLS §3.7)",
                        label_from_expression_or_token(
                            delay_time,
                            "check_delay_call/delay_time_parameter_expression",
                            operator_token,
                            "check_delay_call/delay_time_parameter_expression_operator",
                            "delayTime must be a parameter expression".to_string(),
                        ),
                    ))
                }
                [_, _, delay_max] if !is_parameter_expression(delay_max, class) => {
                    Some(semantic_error(
                        ER055_DELAY_PARAMETER_EXPRESSION,
                        "delay() delayMax argument must be a parameter expression (MLS §3.7)",
                        label_from_expression_or_token(
                            delay_max,
                            "check_delay_call/delay_max_parameter_expression",
                            operator_token,
                            "check_delay_call/delay_max_parameter_expression_operator",
                            "delayMax must be a parameter expression".to_string(),
                        ),
                    ))
                }
                _ => None,
            }
        };

        if let Some(diag) = delay_diag {
            self.diags.push(diag);
        }
    }

    fn check_cardinality_call(&mut self, comp: &ComponentReference, args: &[Expression]) {
        self.check_function_forbidden_operator(comp, "cardinality");

        let cardinality_diags = {
            let Some(class) = self.current_class() else {
                return;
            };
            let [Expression::ComponentReference(cref)] = args else {
                return;
            };
            let Some(target) = resolve_component_reference_target(class, cref, self.def) else {
                return;
            };

            let cref_text = cref.to_string();
            let token = target.token.clone();
            let is_array = component_reference_targets_array(target.component, target.part);
            let is_expandable = target.type_class.is_some_and(|type_class| {
                type_class.class_type == ClassType::Connector && type_class.expandable
            });

            let mut diags = Vec::new();
            if is_array {
                diags.push(semantic_error(
                    ER057_CARDINALITY_INVALID_TARGET,
                    format!(
                        "cardinality() cannot be applied to connector array '{}' (MLS §3.7.4.2)",
                        cref_text
                    ),
                    label_from_token(
                        &token,
                        "check_cardinality_call/array_connector_target",
                        "cardinality() target is a connector array",
                    ),
                ));
            }
            if is_expandable {
                diags.push(semantic_error(
                    ER057_CARDINALITY_INVALID_TARGET,
                    format!(
                        "cardinality() cannot be applied to expandable connector '{}' (MLS §3.7.4.2)",
                        cref_text
                    ),
                    label_from_token(
                        &token,
                        "check_cardinality_call/expandable_connector_target",
                        "cardinality() target is an expandable connector",
                    ),
                ));
            }
            diags
        };

        self.diags.extend(cardinality_diags);
    }

    fn check_function_forbidden_operator(&mut self, comp: &ComponentReference, operator: &str) {
        if !self.in_function {
            return;
        }

        let Some(token) = comp.parts.first().map(|part| &part.ident) else {
            return;
        };

        self.diags.push(semantic_error(
            ER056_OPERATOR_NOT_ALLOWED_IN_FUNCTION,
            format!("{operator}() is not allowed in functions (MLS §3.7 / §12.2)"),
            label_from_token(
                token,
                "check_function_forbidden_operator/operator",
                format!("{operator}() is not allowed in function classes"),
            ),
        ));
    }
}

pub(super) fn builtin_name(comp: &ComponentReference) -> Option<&str> {
    (comp.parts.len() == 1).then(|| comp.parts[0].ident.text.as_ref())
}

fn component_reference_targets_array(
    component: &ast::Component,
    part: &ast::ComponentRefPart,
) -> bool {
    let dimension_count = component.shape.len().max(component.shape_expr.len());
    if dimension_count == 0 {
        return false;
    }
    let indexed_dimension_count = part.subs.as_ref().map_or(0, |subs| {
        subs.iter()
            .take_while(|sub| matches!(sub, Subscript::Expression(_)))
            .count()
    });
    indexed_dimension_count < dimension_count
}

fn is_parameter_expression(expr: &Expression, class: &ClassDef) -> bool {
    rumoca_ir_ast::collect_component_refs(expr)
        .into_iter()
        .all(|cref| match cref.parts.as_slice() {
            [part] => class
                .components
                .get(part.ident.text.as_ref())
                .is_some_and(|component| {
                    matches!(
                        component.variability,
                        Variability::Parameter(_) | Variability::Constant(_)
                    )
                }),
            _ => false,
        })
}
