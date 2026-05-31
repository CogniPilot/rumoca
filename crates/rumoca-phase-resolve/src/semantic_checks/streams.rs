use super::*;
use rumoca_ir_ast::ExpressionContext;
use std::collections::HashSet;
use std::ops::ControlFlow::Continue;

pub(super) const ER064_STREAM_PREFIX_SCOPE: &str = "ER064";
pub(super) const ER065_STREAM_CONNECTOR_FLOW_COUNT: &str = "ER065";
pub(super) const ER066_STREAM_FLOW_TYPE: &str = "ER066";
pub(super) const ER067_INSTREAM_STREAM_ONLY: &str = "ER067";
pub(super) const ER068_ACTUALSTREAM_STREAM_ONLY: &str = "ER068";

pub(super) fn check_stream_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        match &comp.connection {
            Connection::Stream(token) => {
                if class.class_type != ClassType::Connector {
                    diags.push(semantic_error(
                        ER064_STREAM_PREFIX_SCOPE,
                        format!(
                            "component '{}' cannot have 'stream' prefix outside a connector declaration (MLS §15.1)",
                            name
                        ),
                        label_from_token(
                            token,
                            "check_stream_restrictions/stream_prefix_scope",
                            "stream prefix is only allowed in connector declarations",
                        ),
                    ));
                }
            }
            Connection::Flow(_) => {}
            Connection::Empty => {}
        }
    }

    if class.class_type != ClassType::Connector {
        return;
    }

    let level = collect_stream_connector_level(class, def, &mut HashSet::new());
    validate_stream_connector_level(class, def, &level, diags);
}

struct StreamConnectorLevel<'a> {
    stream_components: Vec<(&'a str, &'a ast::Component, &'a Token)>,
    flow_components: Vec<(&'a str, &'a ast::Component, &'a Token)>,
}

fn collect_stream_connector_level<'a>(
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    visited_extends: &mut HashSet<DefId>,
) -> StreamConnectorLevel<'a> {
    let mut level = StreamConnectorLevel {
        stream_components: Vec::new(),
        flow_components: Vec::new(),
    };
    collect_stream_connector_level_inner(class, def, visited_extends, &mut level);
    level
}

fn collect_stream_connector_level_inner<'a>(
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    visited_extends: &mut HashSet<DefId>,
    level: &mut StreamConnectorLevel<'a>,
) {
    if let Some(def_id) = class.def_id
        && !visited_extends.insert(def_id)
    {
        return;
    }

    for ext in &class.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base_class) = find_class_by_def_id(def, base_def_id) else {
            continue;
        };
        collect_stream_connector_level_inner(base_class, def, visited_extends, level);
    }

    for (name, comp) in &class.components {
        match &comp.connection {
            Connection::Stream(token) => level.stream_components.push((name.as_str(), comp, token)),
            Connection::Flow(token) => level.flow_components.push((name.as_str(), comp, token)),
            Connection::Empty => {}
        }
    }
}

fn validate_stream_connector_level(
    class: &ClassDef,
    def: &StoredDefinition,
    level: &StreamConnectorLevel<'_>,
    diags: &mut Vec<Diagnostic>,
) {
    if level.stream_components.is_empty() {
        return;
    }

    if level.flow_components.len() != 1 {
        diags.push(semantic_error(
            ER065_STREAM_CONNECTOR_FLOW_COUNT,
            format!(
                "stream connector '{}' must contain exactly one flow variable at the same level, found {} (MLS §15.1)",
                class.name.text,
                level.flow_components.len()
            ),
            label_from_token(
                &class.name,
                "check_stream_restrictions/stream_connector_flow_count",
                "stream connector requires exactly one same-level flow variable",
            ),
        ));
        return;
    }

    let (flow_name, flow_component, flow_token) = level.flow_components[0];
    if !flow_component.shape.is_empty() || !flow_component.shape_expr.is_empty() {
        diags.push(semantic_error(
            ER066_STREAM_FLOW_TYPE,
            format!(
                "flow variable '{}' in stream connector '{}' must be scalar Real (MLS §15.1)",
                flow_name, class.name.text
            ),
            label_from_token(
                flow_token,
                "check_stream_restrictions/stream_flow_scalar",
                "flow variable in stream connector must be scalar",
            ),
        ));
        return;
    }

    if !matches!(
        resolve_component_type_root(flow_component, def),
        Some(ResolvedTypeRoot::Builtin("Real"))
    ) {
        diags.push(semantic_error(
            ER066_STREAM_FLOW_TYPE,
            format!(
                "flow variable '{}' in stream connector '{}' must be a subtype of Real (MLS §15.1)",
                flow_name, class.name.text
            ),
            label_from_token(
                flow_token,
                "check_stream_restrictions/stream_flow_real_type",
                "flow variable in stream connector must resolve to Real",
            ),
        ));
    }
}

pub(super) fn run_stream_builtin_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut visitor = StreamBuiltinVisitor {
        def,
        class_path: Vec::new(),
        diags: &mut diags,
    };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct StreamBuiltinVisitor<'a> {
    def: &'a StoredDefinition,
    class_path: Vec<String>,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for StreamBuiltinVisitor<'_> {
    fn visit_expression_ctx(
        &mut self,
        expr: &Expression,
        ctx: ExpressionContext,
    ) -> std::ops::ControlFlow<()> {
        if matches!(ctx, ExpressionContext::ComponentAnnotation) {
            return Continue(());
        }
        self.visit_expression(expr)
    }

    fn visit_class_def(&mut self, class: &ClassDef) -> std::ops::ControlFlow<()> {
        self.class_path.push(class.name.text.to_string());

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
                "inStream" => self.check_stream_builtin_argument(
                    comp,
                    args,
                    ER067_INSTREAM_STREAM_ONLY,
                    "inStream",
                    "check_stream_builtin_argument/instream",
                ),
                "actualStream" => self.check_stream_builtin_argument(
                    comp,
                    args,
                    ER068_ACTUALSTREAM_STREAM_ONLY,
                    "actualStream",
                    "check_stream_builtin_argument/actualstream",
                ),
                _ => {}
            }
        }

        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

impl StreamBuiltinVisitor<'_> {
    fn current_class(&self) -> Option<&ClassDef> {
        let (first, rest) = self.class_path.split_first()?;
        let mut current = self.def.classes.get(first)?;
        for segment in rest {
            current = current.classes.get(segment)?;
        }
        Some(current)
    }

    fn check_stream_builtin_argument(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        code: &str,
        operator: &str,
        context: &str,
    ) {
        let Some(operator_token) = comp.parts.first().map(|part| &part.ident) else {
            return;
        };
        let Some(class) = self.current_class() else {
            return;
        };
        let Some(arg) = args.first() else {
            return;
        };

        if let Expression::ComponentReference(cref) = arg {
            let Some(target) = resolve_component_reference_target(class, cref, self.def) else {
                return;
            };
            if matches!(target.component.connection, Connection::Stream(_)) {
                return;
            }
        }

        self.diags.push(semantic_error(
            code,
            format!("{operator}() is only allowed on stream variables (MLS §15)"),
            label_from_expression_or_token(
                arg,
                context,
                operator_token,
                "check_stream_builtin_argument/operator",
                format!("argument to {operator}() must be a stream variable"),
            ),
        ));
    }
}
