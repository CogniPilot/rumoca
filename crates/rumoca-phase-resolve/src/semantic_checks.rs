//! Semantic validation checks applied during the resolve phase.
//!
//! These checks enforce structural rules from the Modelica Language Specification
//! that can be validated purely from the AST without needing type information
//! or instance data.

use rumoca_core::{Diagnostic, PrimaryLabel, SourceId, SourceMap, Span};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::Visitor;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

#[path = "semantic_checks_expr.rs"]
mod semantic_checks_expr;
use semantic_checks_expr::*;

type Causality = rumoca_ir_core::Causality;
type ClassDef = ast::ClassDef;
type ClassType = ast::ClassType;
type ComponentReference = ast::ComponentReference;
type Connection = ast::Connection;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Import = ast::Import;
type Location = rumoca_ir_core::Location;
type OpBinary = rumoca_ir_core::OpBinary;
type Statement = ast::Statement;
type Subscript = ast::Subscript;
type StoredDefinition = ast::StoredDefinition;
type TerminalType = ast::TerminalType;
type Token = rumoca_ir_core::Token;
type Variability = rumoca_ir_core::Variability;

fn walk_expression_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    expr: &Expression,
) -> std::ops::ControlFlow<()> {
    match expr {
        Expression::Empty | Expression::Terminal { .. } => std::ops::ControlFlow::Continue(()),
        Expression::Range { start, step, end } => {
            visitor.visit_expression(start)?;
            if let Some(s) = step {
                visitor.visit_expression(s)?;
            }
            visitor.visit_expression(end)
        }
        Expression::Unary { rhs, .. } => visitor.visit_expression(rhs),
        Expression::Binary { lhs, rhs, .. } => {
            visitor.visit_expression(lhs)?;
            visitor.visit_expression(rhs)
        }
        Expression::ComponentReference(cr) => {
            visitor.visit_component_reference_ctx(cr, ast::ComponentReferenceContext::Expression)
        }
        Expression::FunctionCall { comp, args } => {
            visitor.visit_expr_function_call_ctx(comp, args, ast::FunctionCallContext::Expression)
        }
        Expression::ClassModification {
            target,
            modifications,
        } => {
            visitor.visit_component_reference_ctx(
                target,
                ast::ComponentReferenceContext::ClassModificationTarget,
            )?;
            visitor.visit_each(modifications, V::visit_expression)
        }
        Expression::NamedArgument { value, .. } => visitor.visit_expression(value),
        Expression::Modification { target, value } => {
            visitor.visit_component_reference_ctx(
                target,
                ast::ComponentReferenceContext::ModificationTarget,
            )?;
            visitor.visit_expression(value)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            visitor.visit_each(elements, V::visit_expression)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                visitor.visit_expression(cond)?;
                visitor.visit_expression(then_expr)?;
            }
            visitor.visit_expression(else_branch)
        }
        Expression::Parenthesized { inner } => visitor.visit_expression(inner),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            visitor.visit_expression(expr)?;
            visitor.visit_each(indices, V::visit_for_index)?;
            if let Some(f) = filter {
                visitor.visit_expression(f)?;
            }
            std::ops::ControlFlow::Continue(())
        }
        Expression::ArrayIndex { base, subscripts } => {
            visitor.visit_expression(base)?;
            for subscript in subscripts {
                visitor.visit_subscript_ctx(subscript, ast::SubscriptContext::ArrayIndex)?;
            }
            std::ops::ControlFlow::Continue(())
        }
        Expression::FieldAccess { base, .. } => visitor.visit_expression(base),
    }
}

fn walk_equation_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    eq: &Equation,
) -> std::ops::ControlFlow<()> {
    match eq {
        Equation::Empty => std::ops::ControlFlow::Continue(()),
        Equation::Simple { lhs, rhs } => visitor.visit_simple_equation(lhs, rhs),
        Equation::Connect { lhs, rhs } => visitor.visit_connect(lhs, rhs),
        Equation::For { indices, equations } => visitor.visit_for_equation(indices, equations),
        Equation::When(blocks) => visitor.visit_when_equation(blocks),
        Equation::If {
            cond_blocks,
            else_block,
        } => visitor.visit_if_equation(cond_blocks, else_block.as_deref()),
        Equation::FunctionCall { comp, args } => visitor.visit_equation_function_call(comp, args),
        Equation::Assert {
            condition,
            message,
            level,
        } => visitor.visit_equation_assert(condition, message, level.as_ref()),
    }
}

fn walk_statement_default<V: Visitor + ?Sized>(
    visitor: &mut V,
    stmt: &Statement,
) -> std::ops::ControlFlow<()> {
    match stmt {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {
            std::ops::ControlFlow::Continue(())
        }
        Statement::Assignment { comp, value } => visitor.visit_assignment(comp, value),
        Statement::For { indices, equations } => visitor.visit_for_statement(indices, equations),
        Statement::While(block) => visitor.visit_statement_block(block),
        Statement::If {
            cond_blocks,
            else_block,
        } => visitor.visit_if_statement(cond_blocks, else_block.as_deref()),
        Statement::When(blocks) => visitor.visit_when_statement(blocks),
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => visitor.visit_statement_function_call(comp, args, outputs),
        Statement::Reinit { variable, value } => visitor.visit_reinit(variable, value),
        Statement::Assert {
            condition,
            message,
            level,
        } => visitor.visit_statement_assert(condition, message, level.as_ref()),
    }
}

// Resolve-phase semantic diagnostic codes (ER005+ reserved for semantic checks).
const ER005_PARTIAL_CLASS_INSTANTIATION: &str = "ER005";
const ER006_PARAMETER_VARIABILITY: &str = "ER006";
const ER007_CYCLIC_PARAMETER_BINDING: &str = "ER007";
const ER008_REINIT_OUTSIDE_WHEN: &str = "ER008";
const ER009_CONNECT_ARG_NOT_CONNECTOR: &str = "ER009";
const ER010_IF_CONDITION_NOT_BOOLEAN: &str = "ER010";
const ER011_CLASS_USED_AS_VALUE: &str = "ER011";
const ER012_DUPLICATE_IMPORT_NAME: &str = "ER012";
const ER013_FUNCTION_PUBLIC_MISSING_IO_PREFIX: &str = "ER013";
const ER014_FUNCTION_INPUT_ASSIGNED: &str = "ER014";
const ER015_WHEN_IN_FUNCTION: &str = "ER015";
const ER016_NESTED_WHEN_STATEMENT: &str = "ER016";
const ER017_NESTED_WHEN_EQUATION: &str = "ER017";
const ER018_WHEN_IN_INITIAL_SECTION: &str = "ER018";
const ER019_FOR_LOOP_VARIABLE_ASSIGNED: &str = "ER019";
const ER020_BLOCK_CONNECTOR_MISSING_IO_PREFIX: &str = "ER020";
const ER021_RECORD_PROTECTED_ELEMENT: &str = "ER021";
const ER022_RECORD_INVALID_PREFIX: &str = "ER022";
const ER023_RECORD_INVALID_COMPONENT_TYPE: &str = "ER023";
const ER024_CONNECTOR_INNER_OUTER_PREFIX: &str = "ER024";
const ER025_PROTECTED_DOT_ACCESS: &str = "ER025";
const ER026_DER_ON_DISCRETE: &str = "ER026";
const ER027_CONNECTOR_PARAMETER_OR_CONSTANT: &str = "ER027";
const ER028_UNBALANCED_CONNECTOR: &str = "ER028";
const ER029_REAL_EQUALITY_COMPARISON: &str = "ER029";
const ER030_DER_IN_FUNCTION: &str = "ER030";
const ER031_END_OUTSIDE_SUBSCRIPT: &str = "ER031";
const ER032_DUPLICATE_COMPONENT_NAME: &str = "ER032";
const ER033_COMPONENT_CLASS_NAME_CONFLICT: &str = "ER033";
const ER034_CONNECTOR_PROTECTED_ELEMENT: &str = "ER034";
const ER035_INPUT_PARAMETER_COMBINATION: &str = "ER035";
const ER036_COMPONENT_NAME_EQUALS_CLASS_NAME: &str = "ER036";
const ER037_PACKAGE_NON_CONSTANT_COMPONENT: &str = "ER037";
const ER038_FUNCTION_EQUATION_SECTION: &str = "ER038";
const ER039_CHAINED_RELATIONAL_OPERATOR: &str = "ER039";

/// Context tracking for nested traversal (function vs model, when depth, etc.)
struct CheckContext {
    in_function: bool,
    in_when_equation: bool,
    in_when_statement: bool,
    for_loop_vars: Vec<String>,
}

impl CheckContext {
    fn new() -> Self {
        Self {
            in_function: false,
            in_when_equation: false,
            in_when_statement: false,
            for_loop_vars: Vec::new(),
        }
    }
}

thread_local! {
    static ACTIVE_SEMANTIC_SOURCE_IDS: RefCell<Option<HashMap<String, SourceId>>> = const { RefCell::new(None) };
}

fn set_active_source_map(source_map: &SourceMap) {
    ACTIVE_SEMANTIC_SOURCE_IDS.with(|slot| {
        *slot.borrow_mut() = Some(source_map.source_ids());
    })
}

fn source_id_for(file_name: &str) -> SourceId {
    ACTIVE_SEMANTIC_SOURCE_IDS.with(|slot| {
        let ids_ref = slot.borrow();
        let ids = ids_ref
            .as_ref()
            .unwrap_or_else(|| panic!("semantic source ids are not initialized"));
        *ids.get(file_name)
            .unwrap_or_else(|| panic!("missing semantic source id for file '{file_name}'"))
    })
}

fn span_from_location(location: &Location) -> Option<Span> {
    if location.file_name.is_empty() {
        return None;
    }
    let start = location.start as usize;
    let end = (location.end as usize).max(start.saturating_add(1));
    Some(Span::from_offsets(
        source_id_for(&location.file_name),
        start,
        end,
    ))
}

fn label_from_location(
    location: &Location,
    context: &str,
    message: impl Into<String>,
) -> Option<PrimaryLabel> {
    let _ = context;
    span_from_location(location).map(|span| PrimaryLabel::new(span).with_message(message))
}

fn label_from_token(token: &Token, context: &str, message: impl Into<String>) -> PrimaryLabel {
    let _ = context;
    let start = token.location.start as usize;
    let end = (token.location.end as usize).max(start.saturating_add(1));
    PrimaryLabel::new(Span::from_offsets(
        source_id_for(&token.location.file_name),
        start,
        end,
    ))
    .with_message(message)
}

fn label_from_expression(
    expr: &Expression,
    context: &str,
    message: impl Into<String>,
) -> Option<PrimaryLabel> {
    expr.get_location()
        .and_then(|location| label_from_location(location, context, message))
}

fn label_from_equation(
    eq: &Equation,
    context: &str,
    message: impl Into<String>,
) -> Option<PrimaryLabel> {
    eq.get_location()
        .and_then(|location| label_from_location(location, context, message))
}

fn label_from_statement(
    stmt: &Statement,
    context: &str,
    message: impl Into<String>,
) -> Option<PrimaryLabel> {
    stmt.get_location()
        .and_then(|location| label_from_location(location, context, message))
}

fn label_from_expression_or_token(
    expr: &Expression,
    expr_context: &str,
    token: &Token,
    token_context: &str,
    message: String,
) -> PrimaryLabel {
    label_from_expression(expr, expr_context, message.clone())
        .unwrap_or_else(|| label_from_token(token, token_context, message))
}

fn semantic_error(
    code: &str,
    message: impl Into<String>,
    primary_label: PrimaryLabel,
) -> Diagnostic {
    Diagnostic::error(code, message, primary_label)
}

/// Run all semantic checks on a StoredDefinition and collect diagnostics.
pub fn check_semantics(def: &StoredDefinition, source_map: &SourceMap) -> Vec<Diagnostic> {
    set_active_source_map(source_map);
    run_semantic_checks(def)
}

/// Run all semantic check batches with a single active source-map setup.
pub fn check_all_semantics(def: &StoredDefinition, source_map: &SourceMap) -> Vec<Diagnostic> {
    set_active_source_map(source_map);
    let mut diags = run_semantic_checks(def);
    diags.extend(run_chained_relational_checks(def));
    diags.extend(run_der_in_function_checks(def));
    diags
}

fn run_semantic_checks(def: &StoredDefinition) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut ctx = CheckContext::new();
    let mut visitor = SemanticClassCheckVisitor {
        def,
        ctx: &mut ctx,
        diags: &mut diags,
    };
    let _ = visitor.visit_stored_definition(def);
    diags
}

struct SemanticClassCheckVisitor<'a> {
    def: &'a StoredDefinition,
    ctx: &'a mut CheckContext,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for SemanticClassCheckVisitor<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> std::ops::ControlFlow<()> {
        check_class_structural(class, self.def, self.diags);

        let was_function = self.ctx.in_function;
        if class.class_type == ClassType::Function {
            self.ctx.in_function = true;
        }

        let discrete_vars: HashSet<String> = class
            .components
            .iter()
            .filter(|(_, c)| matches!(c.variability, Variability::Discrete(_)))
            .map(|(n, _)| n.clone())
            .collect();
        let real_vars: HashSet<String> = class
            .components
            .iter()
            .filter(|(_, c)| c.type_name.to_string() == "Real")
            .map(|(n, _)| n.clone())
            .collect();

        for eq in &class.equations {
            check_equation(eq, self.ctx, self.diags);
            check_der_on_discrete_eq(eq, &discrete_vars, self.diags);
            check_protected_access_eq(eq, class, self.def, self.diags);
            check_connect_requires_connectors_eq(eq, class, self.def, self.diags);
            check_end_outside_subscript_eq(eq, self.diags);
            check_expr_type_issues_eq(eq, class, self.def, &real_vars, self.diags);
        }

        for eq in &class.initial_equations {
            check_initial_equation(eq, self.diags);
            check_equation(eq, self.ctx, self.diags);
            check_der_on_discrete_eq(eq, &discrete_vars, self.diags);
            check_end_outside_subscript_eq(eq, self.diags);
        }

        for alg in &class.initial_algorithms {
            for stmt in alg {
                check_initial_statement(stmt, self.diags);
                check_statement(stmt, self.ctx, self.diags);
            }
        }
        for alg in &class.algorithms {
            for stmt in alg {
                check_statement(stmt, self.ctx, self.diags);
            }
        }
        for nested in class.classes.values() {
            self.visit_class_def(nested)?;
        }

        self.ctx.in_function = was_function;
        std::ops::ControlFlow::Continue(())
    }
}

// ============================================================================
// Batch 1: Structural ClassDef checks
// ============================================================================

fn check_class_structural(class: &ClassDef, def: &StoredDefinition, diags: &mut Vec<Diagnostic>) {
    check_duplicate_names(class, diags);
    check_record_restrictions(class, diags);
    check_connector_restrictions(class, diags);
    check_input_parameter_combination(class, diags);
    check_component_name_vs_class_name(class, diags);
    check_package_restrictions(class, diags);
    check_duplicate_imports(class, diags);
    check_function_restrictions(class, diags);
    check_cross_class_restrictions(class, def, diags);
    check_cyclic_parameter_bindings(class, diags);
    check_parameter_variability(class, diags);
}

/// DECL-001: Duplicate variable names within the same class scope.
fn check_duplicate_names(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut seen = std::collections::HashSet::new();
    for (name, comp) in &class.components {
        if !seen.insert(name.as_str()) {
            diags.push(semantic_error(
                ER032_DUPLICATE_COMPONENT_NAME,
                format!(
                    "duplicate component name '{}' in {} '{}'",
                    name,
                    class.class_type.as_str(),
                    class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "check_duplicate_names/duplicate_component",
                    format!("duplicate component '{}'", name),
                ),
            ));
        } else if class.classes.contains_key(name) {
            diags.push(semantic_error(
                ER033_COMPONENT_CLASS_NAME_CONFLICT,
                format!(
                    "component '{}' conflicts with nested class name in {} '{}'",
                    name,
                    class.class_type.as_str(),
                    class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "check_duplicate_names/component_class_conflict",
                    format!("component '{}' conflicts with nested class", name),
                ),
            ));
        }
    }
}

/// DECL-003: Records cannot have protected sections.
/// DECL-004: Record elements cannot have flow/stream/input/output prefixes.
fn check_record_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type != ClassType::Record {
        return;
    }

    for (name, comp) in &class.components {
        // DECL-003
        if comp.is_protected {
            diags.push(semantic_error(
                ER021_RECORD_PROTECTED_ELEMENT,
                format!(
                    "record '{}' cannot have protected element '{}' (MLS §4.7)",
                    class.name.text, name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_record_restrictions/protected_component",
                    format!("protected record element '{}'", name),
                ),
            ));
        }

        // DECL-004: flow/stream
        match &comp.connection {
            Connection::Flow(token) => {
                diags.push(semantic_error(
                    ER022_RECORD_INVALID_PREFIX,
                    format!(
                        "record element '{}' cannot have 'flow' prefix (MLS §4.7)",
                        name
                    ),
                    label_from_token(
                        token,
                        "check_record_restrictions/flow_prefix",
                        "invalid 'flow' prefix on record element",
                    ),
                ));
            }
            Connection::Stream(token) => {
                diags.push(semantic_error(
                    ER022_RECORD_INVALID_PREFIX,
                    format!(
                        "record element '{}' cannot have 'stream' prefix (MLS §4.7)",
                        name
                    ),
                    label_from_token(
                        token,
                        "check_record_restrictions/stream_prefix",
                        "invalid 'stream' prefix on record element",
                    ),
                ));
            }
            Connection::Empty => {}
        }

        // DECL-004: input/output
        match &comp.causality {
            Causality::Input(token) => {
                diags.push(semantic_error(
                    ER022_RECORD_INVALID_PREFIX,
                    format!(
                        "record element '{}' cannot have 'input' prefix (MLS §4.7)",
                        name
                    ),
                    label_from_token(
                        token,
                        "check_record_restrictions/input_prefix",
                        "invalid 'input' prefix on record element",
                    ),
                ));
            }
            Causality::Output(token) => {
                diags.push(semantic_error(
                    ER022_RECORD_INVALID_PREFIX,
                    format!(
                        "record element '{}' cannot have 'output' prefix (MLS §4.7)",
                        name
                    ),
                    label_from_token(
                        token,
                        "check_record_restrictions/output_prefix",
                        "invalid 'output' prefix on record element",
                    ),
                ));
            }
            Causality::Empty => {}
        }
    }

    for (name, nested) in &class.classes {
        if nested.is_protected {
            diags.push(semantic_error(
                ER021_RECORD_PROTECTED_ELEMENT,
                format!(
                    "record '{}' cannot have protected element '{}' (MLS §4.7)",
                    class.name.text, name
                ),
                label_from_token(
                    &nested.name,
                    "check_record_restrictions/protected_nested_class",
                    format!("protected nested element '{}'", name),
                ),
            ));
        }
    }
}

/// DECL-006: Connectors cannot have protected sections.
/// DECL-007: Connector elements cannot have inner/outer prefixes.
fn check_connector_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type != ClassType::Connector {
        return;
    }

    for (name, comp) in &class.components {
        if comp.is_protected {
            diags.push(semantic_error(
                ER034_CONNECTOR_PROTECTED_ELEMENT,
                format!(
                    "connector '{}' cannot have protected element '{}' (MLS §9.1)",
                    class.name.text, name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_connector_restrictions/protected_component",
                    format!("protected connector element '{}'", name),
                ),
            ));
        }
        if comp.inner {
            diags.push(semantic_error(
                ER024_CONNECTOR_INNER_OUTER_PREFIX,
                format!(
                    "connector element '{}' cannot have 'inner' prefix (MLS §9.1)",
                    name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_connector_restrictions/inner_prefix",
                    "invalid 'inner' prefix on connector element",
                ),
            ));
        }
        if comp.outer {
            diags.push(semantic_error(
                ER024_CONNECTOR_INNER_OUTER_PREFIX,
                format!(
                    "connector element '{}' cannot have 'outer' prefix (MLS §9.1)",
                    name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_connector_restrictions/outer_prefix",
                    "invalid 'outer' prefix on connector element",
                ),
            ));
        }
    }

    for (name, nested) in &class.classes {
        if nested.is_protected {
            diags.push(semantic_error(
                ER034_CONNECTOR_PROTECTED_ELEMENT,
                format!(
                    "connector '{}' cannot have protected element '{}' (MLS §9.1)",
                    class.name.text, name
                ),
                label_from_token(
                    &nested.name,
                    "check_connector_restrictions/protected_nested_class",
                    format!("protected nested connector element '{}'", name),
                ),
            ));
        }
    }
}

/// DECL-012: Input prefix combined with parameter/constant is forbidden.
fn check_input_parameter_combination(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        let Causality::Input(input_token) = &comp.causality else {
            continue;
        };
        let var_str = match &comp.variability {
            Variability::Parameter(_) => "parameter",
            Variability::Constant(_) => "constant",
            _ => continue,
        };
        diags.push(semantic_error(
            ER035_INPUT_PARAMETER_COMBINATION,
            format!(
                "variable '{}' cannot combine 'input' with '{}' prefix (MLS §4.4.2.2)",
                name, var_str
            ),
            label_from_token(
                input_token,
                "check_input_parameter_combination/input_prefix",
                format!("'input' combined with '{}'", var_str),
            ),
        ));
    }
}

/// DECL-015: Component name cannot be the same as the enclosing class name.
/// Only flags when the component's type also matches the class name, creating
/// true lookup ambiguity (e.g., `model Real { Real Real; }`).
/// Functions and cases where the component type differs (e.g., MSL example models
/// like `model Adder4 { FullAdder Adder4; }`) are not flagged.
fn check_component_name_vs_class_name(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type == ClassType::Function {
        return;
    }
    for (name, comp) in &class.components {
        if name.as_str() == &*class.name.text
            && comp.type_name.to_string().as_str() == &*class.name.text
        {
            diags.push(semantic_error(
                ER036_COMPONENT_NAME_EQUALS_CLASS_NAME,
                format!(
                    "component '{}' has the same name as its enclosing class (MLS §5.3)",
                    name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_component_name_vs_class_name/component_name",
                    "component name conflicts with enclosing class name",
                ),
            ));
        }
    }
}

/// DECL-024: Package may only contain classes and constants.
fn check_package_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type != ClassType::Package {
        return;
    }
    for (name, comp) in &class.components {
        if !matches!(comp.variability, Variability::Constant(_)) {
            diags.push(semantic_error(
                ER037_PACKAGE_NON_CONSTANT_COMPONENT,
                format!(
                    "package '{}' can only contain classes and constants, \
                     but '{}' is not constant (MLS §4.7)",
                    class.name.text, name
                ),
                label_from_token(
                    &comp.name_token,
                    "check_package_restrictions/non_constant_component",
                    format!("'{}' is not constant", name),
                ),
            ));
        }
    }
}

/// PKG-001: Duplicate import names.
fn check_duplicate_imports(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut import_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for imp in &class.imports {
        let imported_name_and_token = match imp {
            Import::Qualified { path, .. } => {
                path.name.last().map(|t| (t.text.to_string(), t.clone()))
            }
            Import::Renamed { alias, .. } => Some((alias.text.to_string(), alias.clone())),
            Import::Unqualified { .. } => None,
            Import::Selective { names, .. } => {
                check_selective_import_dupes(class, names, &mut import_names, diags);
                None
            }
        };
        if let Some((name, token)) = imported_name_and_token
            && !import_names.insert(name.clone())
        {
            diags.push(semantic_error(
                ER012_DUPLICATE_IMPORT_NAME,
                format!(
                    "duplicate import name '{}' in {} '{}' (MLS §13.2.1)",
                    name,
                    class.class_type.as_str(),
                    class.name.text
                ),
                label_from_token(
                    &token,
                    "check_duplicate_imports/duplicate_import",
                    format!("duplicate import alias '{}'", name),
                ),
            ));
        }
    }
}

fn check_selective_import_dupes(
    class: &ClassDef,
    names: &[rumoca_ir_core::Token],
    import_names: &mut HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    for name_tok in names {
        let n = name_tok.text.to_string();
        if !import_names.insert(n.clone()) {
            diags.push(semantic_error(
                ER012_DUPLICATE_IMPORT_NAME,
                format!(
                    "duplicate import name '{}' in {} '{}' (MLS §13.2.1)",
                    n,
                    class.class_type.as_str(),
                    class.name.text
                ),
                label_from_token(
                    name_tok,
                    "check_selective_import_dupes/duplicate_import",
                    format!("duplicate import alias '{}'", n),
                ),
            ));
        }
    }
}

/// FUNC-001: Public function components must have input or output prefix.
/// FUNC-002: Assignment to function input is forbidden.
/// FUNC-006: Functions cannot have equation sections.
fn check_function_restrictions(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type != ClassType::Function {
        return;
    }

    // FUNC-001
    for (name, comp) in &class.components {
        if !comp.is_protected && matches!(comp.causality, Causality::Empty) {
            diags.push(semantic_error(
                ER013_FUNCTION_PUBLIC_MISSING_IO_PREFIX,
                format!(
                    "public component '{}' in function '{}' must have \
                     input or output prefix (MLS §12.2)",
                    name, class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "check_function_restrictions/missing_io_prefix",
                    format!("component '{}' is missing input/output prefix", name),
                ),
            ));
        }
    }

    // FUNC-006
    if !class.equations.is_empty() || !class.initial_equations.is_empty() {
        let label = if let Some(eq) = class.equations.first() {
            label_from_equation(
                eq,
                "check_function_restrictions/function_equations",
                "equation section is not allowed in functions",
            )
            .unwrap_or_else(|| {
                label_from_token(
                    &class.name,
                    "check_function_restrictions/function_equations_fallback",
                    "equation section is not allowed in functions",
                )
            })
        } else if let Some(eq) = class.initial_equations.first() {
            label_from_equation(
                eq,
                "check_function_restrictions/function_initial_equations",
                "initial equation section is not allowed in functions",
            )
            .unwrap_or_else(|| {
                label_from_token(
                    &class.name,
                    "check_function_restrictions/function_initial_equations_fallback",
                    "initial equation section is not allowed in functions",
                )
            })
        } else {
            label_from_token(
                &class.name,
                "check_function_restrictions/function_equation_section",
                "equation section is not allowed in functions",
            )
        };
        diags.push(semantic_error(
            ER038_FUNCTION_EQUATION_SECTION,
            format!(
                "function '{}' cannot have equation sections (MLS §12.2)",
                class.name.text
            ),
            label,
        ));
    }

    // FUNC-002
    let input_names: std::collections::HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Input(_)))
        .map(|(n, _)| n.clone())
        .collect();

    for alg in &class.algorithms {
        check_input_assignment(alg, &input_names, diags);
    }
}

fn check_input_assignment(
    stmts: &[Statement],
    input_names: &std::collections::HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    struct InputAssignmentVisitor<'a> {
        input_names: &'a std::collections::HashSet<String>,
        diags: &'a mut Vec<Diagnostic>,
    }

    impl ast::Visitor for InputAssignmentVisitor<'_> {
        fn visit_assignment(
            &mut self,
            comp: &ComponentReference,
            _value: &Expression,
        ) -> std::ops::ControlFlow<()> {
            if let Some(first) = comp.parts.first()
                && self.input_names.contains(&*first.ident.text)
            {
                self.diags.push(semantic_error(
                    ER014_FUNCTION_INPUT_ASSIGNED,
                    format!(
                        "cannot assign to input parameter '{}' (MLS §12.2)",
                        first.ident.text
                    ),
                    label_from_token(
                        &first.ident,
                        "check_input_assignment/input_assignment",
                        format!("assignment to input '{}'", first.ident.text),
                    ),
                ));
            }
            std::ops::ControlFlow::Continue(())
        }
    }

    let mut visitor = InputAssignmentVisitor { input_names, diags };
    for stmt in stmts {
        let _ = visitor.visit_statement(stmt);
    }
}

// ============================================================================
// Cross-class checks (need access to full StoredDefinition)
// ============================================================================

/// Look up a class by type name in the StoredDefinition.
fn find_class_by_name<'a>(def: &'a StoredDefinition, type_name: &str) -> Option<&'a ClassDef> {
    // Try direct lookup
    if let Some(cls) = def.classes.get(type_name) {
        return Some(cls);
    }
    // Try nested lookup (one level)
    for parent in def.classes.values() {
        if let Some(cls) = parent.classes.get(type_name) {
            return Some(cls);
        }
    }
    None
}

/// Cross-class checks that need to look up type classes.
fn check_cross_class_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        let type_name = comp.type_name.to_string();
        let type_class = find_class_by_name(def, &type_name);

        check_record_component_type_restriction(class, name, comp, &type_name, type_class, diags);
        check_partial_class_instantiation_restriction(
            class, name, comp, &type_name, type_class, diags,
        );
        check_connector_variability_restriction(name, comp, type_class, diags);
    }

    // CONN-017: Check connector balance when defining the connector itself
    if class.class_type == ClassType::Connector && !class.partial && !class.expandable {
        check_connector_balance(class, diags);
    }

    // DECL-002: Block connector components need input/output prefix
    check_block_connector_causality_restrictions(class, def, diags);
}

fn check_record_component_type_restriction(
    class: &ClassDef,
    component_name: &str,
    comp: &ast::Component,
    type_name: &str,
    type_class: Option<&ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    if class.class_type != ClassType::Record {
        return;
    }
    let Some(tc) = type_class else {
        return;
    };
    if matches!(tc.class_type, ClassType::Record | ClassType::Type) {
        return;
    }

    diags.push(semantic_error(
        ER023_RECORD_INVALID_COMPONENT_TYPE,
        format!(
            "record component '{}' has type '{}' which is a {}, \
                     but only record or type components are allowed (MLS §4.7)",
            component_name,
            type_name,
            tc.class_type.as_str()
        ),
        label_from_token(
            &comp.name_token,
            "check_cross_class_restrictions/record_component_type",
            format!(
                "record component '{}' has invalid type '{}'",
                component_name, type_name
            ),
        ),
    ));
}

fn check_partial_class_instantiation_restriction(
    class: &ClassDef,
    component_name: &str,
    comp: &ast::Component,
    type_name: &str,
    type_class: Option<&ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    if !matches!(class.class_type, ClassType::Model | ClassType::Block) {
        return;
    }
    let Some(tc) = type_class else {
        return;
    };
    if !tc.partial {
        return;
    }

    diags.push(semantic_error(
        ER005_PARTIAL_CLASS_INSTANTIATION,
        format!(
            "component '{}' instantiates partial {} '{}' (MLS §4.7)",
            component_name,
            tc.class_type.as_str(),
            type_name
        ),
        label_from_token(
            &comp.name_token,
            "check_cross_class_restrictions/partial_instantiation",
            format!("instantiation of partial class '{}'", type_name),
        ),
    ));
}

fn check_connector_variability_restriction(
    component_name: &str,
    comp: &ast::Component,
    type_class: Option<&ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    let Some(tc) = type_class else {
        return;
    };
    if tc.class_type != ClassType::Connector {
        return;
    }
    if !matches!(
        comp.variability,
        Variability::Parameter(_) | Variability::Constant(_)
    ) {
        return;
    }

    let var_str = match &comp.variability {
        Variability::Parameter(_) => "parameter",
        Variability::Constant(_) => "constant",
        _ => unreachable!(),
    };
    let prefix_label = match &comp.variability {
        Variability::Parameter(token) => label_from_token(
            token,
            "check_cross_class_restrictions/connector_parameter",
            "invalid 'parameter' prefix on connector component",
        ),
        Variability::Constant(token) => label_from_token(
            token,
            "check_cross_class_restrictions/connector_constant",
            "invalid 'constant' prefix on connector component",
        ),
        _ => unreachable!(),
    };
    diags.push(semantic_error(
        ER027_CONNECTOR_PARAMETER_OR_CONSTANT,
        format!(
            "connector component '{}' cannot have '{}' prefix (MLS §9.1)",
            component_name, var_str
        ),
        prefix_label,
    ));
}

fn check_block_connector_causality_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if class.class_type != ClassType::Block {
        return;
    }

    for (name, comp) in &class.components {
        let type_name = comp.type_name.to_string();
        if let Some(tc) = find_class_by_name(def, &type_name)
            && tc.class_type == ClassType::Connector
                && !comp.is_protected
                && matches!(comp.causality, Causality::Empty)
                // Also check if the type alias itself provides causality
                && matches!(tc.causality, Causality::Empty)
        {
            diags.push(semantic_error(
                ER020_BLOCK_CONNECTOR_MISSING_IO_PREFIX,
                format!(
                    "public connector component '{}' in block '{}' must \
                         have input or output prefix (MLS §4.7)",
                    name, class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "check_cross_class_restrictions/block_connector_causality",
                    format!("connector component '{}' is missing input/output", name),
                ),
            ));
        }
    }
}

/// CONN-017: Check flow/potential balance in connector.
fn check_connector_balance(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut flow_count = 0usize;
    let mut potential_count = 0usize;

    for (_, comp) in &class.components {
        // Only count Real-typed components for balance.
        // Non-Real types (Integer, Boolean, String) are not physical variables.
        // Non-primitive types (records, models) expand to multiple scalars
        // which we can't accurately count without type info.
        let type_name = comp.type_name.to_string();
        if type_name != "Real" {
            if !matches!(type_name.as_str(), "Integer" | "Boolean" | "String") {
                return; // Skip balance check for connectors with non-primitive members
            }
            continue; // Skip non-Real primitives for counting
        }
        match &comp.connection {
            Connection::Flow(_) => flow_count += 1,
            Connection::Stream(_) => {} // stream doesn't count
            Connection::Empty => potential_count += 1,
        }
    }

    if flow_count > 0 && flow_count != potential_count {
        diags.push(semantic_error(
            ER028_UNBALANCED_CONNECTOR,
            format!(
                "connector '{}' is unbalanced: {} flow variable(s) vs {} \
                 potential variable(s) (MLS §9.3.1)",
                class.name.text, flow_count, potential_count
            ),
            label_from_token(
                &class.name,
                "check_connector_balance/unbalanced_connector",
                "connector is structurally unbalanced",
            ),
        ));
    }
}

/// EXPR-012: Check that parameter/constant bindings don't reference continuous variables.
fn check_parameter_variability(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    // Skip functions: function inputs/outputs have different variability semantics
    if class.class_type == ClassType::Function {
        return;
    }

    // Collect continuous variables: only Real-typed with no variability prefix and no
    // input/output causality. Input/output variables are determined externally and their
    // structural properties (like array size) are valid in parameter bindings.
    let continuous_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            matches!(c.variability, Variability::Empty)
                && c.type_name.to_string() == "Real"
                && matches!(c.causality, Causality::Empty)
        })
        .map(|(n, _)| n.clone())
        .collect();

    if continuous_vars.is_empty() {
        return;
    }

    for (name, comp) in &class.components {
        if !matches!(
            comp.variability,
            Variability::Parameter(_) | Variability::Constant(_)
        ) {
            continue;
        }
        let Some(binding) = &comp.binding else {
            continue;
        };
        let mut refs = HashSet::new();
        collect_component_refs(binding, &continuous_vars, &mut refs, false);
        let Some(dep) = refs.into_iter().next() else {
            continue;
        };
        let var_str = match &comp.variability {
            Variability::Parameter(_) => "parameter",
            Variability::Constant(_) => "constant",
            _ => unreachable!(),
        };
        let label = label_from_expression_or_token(
            binding,
            "check_parameter_variability/binding_dependency",
            &comp.name_token,
            "check_parameter_variability/binding_dependency_fallback",
            format!("binding references continuous variable '{}'", dep),
        );
        diags.push(semantic_error(
            ER006_PARAMETER_VARIABILITY,
            format!(
                "{} '{}' cannot depend on continuous variable '{}' (MLS §3.8.4)",
                var_str, name, dep
            ),
            label,
        ));
    }
}

/// INST-008: Detect cyclic parameter bindings.
fn check_cyclic_parameter_bindings(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    use std::collections::HashMap;

    // Build dependency graph: parameter name -> set of parameter names referenced in binding
    let param_names: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            matches!(
                c.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
        .map(|(n, _)| n.clone())
        .collect();

    if param_names.is_empty() {
        return;
    }

    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for (name, comp) in &class.components {
        if !param_names.contains(name) {
            continue;
        }
        let mut refs = HashSet::new();
        if let Some(binding) = &comp.binding {
            // Skip if-branches to avoid false cycles from conditional mutual deps
            collect_component_refs(binding, &param_names, &mut refs, true);
        }
        deps.insert(name.clone(), refs);
    }

    // DFS cycle detection
    let mut visited = HashSet::new();
    let mut on_stack = HashSet::new();

    for name in param_names {
        if !visited.contains(&name) && has_cycle(&name, &deps, &mut visited, &mut on_stack) {
            let Some(comp) = class.components.get(&name) else {
                continue;
            };
            let label = if let Some(binding) = &comp.binding {
                label_from_expression_or_token(
                    binding,
                    "check_cyclic_parameter_bindings/binding_cycle",
                    &comp.name_token,
                    "check_cyclic_parameter_bindings/binding_cycle_fallback",
                    format!("cyclic dependency for '{}'", name),
                )
            } else {
                label_from_token(
                    &comp.name_token,
                    "check_cyclic_parameter_bindings/component_cycle",
                    format!("cyclic dependency for '{}'", name),
                )
            };
            diags.push(semantic_error(
                ER007_CYCLIC_PARAMETER_BINDING,
                format!(
                    "cyclic dependency in parameter binding for '{}' (MLS §7.2.3)",
                    name
                ),
                label,
            ));
        }
    }
}

fn has_cycle(
    node: &str,
    deps: &std::collections::HashMap<String, HashSet<String>>,
    visited: &mut HashSet<String>,
    on_stack: &mut HashSet<String>,
) -> bool {
    visited.insert(node.to_string());
    on_stack.insert(node.to_string());

    let found_cycle = deps.get(node).is_some_and(|neighbors| {
        neighbors.iter().any(|neighbor| {
            if !visited.contains(neighbor) {
                has_cycle(neighbor, deps, visited, on_stack)
            } else {
                on_stack.contains(neighbor)
            }
        })
    });

    if !found_cycle {
        on_stack.remove(node);
    }
    found_cycle
}

/// Collect component references from an expression that refer to known parameter names.
/// Only matches single-part references (e.g., `x`) not multi-part (e.g., `system.x`),
/// since multi-part references access sub-components rather than the parameter itself.
/// When `skip_if_branches` is true, only collects from if-conditions (not branches),
/// avoiding false positive cycles from conditional mutual dependencies.
fn collect_component_refs(
    expr: &Expression,
    known_params: &HashSet<String>,
    refs: &mut HashSet<String>,
    skip_if_branches: bool,
) {
    struct ComponentRefCollector<'a> {
        known_params: &'a HashSet<String>,
        refs: &'a mut HashSet<String>,
        skip_if_branches: bool,
    }

    impl ast::Visitor for ComponentRefCollector<'_> {
        fn visit_component_reference_ctx(
            &mut self,
            cref: &ComponentReference,
            ctx: ast::ComponentReferenceContext,
        ) -> std::ops::ControlFlow<()> {
            if !matches!(ctx, ast::ComponentReferenceContext::Expression) {
                return ast::Visitor::visit_component_reference(self, cref);
            }
            // Only match single-part references for direct dependencies.
            // Multi-part refs like `system.x` access sub-components, not the param itself.
            let [part] = cref.parts.as_slice() else {
                return ast::Visitor::visit_component_reference(self, cref);
            };
            let name = part.ident.text.to_string();
            if self.known_params.contains(&name) {
                self.refs.insert(name);
            }
            ast::Visitor::visit_component_reference(self, cref)
        }

        fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
            if !self.skip_if_branches {
                return walk_expression_default(self, expr);
            }
            let Expression::If { branches, .. } = expr else {
                return walk_expression_default(self, expr);
            };
            for (cond, _) in branches {
                self.visit_expression(cond)?;
            }
            std::ops::ControlFlow::Continue(())
        }
    }

    let mut collector = ComponentRefCollector {
        known_params,
        refs,
        skip_if_branches,
    };
    let _ = collector.visit_expression(expr);
}

// ============================================================================
// Batch 2: Context-sensitive checks
// ============================================================================

/// Check equations for context-sensitive issues.
fn check_equation(eq: &Equation, ctx: &mut CheckContext, diags: &mut Vec<Diagnostic>) {
    let mut visitor = ContextSensitiveVisitor { ctx, diags };
    let _ = visitor.visit_equation(eq);
}

/// Check statements for context-sensitive issues.
fn check_statement(stmt: &Statement, ctx: &mut CheckContext, diags: &mut Vec<Diagnostic>) {
    let mut visitor = ContextSensitiveVisitor { ctx, diags };
    let _ = visitor.visit_statement(stmt);
}

struct ContextSensitiveVisitor<'a> {
    ctx: &'a mut CheckContext,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for ContextSensitiveVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        if let Equation::When(_) = eq
            && self.ctx.in_when_equation
            && let Some(label) = label_from_equation(
                eq,
                "check_equation/nested_when_equation",
                "nested when-equation is not allowed",
            )
        {
            self.diags.push(semantic_error(
                ER017_NESTED_WHEN_EQUATION,
                "when-equations cannot be nested (MLS §8.3.5)",
                label,
            ));
        }
        if let Equation::FunctionCall { comp, .. } = eq
            && let Some(first) = comp.parts.first()
            && &*first.ident.text == "reinit"
            && !self.ctx.in_when_equation
        {
            self.diags.push(semantic_error(
                ER008_REINIT_OUTSIDE_WHEN,
                "reinit() can only be used inside when-equations (MLS §8.3.6)",
                label_from_token(
                    &first.ident,
                    "check_equation/reinit_outside_when_equation",
                    "reinit() used outside when-equation",
                ),
            ));
        }
        walk_equation_default(self, eq)
    }

    fn visit_when_equation(&mut self, blocks: &[ast::EquationBlock]) -> std::ops::ControlFlow<()> {
        let was = self.ctx.in_when_equation;
        self.ctx.in_when_equation = true;
        self.visit_each(blocks, Self::visit_equation_block)?;
        self.ctx.in_when_equation = was;
        std::ops::ControlFlow::Continue(())
    }

    fn visit_for_equation(
        &mut self,
        indices: &[ast::ForIndex],
        equations: &[Equation],
    ) -> std::ops::ControlFlow<()> {
        let new_vars: Vec<String> = indices.iter().map(|i| i.ident.text.to_string()).collect();
        self.ctx.for_loop_vars.extend(new_vars.clone());

        for inner_eq in equations {
            check_for_variable_assignment_eq(inner_eq, &self.ctx.for_loop_vars, self.diags);
            self.visit_equation(inner_eq)?;
        }

        for var in &new_vars {
            self.ctx.for_loop_vars.retain(|v| v != var);
        }
        std::ops::ControlFlow::Continue(())
    }

    fn visit_statement(&mut self, stmt: &Statement) -> std::ops::ControlFlow<()> {
        if let Statement::When(_) = stmt
            && self.ctx.in_function
            && let Some(label) = label_from_statement(
                stmt,
                "check_statement/when_in_function",
                "when-statement is not allowed in function",
            )
        {
            self.diags.push(semantic_error(
                ER015_WHEN_IN_FUNCTION,
                "when-statements are not allowed in functions (MLS §12.2)",
                label,
            ));
        }
        if let Statement::When(_) = stmt
            && self.ctx.in_when_statement
            && let Some(label) = label_from_statement(
                stmt,
                "check_statement/nested_when_statement",
                "nested when-statement is not allowed",
            )
        {
            self.diags.push(semantic_error(
                ER016_NESTED_WHEN_STATEMENT,
                "when-statements cannot be nested (MLS §11.2.7)",
                label,
            ));
        }
        if matches!(stmt, Statement::Reinit { .. })
            && !self.ctx.in_when_statement
            && let Some(label) = label_from_statement(
                stmt,
                "check_statement/reinit_outside_when_statement",
                "reinit() used outside when-statement",
            )
        {
            self.diags.push(semantic_error(
                ER008_REINIT_OUTSIDE_WHEN,
                "reinit() can only be used inside when-statements (MLS §8.3.6)",
                label,
            ));
        }
        walk_statement_default(self, stmt)
    }

    fn visit_when_statement(
        &mut self,
        blocks: &[ast::StatementBlock],
    ) -> std::ops::ControlFlow<()> {
        let was = self.ctx.in_when_statement;
        self.ctx.in_when_statement = true;
        self.visit_each(blocks, Self::visit_statement_block)?;
        self.ctx.in_when_statement = was;
        std::ops::ControlFlow::Continue(())
    }
}

/// Check initial equations for when-clause presence (EQN-006, EQN-037).
fn check_initial_equation(eq: &Equation, diags: &mut Vec<Diagnostic>) {
    let mut visitor = InitialEquationVisitor { diags };
    let _ = visitor.visit_equation(eq);
}

/// Check initial algorithm statements for when-clause presence (EQN-037).
fn check_initial_statement(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    let mut visitor = InitialStatementVisitor { diags };
    let _ = visitor.visit_statement(stmt);
}

struct InitialEquationVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for InitialEquationVisitor<'_> {
    fn visit_equation(&mut self, eq: &Equation) -> std::ops::ControlFlow<()> {
        if let Equation::When(_) = eq
            && let Some(label) = label_from_equation(
                eq,
                "check_initial_equation/when_in_initial_equation",
                "when-equation in initial equation section",
            )
        {
            self.diags.push(semantic_error(
                ER018_WHEN_IN_INITIAL_SECTION,
                "when-equations are not allowed in initial equation sections (MLS §8.6)",
                label,
            ));
            return std::ops::ControlFlow::Continue(());
        }
        walk_equation_default(self, eq)
    }
}

struct InitialStatementVisitor<'a> {
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for InitialStatementVisitor<'_> {
    fn visit_statement(&mut self, stmt: &Statement) -> std::ops::ControlFlow<()> {
        if let Statement::When(_) = stmt
            && let Some(label) = label_from_statement(
                stmt,
                "check_initial_statement/when_in_initial_algorithm",
                "when-statement in initial algorithm section",
            )
        {
            self.diags.push(semantic_error(
                ER018_WHEN_IN_INITIAL_SECTION,
                "when-statements are not allowed in initial algorithm sections (MLS §8.6)",
                label,
            ));
            return std::ops::ControlFlow::Continue(());
        }
        walk_statement_default(self, stmt)
    }
}

/// Check for-loop variable assignment in equations (EQN-010).
fn check_for_variable_assignment_eq(
    eq: &Equation,
    for_vars: &[String],
    diags: &mut Vec<Diagnostic>,
) {
    if let Equation::Simple { lhs, .. } = eq
        && let Expression::ComponentReference(comp) = lhs
        && let Some(first) = comp.parts.first()
        && for_vars.iter().any(|v| v.as_str() == &*first.ident.text)
    {
        diags.push(semantic_error(
            ER019_FOR_LOOP_VARIABLE_ASSIGNED,
            format!(
                "cannot assign to for-loop variable '{}' (MLS §8.3.3)",
                first.ident.text
            ),
            label_from_token(
                &first.ident,
                "check_for_variable_assignment_eq/for_loop_assignment",
                format!("assignment to loop variable '{}'", first.ident.text),
            ),
        ));
    }
}

// ============================================================================
// Batch 3: Expression checks
// ============================================================================

/// EXPR-014: Check for chained relational operators (e.g., 1 < 2 < 3).
pub fn check_chained_relationals(
    def: &StoredDefinition,
    source_map: &SourceMap,
) -> Vec<Diagnostic> {
    set_active_source_map(source_map);
    run_chained_relational_checks(def)
}

/// EXPR-004: Check for der() in function algorithm sections.
pub fn check_der_in_functions(def: &StoredDefinition, source_map: &SourceMap) -> Vec<Diagnostic> {
    set_active_source_map(source_map);
    run_der_in_function_checks(def)
}
