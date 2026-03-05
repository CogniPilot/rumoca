//! Semantic validation checks applied during the resolve phase.
//!
//! These checks enforce structural rules from the Modelica Language Specification
//! that can be validated purely from the AST without needing type information
//! or instance data.

use rumoca_core::{DefId, Diagnostic, Label, SourceMap};
use rumoca_ir_ast as ast;
use std::collections::{HashMap, HashSet};

type Causality = ast::Causality;
type ClassDef = ast::ClassDef;
type ClassType = ast::ClassType;
type ComponentReference = ast::ComponentReference;
type Connection = ast::Connection;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Import = ast::Import;
type OpBinary = ast::OpBinary;
type Statement = ast::Statement;
type Subscript = ast::Subscript;
type StoredDefinition = ast::StoredDefinition;
type TerminalType = ast::TerminalType;
type Variability = ast::Variability;

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
const ER032_FUNCTION_OUTPUT_NOT_ASSIGNED: &str = "ER032";
const ER033_NAMED_ARGUMENT_SLOT_FILLED: &str = "ER033";
const ER034_FUNCTION_CALL_MISSING_INPUT: &str = "ER034";
const ER035_FUNCTION_INNER_OUTER_FORBIDDEN: &str = "ER035";
const ER036_FUNCTION_CLOCK_COMPONENT_FORBIDDEN: &str = "ER036";
const ER037_PARTIAL_FUNCTION_CALL_FORBIDDEN: &str = "ER037";
const ER038_FUNCTION_INVALID_COMPONENT_TYPE: &str = "ER038";
const ER039_FUNCTION_FORBIDDEN_OPERATOR: &str = "ER039";
const ER040_IMPURE_INHERITANCE_REQUIRES_IMPURE: &str = "ER040";
const ER041_IMPURE_CALL_SCOPE_VIOLATION: &str = "ER041";
const ER042_FUNCTION_BINDING_CYCLE: &str = "ER042";
const ER043_FUNCTION_UNINITIALIZED_USE: &str = "ER043";
const ER044_FUNCTION_ARRAY_DIMENSION_SOURCE: &str = "ER044";
const ER045_FUNCTION_VECTORIZATION_REPLACEABLE: &str = "ER045";
const ER046_FUNCTION_VECTORIZATION_SIZE_MISMATCH: &str = "ER046";
const ER047_FUNCTION_OUTPUT_TARGET_MISMATCH: &str = "ER047";
const ER048_RECORD_CONSTRUCTOR_NON_GLOBAL: &str = "ER048";
const ER049_RECORD_CONSTRUCTOR_CONDITIONAL_COMPONENT: &str = "ER049";
const ER050_DERIVATIVE_OUTPUTS_EMPTY: &str = "ER050";
const ER051_ZERODERIVATIVE_INVALID_TARGET: &str = "ER051";
const ER052_EXTERNAL_PURITY_DEPRECATED: &str = "ER052";
const ER053_FUNCTIONAL_PARAM_RECORD_ENUM: &str = "ER053";
const ER054_INPUT_DEFAULT_DEPENDS_ON_NON_INPUT: &str = "ER054";
const ER055_DERIVATIVE_ORDERING: &str = "ER055";

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

/// Run all semantic checks on a StoredDefinition and collect diagnostics.
pub fn check_semantics(def: &StoredDefinition, source_map: &SourceMap) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let mut ctx = CheckContext::new();
    let class_index = build_class_def_index(def);
    let top_level_class_ids = build_top_level_class_ids(def);
    for class in def.classes.values() {
        check_class(
            class,
            def,
            source_map,
            &class_index,
            &top_level_class_ids,
            &mut ctx,
            &mut diags,
        );
    }
    diags
}

fn build_class_def_index(def: &StoredDefinition) -> HashMap<DefId, &ClassDef> {
    let mut index = HashMap::new();
    for class in def.classes.values() {
        index_class_def_recursive(class, &mut index);
    }
    index
}

fn build_top_level_class_ids(def: &StoredDefinition) -> HashSet<DefId> {
    def.classes
        .values()
        .filter_map(|class| class.def_id)
        .collect()
}

fn index_class_def_recursive<'a>(class: &'a ClassDef, index: &mut HashMap<DefId, &'a ClassDef>) {
    if let Some(def_id) = class.def_id {
        index.insert(def_id, class);
    }
    for nested in class.classes.values() {
        index_class_def_recursive(nested, index);
    }
}

fn check_class(
    class: &ClassDef,
    def: &StoredDefinition,
    source_map: &SourceMap,
    class_index: &HashMap<DefId, &ClassDef>,
    top_level_class_ids: &HashSet<DefId>,
    ctx: &mut CheckContext,
    diags: &mut Vec<Diagnostic>,
) {
    check_class_structural(class, def, source_map, class_index, diags);

    let was_function = ctx.in_function;
    if class.class_type == ClassType::Function {
        ctx.in_function = true;
    }

    // DECL-020: Check for der() on discrete variables
    let discrete_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.variability, Variability::Discrete(_)))
        .map(|(n, _)| n.clone())
        .collect();

    // Collect Real-typed variables for EXPR-002 check
    let real_vars: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| {
            let tn = c.type_name.to_string();
            tn == "Real"
        })
        .map(|(n, _)| n.clone())
        .collect();

    // Check equations
    for eq in &class.equations {
        check_equation(eq, ctx, diags);
        check_der_on_discrete_eq(eq, &discrete_vars, diags);
        check_protected_access_eq(eq, class, def, diags);
        check_connect_requires_connectors_eq(eq, class, def, diags);
        check_end_outside_subscript_eq(eq, diags);
        check_expr_type_issues_eq(eq, class, def, &real_vars, diags);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_initial_equation(eq, diags);
        check_equation(eq, ctx, diags);
        check_der_on_discrete_eq(eq, &discrete_vars, diags);
        check_end_outside_subscript_eq(eq, diags);
    }

    // Check initial algorithms
    for alg in &class.initial_algorithms {
        for stmt in alg {
            check_initial_statement(stmt, diags);
            check_statement(stmt, ctx, diags);
        }
    }

    // Check algorithm sections
    for alg in &class.algorithms {
        for stmt in alg {
            check_statement(stmt, ctx, diags);
        }
    }
    check_function_call_argument_contracts_in_class(class, class_index, top_level_class_ids, diags);
    check_impure_call_scope_in_class(class, class_index, diags);

    // Recurse into nested classes
    for nested in class.classes.values() {
        check_class(
            nested,
            def,
            source_map,
            class_index,
            top_level_class_ids,
            ctx,
            diags,
        );
    }

    ctx.in_function = was_function;
}

// ============================================================================
// Batch 1: Structural ClassDef checks
// ============================================================================

fn check_class_structural(
    class: &ClassDef,
    def: &StoredDefinition,
    source_map: &SourceMap,
    class_index: &HashMap<DefId, &ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    check_duplicate_names(class, diags);
    check_record_restrictions(class, diags);
    check_connector_restrictions(class, diags);
    check_input_parameter_combination(class, diags);
    check_component_name_vs_class_name(class, diags);
    check_package_restrictions(class, diags);
    check_duplicate_imports(class, source_map, diags);
    check_function_restrictions(class, class_index, diags);
    check_cross_class_restrictions(class, def, diags);
    check_cyclic_parameter_bindings(class, diags);
    check_parameter_variability(class, diags);
    check_function_binding_cycles(class, diags);
}

/// DECL-001: Duplicate variable names within the same class scope.
fn check_duplicate_names(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut seen = std::collections::HashSet::new();
    for name in class.components.keys() {
        if !seen.insert(name.as_str()) {
            diags.push(Diagnostic::error(format!(
                "duplicate component name '{}' in {} '{}'",
                name,
                class.class_type.as_str(),
                class.name.text
            )));
        } else if class.classes.contains_key(name) {
            diags.push(Diagnostic::error(format!(
                "component '{}' conflicts with nested class name in {} '{}'",
                name,
                class.class_type.as_str(),
                class.name.text
            )));
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
            diags.push(
                Diagnostic::error(format!(
                    "record '{}' cannot have protected element '{}' (MLS §4.7)",
                    class.name.text, name
                ))
                .with_code(ER021_RECORD_PROTECTED_ELEMENT),
            );
        }

        // DECL-004: flow/stream
        match &comp.connection {
            Connection::Flow(_) => {
                diags.push(
                    Diagnostic::error(format!(
                        "record element '{}' cannot have 'flow' prefix (MLS §4.7)",
                        name
                    ))
                    .with_code(ER022_RECORD_INVALID_PREFIX),
                );
            }
            Connection::Stream(_) => {
                diags.push(
                    Diagnostic::error(format!(
                        "record element '{}' cannot have 'stream' prefix (MLS §4.7)",
                        name
                    ))
                    .with_code(ER022_RECORD_INVALID_PREFIX),
                );
            }
            Connection::Empty => {}
        }

        // DECL-004: input/output
        match &comp.causality {
            Causality::Input(_) => {
                diags.push(
                    Diagnostic::error(format!(
                        "record element '{}' cannot have 'input' prefix (MLS §4.7)",
                        name
                    ))
                    .with_code(ER022_RECORD_INVALID_PREFIX),
                );
            }
            Causality::Output(_) => {
                diags.push(
                    Diagnostic::error(format!(
                        "record element '{}' cannot have 'output' prefix (MLS §4.7)",
                        name
                    ))
                    .with_code(ER022_RECORD_INVALID_PREFIX),
                );
            }
            Causality::Empty => {}
        }
    }

    for (name, nested) in &class.classes {
        if nested.is_protected {
            diags.push(
                Diagnostic::error(format!(
                    "record '{}' cannot have protected element '{}' (MLS §4.7)",
                    class.name.text, name
                ))
                .with_code(ER021_RECORD_PROTECTED_ELEMENT),
            );
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
            diags.push(Diagnostic::error(format!(
                "connector '{}' cannot have protected element '{}' (MLS §9.1)",
                class.name.text, name
            )));
        }
        if comp.inner {
            diags.push(
                Diagnostic::error(format!(
                    "connector element '{}' cannot have 'inner' prefix (MLS §9.1)",
                    name
                ))
                .with_code(ER024_CONNECTOR_INNER_OUTER_PREFIX),
            );
        }
        if comp.outer {
            diags.push(
                Diagnostic::error(format!(
                    "connector element '{}' cannot have 'outer' prefix (MLS §9.1)",
                    name
                ))
                .with_code(ER024_CONNECTOR_INNER_OUTER_PREFIX),
            );
        }
    }

    for (name, nested) in &class.classes {
        if nested.is_protected {
            diags.push(Diagnostic::error(format!(
                "connector '{}' cannot have protected element '{}' (MLS §9.1)",
                class.name.text, name
            )));
        }
    }
}

/// DECL-012: Input prefix combined with parameter/constant is forbidden.
fn check_input_parameter_combination(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        if !matches!(comp.causality, Causality::Input(_)) {
            continue;
        }
        let var_str = match &comp.variability {
            Variability::Parameter(_) => "parameter",
            Variability::Constant(_) => "constant",
            _ => continue,
        };
        diags.push(Diagnostic::error(format!(
            "variable '{}' cannot combine 'input' with '{}' prefix (MLS §4.4.2.2)",
            name, var_str
        )));
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
            diags.push(Diagnostic::error(format!(
                "component '{}' has the same name as its enclosing class (MLS §5.3)",
                name
            )));
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
            diags.push(Diagnostic::error(format!(
                "package '{}' can only contain classes and constants, \
                 but '{}' is not constant (MLS §4.7)",
                class.name.text, name
            )));
        }
    }
}

/// PKG-001: Duplicate import names.
fn check_duplicate_imports(class: &ClassDef, source_map: &SourceMap, diags: &mut Vec<Diagnostic>) {
    let mut import_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for imp in &class.imports {
        let imported_name_and_token = match imp {
            Import::Qualified { path, .. } => {
                path.name.last().map(|t| (t.text.to_string(), t.clone()))
            }
            Import::Renamed { alias, .. } => Some((alias.text.to_string(), alias.clone())),
            Import::Unqualified { .. } => None,
            Import::Selective { names, .. } => {
                check_selective_import_dupes(class, names, source_map, &mut import_names, diags);
                None
            }
        };
        if let Some((name, token)) = imported_name_and_token
            && !import_names.insert(name.clone())
        {
            diags.push(
                Diagnostic::error(format!(
                    "duplicate import name '{}' in {} '{}' (MLS §13.2.1)",
                    name,
                    class.class_type.as_str(),
                    class.name.text
                ))
                .with_code(ER012_DUPLICATE_IMPORT_NAME)
                .with_label(label_for_token(
                    &token,
                    source_map,
                    format!("duplicate import alias '{}'", name),
                )),
            );
        }
    }
}

fn check_selective_import_dupes(
    class: &ClassDef,
    names: &[rumoca_ir_ast::Token],
    source_map: &SourceMap,
    import_names: &mut HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    for name_tok in names {
        let n = name_tok.text.to_string();
        if !import_names.insert(n.clone()) {
            diags.push(
                Diagnostic::error(format!(
                    "duplicate import name '{}' in {} '{}' (MLS §13.2.1)",
                    n,
                    class.class_type.as_str(),
                    class.name.text
                ))
                .with_code(ER012_DUPLICATE_IMPORT_NAME)
                .with_label(label_for_token(
                    name_tok,
                    source_map,
                    format!("duplicate import alias '{}'", n),
                )),
            );
        }
    }
}

fn label_for_token(token: &ast::Token, source_map: &SourceMap, message: String) -> Label {
    let start = token.location.start as usize;
    let mut end = token.location.end as usize;
    if end <= start {
        end = start.saturating_add(token.text.len().max(1));
    }
    Label::primary(source_map.location_to_span(&token.location.file_name, start, end))
        .with_message(message)
}

/// FUNC-001: Public function components must have input or output prefix.
/// FUNC-002: Assignment to function input is forbidden.
/// FUNC-006: Functions cannot have equation sections.
/// FUNC-003: Function outputs must be assigned or function must be external.
/// FUNC-011: Functions may not declare Clock components.
/// FUNC-012: Function components cannot use inner/outer prefixes.
/// FUNC-015: Function components cannot be model/block/operator/connector types.
/// FUNC-010: Functions cannot use forbidden simulation operators.
/// FUNC-021: Extending an impure function requires impure declaration.
fn check_function_restrictions(
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    if class.class_type != ClassType::Function {
        return;
    }

    // FUNC-001
    for (name, comp) in &class.components {
        if !comp.is_protected && matches!(comp.causality, Causality::Empty) {
            diags.push(
                Diagnostic::error(format!(
                    "public component '{}' in function '{}' must have \
                     input or output prefix (MLS §12.2)",
                    name, class.name.text
                ))
                .with_code(ER013_FUNCTION_PUBLIC_MISSING_IO_PREFIX),
            );
        }
    }

    // FUNC-006
    if !class.equations.is_empty() || !class.initial_equations.is_empty() {
        diags.push(Diagnostic::error(format!(
            "function '{}' cannot have equation sections (MLS §12.2)",
            class.name.text
        )));
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

    for (name, comp) in &class.components {
        if comp.inner || comp.outer {
            diags.push(
                Diagnostic::error(format!(
                    "function element '{}' cannot use inner/outer prefixes (MLS §12.2)",
                    name
                ))
                .with_code(ER035_FUNCTION_INNER_OUTER_FORBIDDEN),
            );
        }

        if comp.type_name.to_string() == "Clock" {
            diags.push(
                Diagnostic::error(format!(
                    "function component '{}' cannot have type Clock (MLS §12.2)",
                    name
                ))
                .with_code(ER036_FUNCTION_CLOCK_COMPONENT_FORBIDDEN),
            );
        }

        let Some(type_def_id) = comp.type_def_id else {
            continue;
        };
        let Some(type_class) = class_index.get(&type_def_id) else {
            continue;
        };
        if matches!(
            type_class.class_type,
            ClassType::Model | ClassType::Block | ClassType::Operator | ClassType::Connector
        ) {
            diags.push(
                Diagnostic::error(format!(
                    "function component '{}' cannot have type '{}' (a {}) (MLS §12.2)",
                    name,
                    comp.type_name,
                    type_class.class_type.as_str()
                ))
                .with_code(ER038_FUNCTION_INVALID_COMPONENT_TYPE),
            );
        }
    }

    check_forbidden_operators_in_function(class, diags);
    check_impure_inheritance(class, class_index, diags);
    check_function_output_assignment(class, diags);
    check_function_uninitialized_use(class, diags);
    check_function_array_dimension_sources(class, diags);
    check_input_default_independence(class, diags);
    check_external_function_purity_deprecation(class, diags);
    check_functional_parameter_type_contract(class, class_index, diags);
    check_derivative_annotation_contracts(class, diags);
}

fn check_forbidden_operators_in_function(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for alg in &class.algorithms {
        for stmt in alg {
            check_forbidden_operators_in_statement(stmt, diags);
        }
    }
    for alg in &class.initial_algorithms {
        for stmt in alg {
            check_forbidden_operators_in_statement(stmt, diags);
        }
    }
}

fn check_forbidden_operators_in_statement(stmt: &Statement, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {}
        Statement::Assignment { value, .. } => {
            check_forbidden_operators_in_expr(value, diags);
        }
        Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            check_forbidden_operator_call(comp, diags);
            for arg in args {
                check_forbidden_operators_in_expr(arg, diags);
            }
            for output in outputs {
                check_forbidden_operators_in_expr(output, diags);
            }
        }
        Statement::For { indices, equations } => {
            for index in indices {
                check_forbidden_operators_in_expr(&index.range, diags);
            }
            for inner in equations {
                check_forbidden_operators_in_statement(inner, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_forbidden_operators_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_forbidden_operators_in_statement(inner, diags);
                }
            }
            if let Some(else_stmts) = else_block {
                for inner in else_stmts {
                    check_forbidden_operators_in_statement(inner, diags);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_forbidden_operators_in_expr(&block.cond, diags);
                for inner in &block.stmts {
                    check_forbidden_operators_in_statement(inner, diags);
                }
            }
        }
        Statement::While(block) => {
            check_forbidden_operators_in_expr(&block.cond, diags);
            for inner in &block.stmts {
                check_forbidden_operators_in_statement(inner, diags);
            }
        }
        Statement::Reinit { value, .. } => {
            diags.push(
                Diagnostic::error("reinit() is not allowed in functions (MLS §12.2)".to_string())
                    .with_code(ER039_FUNCTION_FORBIDDEN_OPERATOR),
            );
            check_forbidden_operators_in_expr(value, diags);
        }
        Statement::Assert {
            condition,
            message,
            level,
        } => {
            check_forbidden_operators_in_expr(condition, diags);
            check_forbidden_operators_in_expr(message, diags);
            if let Some(level_expr) = level {
                check_forbidden_operators_in_expr(level_expr, diags);
            }
        }
    }
}

fn check_forbidden_operators_in_expr(expr: &Expression, diags: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => {}
        Expression::Range { start, step, end } => {
            check_forbidden_operators_in_expr(start, diags);
            if let Some(step_expr) = step {
                check_forbidden_operators_in_expr(step_expr, diags);
            }
            check_forbidden_operators_in_expr(end, diags);
        }
        Expression::Unary { rhs, .. } => check_forbidden_operators_in_expr(rhs, diags),
        Expression::Binary { lhs, rhs, .. } => {
            check_forbidden_operators_in_expr(lhs, diags);
            check_forbidden_operators_in_expr(rhs, diags);
        }
        Expression::FunctionCall { comp, args } => {
            check_forbidden_operator_call(comp, diags);
            for arg in args {
                check_forbidden_operators_in_expr(arg, diags);
            }
        }
        Expression::ClassModification {
            target: _,
            modifications,
        } => {
            for mod_expr in modifications {
                check_forbidden_operators_in_expr(mod_expr, diags);
            }
        }
        Expression::NamedArgument { value, .. } | Expression::Modification { value, .. } => {
            check_forbidden_operators_in_expr(value, diags);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for elem in elements {
                check_forbidden_operators_in_expr(elem, diags);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_forbidden_operators_in_expr(cond, diags);
                check_forbidden_operators_in_expr(then_expr, diags);
            }
            check_forbidden_operators_in_expr(else_branch, diags);
        }
        Expression::Parenthesized { inner } => check_forbidden_operators_in_expr(inner, diags),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            check_forbidden_operators_in_expr(expr, diags);
            for index in indices {
                check_forbidden_operators_in_expr(&index.range, diags);
            }
            if let Some(filter_expr) = filter {
                check_forbidden_operators_in_expr(filter_expr, diags);
            }
        }
        Expression::ArrayIndex { base, subscripts } => {
            check_forbidden_operators_in_expr(base, diags);
            for sub in subscripts {
                if let Subscript::Expression(sub_expr) = sub {
                    check_forbidden_operators_in_expr(sub_expr, diags);
                }
            }
        }
        Expression::FieldAccess { base, .. } => check_forbidden_operators_in_expr(base, diags),
    }
}

fn check_forbidden_operator_call(comp: &ComponentReference, diags: &mut Vec<Diagnostic>) {
    let Some(first) = comp.parts.first() else {
        return;
    };
    let name = first.ident.text.as_ref();
    if !is_forbidden_function_operator(name) {
        return;
    }
    diags.push(
        Diagnostic::error(format!(
            "operator '{}' is not allowed in functions (MLS §12.2)",
            name
        ))
        .with_code(ER039_FUNCTION_FORBIDDEN_OPERATOR),
    );
}

fn is_forbidden_function_operator(name: &str) -> bool {
    matches!(
        name,
        "initial"
            | "terminal"
            | "sample"
            | "pre"
            | "edge"
            | "change"
            | "delay"
            | "cardinality"
            | "inStream"
            | "actualStream"
    )
}

fn check_impure_inheritance(
    class: &ClassDef,
    class_index: &HashMap<DefId, &ClassDef>,
    diags: &mut Vec<Diagnostic>,
) {
    if !class.pure {
        return;
    }
    let mut visited = HashSet::new();
    let extends_impure = class.extends.iter().any(|ext| {
        ext.base_def_id.is_some_and(|base_id| {
            function_or_ancestor_is_impure(base_id, class_index, &mut visited)
        })
    });
    if !extends_impure {
        return;
    }
    diags.push(
        Diagnostic::error(format!(
            "function '{}' extends an impure function and must be declared impure (MLS §12.3)",
            class.name.text
        ))
        .with_code(ER040_IMPURE_INHERITANCE_REQUIRES_IMPURE),
    );
}

fn function_or_ancestor_is_impure(
    def_id: DefId,
    class_index: &HashMap<DefId, &ClassDef>,
    visited: &mut HashSet<DefId>,
) -> bool {
    if !visited.insert(def_id) {
        return false;
    }
    let Some(class) = class_index.get(&def_id) else {
        return false;
    };
    if class.class_type == ClassType::Function && !class.pure {
        return true;
    }
    class.extends.iter().any(|ext| {
        ext.base_def_id
            .is_some_and(|base_id| function_or_ancestor_is_impure(base_id, class_index, visited))
    })
}

fn check_function_output_assignment(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.external.is_some() {
        return;
    }

    let output_names: Vec<&str> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Output(_)))
        .map(|(name, _)| name.as_str())
        .collect();

    if output_names.is_empty() {
        return;
    }

    let mut assigned = HashSet::new();
    for alg in &class.algorithms {
        collect_assigned_output_names(alg, &mut assigned);
    }

    for output_name in output_names {
        let has_binding = class
            .components
            .get(output_name)
            .and_then(|comp| comp.binding.as_ref())
            .is_some();
        if has_binding || assigned.contains(output_name) {
            continue;
        }
        diags.push(
            Diagnostic::error(format!(
                "output '{}' in function '{}' must be assigned in the algorithm \
                 or provided by an external function interface (MLS §12.2)",
                output_name, class.name.text
            ))
            .with_code(ER032_FUNCTION_OUTPUT_NOT_ASSIGNED),
        );
    }
}

fn collect_assigned_output_names(stmts: &[Statement], assigned: &mut HashSet<String>) {
    for stmt in stmts {
        match stmt {
            Statement::Assignment { comp, .. } => {
                if let Some(first) = comp.parts.first() {
                    assigned.insert(first.ident.text.to_string());
                }
            }
            Statement::FunctionCall { outputs, .. } => {
                for output in outputs {
                    collect_output_target_names(output, assigned);
                }
            }
            Statement::For { equations, .. } => {
                collect_assigned_output_names(equations, assigned);
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    collect_assigned_output_names(&block.stmts, assigned);
                }
                if let Some(else_stmts) = else_block {
                    collect_assigned_output_names(else_stmts, assigned);
                }
            }
            Statement::When(blocks) => {
                for block in blocks {
                    collect_assigned_output_names(&block.stmts, assigned);
                }
            }
            Statement::While(block) => {
                collect_assigned_output_names(&block.stmts, assigned);
            }
            Statement::Empty
            | Statement::Return { .. }
            | Statement::Break { .. }
            | Statement::Reinit { .. }
            | Statement::Assert { .. } => {}
        }
    }
}

fn collect_output_target_names(expr: &Expression, assigned: &mut HashSet<String>) {
    match expr {
        Expression::ComponentReference(cref) => {
            if let Some(first) = cref.parts.first() {
                assigned.insert(first.ident.text.to_string());
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_output_target_names(elem, assigned);
            }
        }
        Expression::Parenthesized { inner } => {
            collect_output_target_names(inner, assigned);
        }
        Expression::ArrayIndex { base, .. } => {
            collect_output_target_names(base, assigned);
        }
        Expression::FieldAccess { base, .. } => {
            collect_output_target_names(base, assigned);
        }
        _ => {}
    }
}

fn check_function_uninitialized_use(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let tracked_names: HashSet<String> = class.components.keys().cloned().collect();
    if tracked_names.is_empty() {
        return;
    }

    let output_names: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Output(_)))
        .map(|(n, _)| n.clone())
        .collect();

    let base_initialized: HashSet<String> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.causality, Causality::Input(_)) || c.binding.is_some())
        .map(|(n, _)| n.clone())
        .collect();

    let mut checker = FunctionInitUseCtx::new(&tracked_names, &output_names, diags);

    for alg in &class.algorithms {
        let mut initialized = base_initialized.clone();
        checker.check_statements(alg, &mut initialized);
    }
    for alg in &class.initial_algorithms {
        let mut initialized = base_initialized.clone();
        checker.check_statements(alg, &mut initialized);
    }
}

struct FunctionInitUseCtx<'a> {
    tracked_names: &'a HashSet<String>,
    output_names: &'a HashSet<String>,
    reported: HashSet<String>,
    diags: &'a mut Vec<Diagnostic>,
}

impl<'a> FunctionInitUseCtx<'a> {
    fn new(
        tracked_names: &'a HashSet<String>,
        output_names: &'a HashSet<String>,
        diags: &'a mut Vec<Diagnostic>,
    ) -> Self {
        Self {
            tracked_names,
            output_names,
            reported: HashSet::new(),
            diags,
        }
    }

    fn check_statements(&mut self, stmts: &[Statement], initialized: &mut HashSet<String>) {
        for stmt in stmts {
            self.check_statement(stmt, initialized);
        }
    }

    fn check_statement(&mut self, stmt: &Statement, initialized: &mut HashSet<String>) {
        match stmt {
            Statement::Empty | Statement::Break { .. } => {}
            Statement::Return { .. } => {
                for output_name in self
                    .output_names
                    .iter()
                    .filter(|output_name| !initialized.contains(*output_name))
                {
                    self.report_uninitialized_return(output_name);
                }
            }
            Statement::Assignment { comp, value } => {
                self.check_expr(value, initialized);
                if let Some(name) = component_reference_head_name(comp)
                    && self.tracked_names.contains(name)
                {
                    initialized.insert(name.to_string());
                }
            }
            Statement::FunctionCall { args, outputs, .. } => {
                for arg in args {
                    self.check_expr(arg, initialized);
                }
                for output in outputs {
                    self.mark_output_targets_initialized(output, initialized);
                }
            }
            Statement::For { indices, equations } => {
                for index in indices {
                    self.check_expr(&index.range, initialized);
                }
                let mut body_init = initialized.clone();
                self.check_statements(equations, &mut body_init);
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                let mut branch_states = Vec::new();
                for block in cond_blocks {
                    self.check_expr(&block.cond, initialized);
                    let mut branch_init = initialized.clone();
                    self.check_statements(&block.stmts, &mut branch_init);
                    branch_states.push(branch_init);
                }

                if let Some(else_stmts) = else_block {
                    let mut else_init = initialized.clone();
                    self.check_statements(else_stmts, &mut else_init);
                    branch_states.push(else_init);
                } else {
                    branch_states.push(initialized.clone());
                }

                if let Some(merged) = intersect_initialized_states(branch_states) {
                    *initialized = merged;
                }
            }
            Statement::When(blocks) => {
                for block in blocks {
                    self.check_expr(&block.cond, initialized);
                    let mut block_init = initialized.clone();
                    self.check_statements(&block.stmts, &mut block_init);
                }
            }
            Statement::While(block) => {
                self.check_expr(&block.cond, initialized);
                let mut block_init = initialized.clone();
                self.check_statements(&block.stmts, &mut block_init);
            }
            Statement::Reinit { value, .. } => {
                self.check_expr(value, initialized);
            }
            Statement::Assert {
                condition,
                message,
                level,
            } => {
                self.check_expr(condition, initialized);
                self.check_expr(message, initialized);
                if let Some(level_expr) = level {
                    self.check_expr(level_expr, initialized);
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &Expression, initialized: &HashSet<String>) {
        match expr {
            Expression::Empty | Expression::Terminal { .. } => {}
            Expression::ComponentReference(cref) => {
                if let Some(name) = component_reference_head_name(cref)
                    && self.tracked_names.contains(name)
                    && !initialized.contains(name)
                {
                    self.report_uninitialized_use(name);
                }
                self.check_component_reference_subscripts(cref, initialized);
            }
            Expression::Modification { value, .. } | Expression::NamedArgument { value, .. } => {
                self.check_expr(value, initialized);
            }
            Expression::Range { start, step, end } => {
                self.check_expr(start, initialized);
                if let Some(step_expr) = step {
                    self.check_expr(step_expr, initialized);
                }
                self.check_expr(end, initialized);
            }
            Expression::Unary { rhs, .. } => self.check_expr(rhs, initialized),
            Expression::Binary { lhs, rhs, .. } => {
                self.check_expr(lhs, initialized);
                self.check_expr(rhs, initialized);
            }
            Expression::FunctionCall { args, .. } => {
                for arg in args {
                    self.check_expr(arg, initialized);
                }
            }
            Expression::ClassModification {
                target: _,
                modifications,
            } => {
                for mod_expr in modifications {
                    self.check_expr(mod_expr, initialized);
                }
            }
            Expression::Array { elements, .. } | Expression::Tuple { elements } => {
                for elem in elements {
                    self.check_expr(elem, initialized);
                }
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, then_expr) in branches {
                    self.check_expr(cond, initialized);
                    self.check_expr(then_expr, initialized);
                }
                self.check_expr(else_branch, initialized);
            }
            Expression::Parenthesized { inner } => {
                self.check_expr(inner, initialized);
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                self.check_expr(expr, initialized);
                for index in indices {
                    self.check_expr(&index.range, initialized);
                }
                if let Some(filter_expr) = filter {
                    self.check_expr(filter_expr, initialized);
                }
            }
            Expression::ArrayIndex { base, subscripts } => {
                self.check_expr(base, initialized);
                self.check_subscripts(subscripts, initialized);
            }
            Expression::FieldAccess { base, .. } => {
                self.check_expr(base, initialized);
            }
        }
    }

    fn check_subscripts(&mut self, subscripts: &[Subscript], initialized: &HashSet<String>) {
        for sub in subscripts {
            if let Subscript::Expression(sub_expr) = sub {
                self.check_expr(sub_expr, initialized);
            }
        }
    }

    fn check_component_reference_subscripts(
        &mut self,
        cref: &ComponentReference,
        initialized: &HashSet<String>,
    ) {
        for subs in cref.parts.iter().filter_map(|part| part.subs.as_deref()) {
            self.check_subscripts(subs, initialized);
        }
    }

    fn mark_output_targets_initialized(
        &self,
        output: &Expression,
        initialized: &mut HashSet<String>,
    ) {
        let mut assigned = HashSet::new();
        collect_output_target_names(output, &mut assigned);
        for name in assigned {
            if self.tracked_names.contains(&name) {
                initialized.insert(name);
            }
        }
    }

    fn report_uninitialized_use(&mut self, name: &str) {
        let report_key = format!("use:{name}");
        if !self.reported.insert(report_key) {
            return;
        }
        self.diags.push(
            Diagnostic::error(format!(
                "function variable '{}' may be used before initialization (MLS §12.4.4)",
                name
            ))
            .with_code(ER043_FUNCTION_UNINITIALIZED_USE),
        );
    }

    fn report_uninitialized_return(&mut self, name: &str) {
        let report_key = format!("return:{name}");
        if !self.reported.insert(report_key) {
            return;
        }
        self.diags.push(
            Diagnostic::error(format!(
                "output '{}' may be returned before initialization in function (MLS §12.4.4)",
                name
            ))
            .with_code(ER043_FUNCTION_UNINITIALIZED_USE),
        );
    }
}

fn component_reference_head_name(cref: &ComponentReference) -> Option<&str> {
    cref.parts.first().map(|part| part.ident.text.as_ref())
}

fn intersect_initialized_states(states: Vec<HashSet<String>>) -> Option<HashSet<String>> {
    let mut state_iter = states.into_iter();
    let mut merged = state_iter.next()?;
    for branch_state in state_iter {
        merged.retain(|name| branch_state.contains(name));
    }
    Some(merged)
}

include!("semantic_checks/function_contracts.rs");
include!("semantic_checks/core_checks.rs");
