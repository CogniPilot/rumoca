//! Declaration, inheritance, statement, and advisory restriction checks
//! (split from `restrictions.rs` per SPEC_0021).

use super::super::*;
use super::{
    ClassContext, first_expression_token, is_input, reference_text, resolve_local_class_path,
};

// ---------------------------------------------------------------------------
// Declaration and inheritance restrictions (MLS §4, §5, §7).
// ---------------------------------------------------------------------------

/// MLS §7.1.3 / INST-024 (conservative subset): functions may only take part
/// in inheritance with other functions, and packages only with packages.
pub(super) fn check_inheritance_compatibility(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for ext in &class.extends {
        let Some(def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base) = find_class_by_def_id(def, def_id) else {
            continue;
        };
        // MLS §4.6 / DECL-008: extending an operator record is only legal as
        // a short class definition (alias); long-form classes have an
        // explicit `end Name;` token.
        if base.operator_record && !class.operator_record && class.end_name_token.is_some() {
            diags.push(semantic_error(
                ER091_INHERITANCE_COMPATIBILITY,
                format!(
                    "'{}' cannot extend operator record '{}' except as a short class definition (MLS §4.6)",
                    class.name.text, base.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/extends_operator_record",
                    "use `record X = OperatorRecord;` instead",
                ),
            ));
        }
        // The generic `class` kind is unrestricted and inherits into any
        // specialized class (MSL Icons are `class` definitions extended by
        // packages, models, and functions alike).
        if class.class_type == ClassType::Class || base.class_type == ClassType::Class {
            continue;
        }
        let class_is_function = class.class_type == ClassType::Function;
        let base_is_function = base.class_type == ClassType::Function;
        let class_is_package = class.class_type == ClassType::Package;
        let base_is_package = base.class_type == ClassType::Package;
        let incompatible =
            (class_is_function != base_is_function) || (class_is_package != base_is_package);
        if incompatible {
            diags.push(semantic_error(
                ER091_INHERITANCE_COMPATIBILITY,
                format!(
                    "{} '{}' cannot extend {} '{}' (MLS §7.1.3)",
                    class.class_type.as_str(),
                    class.name.text,
                    base.class_type.as_str(),
                    base.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/inheritance_compatibility",
                    "specialized classes must be compatible for inheritance",
                ),
            ));
        }
    }
}

/// MLS §4.7, §14 / DECL-026, DECL-029: operator classes and operator
/// functions must be directly inside an operator record; an operator record
/// shall not extend any of its enclosing scopes.
pub(super) fn check_operator_placement(
    class: &ClassDef,
    ancestors: &[ClassContext],
    diags: &mut Vec<Diagnostic>,
) {
    let parent_is_operator_scope = ancestors
        .last()
        .is_some_and(|parent| parent.operator_record || parent.class_type == ClassType::Operator);
    let is_operator_item = class.class_type == ClassType::Operator
        || (class.class_type == ClassType::Function && class.name.text.starts_with('\''));
    if is_operator_item && !parent_is_operator_scope {
        diags.push(semantic_error(
            ER092_OPERATOR_PLACEMENT,
            format!(
                "operator '{}' may only be declared directly inside an operator record (MLS §4.7)",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "restrictions/operator_placement",
                "move the operator into an operator record",
            ),
        ));
    }

    if class.operator_record {
        for ext in &class.extends {
            let Some(def_id) = ext.base_def_id else {
                continue;
            };
            if ancestors
                .iter()
                .any(|ancestor| ancestor.def_id == Some(def_id))
            {
                diags.push(semantic_error(
                    ER092_OPERATOR_PLACEMENT,
                    format!(
                        "operator record '{}' cannot extend one of its enclosing scopes (MLS §4.7)",
                        class.name.text
                    ),
                    label_from_token(
                        &class.name,
                        "restrictions/operator_record_enclosing",
                        "operator records cannot extend enclosing classes",
                    ),
                ));
            }
        }
    }
}

/// MLS §4.6.2 / DECL-034: a class whose base class chain ends in a builtin
/// type may not add components, equations, or algorithms.
pub(super) fn check_extends_builtin_with_elements(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let has_elements = !class.components.is_empty()
        || !class.equations.is_empty()
        || !class.initial_equations.is_empty()
        || !class.algorithms.is_empty()
        || !class.initial_algorithms.is_empty();
    if !has_elements {
        return;
    }
    for ext in &class.extends {
        if matches!(
            resolve_extend_type_root(ext, def),
            Some(ResolvedTypeRoot::Builtin(_))
        ) {
            diags.push(semantic_error(
                ER094_EXTENDS_BUILTIN_WITH_ELEMENTS,
                format!(
                    "class '{}' extends a builtin type and cannot declare components, equations, or algorithms (MLS §4.6.2)",
                    class.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/extends_builtin",
                    "classes refining builtin types may only modify attributes",
                ),
            ));
            return;
        }
    }
}

/// Component-level restrictions: declaration equations on model/block
/// components (DECL-013) and inner/outer connectors containing inputs
/// (INST-020).
pub(super) fn check_component_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        let type_root = resolve_component_type_root(comp, def);
        if comp.has_explicit_binding
            && comp.binding.is_some()
            && let Some(ResolvedTypeRoot::Class(type_class)) = &type_root
            && matches!(
                type_class.class_type,
                ClassType::Model | ClassType::Block | ClassType::Package
            )
        {
            // MLS §4.4 / DECL-013: declaration equations are only allowed on
            // components of type, record, operator record, or connector kind.
            diags.push(semantic_error(
                ER093_DECLARATION_EQUATION_CLASS,
                format!(
                    "component '{}' of {} type '{}' cannot have a declaration equation (MLS §4.4)",
                    name,
                    type_class.class_type.as_str(),
                    type_class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/declaration_equation",
                    "declaration equations require simple, record, or connector types",
                ),
            ));
        }

        // MLS §12.6 / OPREC-011: algebra over zero-sized operator-record
        // arrays needs the record's '0' operator (e.g. a matrix product with
        // zero inner dimension sums no terms). Reject zero-dimension
        // operator-record components when the record does not define one.
        if let Some(ResolvedTypeRoot::Class(type_class)) = &type_root
            && type_class.operator_record
            && component_has_literal_zero_dimension(comp)
            && !type_class.classes.contains_key("'0'")
        {
            diags.push(semantic_error(
                ER129_OPREC_ZERO_DIMENSION,
                format!(
                    "operator record '{}' has no '0' operator: zero-sized array component '{}' cannot participate in sum-style algebra (MLS §12.6)",
                    type_class.name.text, name
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/oprec_zero_dimension",
                    "declare an operator '0' in the operator record or use a non-zero dimension",
                ),
            ));
        }

        if (comp.inner || comp.outer)
            && let Some(ResolvedTypeRoot::Class(type_class)) = &type_root
            && type_class.class_type == ClassType::Connector
        {
            let has_public_input = type_class
                .components
                .iter()
                .any(|(_, member)| is_input(member) && !member.is_protected);
            if has_public_input {
                // MLS §4.8 / INST-020: inner/outer components shall not have
                // top-level public connectors containing inputs.
                diags.push(semantic_error(
                    ER095_INNER_OUTER_CONNECTOR_INPUT,
                    format!(
                        "inner/outer component '{}' uses connector '{}' containing public inputs (MLS §4.8)",
                        name, type_class.name.text
                    ),
                    label_from_token(
                        &comp.name_token,
                        "restrictions/inner_outer_connector_input",
                        "inner/outer connectors must not contain inputs",
                    ),
                ));
            }
        }
    }
}

fn resolve_extend_type_root<'a>(
    ext: &'a ast::Extend,
    def: &'a StoredDefinition,
) -> Option<ResolvedTypeRoot<'a>> {
    resolve_named_type_root(ext.base_def_id, &ext.base_name, def)
}

/// MLS §5.3.1 / INST-050: a name provided by more than one unqualified
/// (wildcard) import is ambiguous; using it is an error.
pub(super) fn check_ambiguous_unqualified_imports(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let mut export_sets: Vec<(String, HashSet<String>)> = Vec::new();
    for import in &class.imports {
        let Import::Unqualified { path, .. } = import else {
            continue;
        };
        let Some(target) = path
            .def_id
            .and_then(|def_id| find_class_by_def_id(def, def_id))
            .or_else(|| find_class_by_absolute_path(def, path))
        else {
            continue;
        };
        let mut names: HashSet<String> = HashSet::new();
        for (name, comp) in &target.components {
            if !comp.is_protected {
                names.insert(name.clone());
            }
        }
        for (name, nested) in &target.classes {
            if !nested.is_protected {
                names.insert(name.clone());
            }
        }
        export_sets.push((path.to_string(), names));
    }
    if export_sets.len() < 2 {
        return;
    }

    let mut ambiguous: HashSet<&str> = HashSet::new();
    for (i, (_, left)) in export_sets.iter().enumerate() {
        for (_, right) in export_sets.iter().skip(i + 1) {
            ambiguous.extend(left.intersection(right).map(String::as_str));
        }
    }
    if ambiguous.is_empty() {
        return;
    }

    let mut used = SinglePartRefCollector { found: Vec::new() };
    for (_, comp) in &class.components {
        if let Some(binding) = comp.binding.as_ref() {
            let _ = used.visit_expression(binding);
        }
    }
    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        let _ = used.visit_equation(eq);
    }
    for (name, token) in used.found {
        // Locally declared names shadow imports and are unambiguous.
        if class.components.contains_key(name.as_str()) || class.classes.contains_key(name.as_str())
        {
            continue;
        }
        if ambiguous.contains(name.as_str()) {
            diags.push(semantic_error(
                ER112_AMBIGUOUS_UNQUALIFIED_IMPORT,
                format!(
                    "'{name}' is provided by more than one unqualified import and is ambiguous (MLS §5.3.1)"
                ),
                label_from_token(
                    &token,
                    "restrictions/ambiguous_unqualified_import",
                    "qualify the name or use a selective import",
                ),
            ));
            return;
        }
    }
}

/// Walk a dotted name from the top-level classes of the stored definition.
fn find_class_by_absolute_path<'a>(
    def: &'a StoredDefinition,
    path: &ast::Name,
) -> Option<&'a ClassDef> {
    let mut segments = path.name.iter().map(|token| token.text.as_ref());
    let mut current = def.classes.get(segments.next()?)?;
    for segment in segments {
        current = current.classes.get(segment)?;
    }
    Some(current)
}

/// Collects single-part component references (name + token).
struct SinglePartRefCollector {
    found: Vec<(String, Token)>,
}

impl ast::Visitor for SinglePartRefCollector {
    fn visit_component_reference(
        &mut self,
        comp: &ComponentReference,
    ) -> std::ops::ControlFlow<()> {
        if let [part] = comp.parts.as_slice() {
            self.found
                .push((part.ident.text.to_string(), part.ident.clone()));
        }
        ast::visitor::walk_component_reference_default(self, comp)
    }
}

/// Collects unqualified calls to a specific builtin name (token cloned).
pub(super) struct NamedCallCollector {
    pub(super) name: &'static str,
    pub(super) found: Vec<Token>,
}

impl ast::Visitor for NamedCallCollector {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if let [part] = comp.parts.as_slice()
            && part.ident.text.as_ref() == self.name
        {
            self.found.push(part.ident.clone());
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

/// Whether a connect() endpoint is a source for its connection set: an inside
/// output connector (dotted reference) or a public outside input connector
/// (direct reference). Returns None when causality cannot be resolved.
pub(super) fn connect_endpoint_is_source(
    class: &ClassDef,
    comp: &ComponentReference,
    def: &StoredDefinition,
) -> Option<bool> {
    let target = resolve_component_reference_target(class, comp, def)?;
    let causality = if matches!(target.component.causality, Causality::Empty) {
        target
            .type_class
            .map(|type_class| type_class.causality.clone())?
    } else {
        target.component.causality.clone()
    };
    // Only inside output connectors count as sources here. Outside inputs
    // are not flagged: in standalone compilation a top-level input feeding
    // the set is the standard actuated-model pattern (and MSL's Adaptors
    // legitimately connect two optional outside inputs).
    let inside = comp.parts.len() > 1;
    Some(inside && matches!(causality, Causality::Output(_)))
}

/// MLS §7.2.5 / INST-040, INST-041: `each` requires an array target, and
/// array modification values must match the component array size when `each`
/// is absent.
pub(super) fn check_modification_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for (name, comp) in &class.components {
        // Arrays may also come from a type alias (`type Vector3 = Real[3]`);
        // walk the alias chain before deciding the component is scalar.
        let is_array =
            !comp.shape.is_empty() || !comp.shape_expr.is_empty() || type_alias_is_array(comp, def);
        if !is_array && comp.source_modification_each_flags.iter().any(|&f| f) {
            diags.push(semantic_error(
                ER104_EACH_ON_SCALAR,
                format!("'each' modification on non-array component '{name}' (MLS §7.2.5)"),
                label_from_token(
                    &comp.name_token,
                    "restrictions/each_on_scalar",
                    "'each' requires the modified component to be an array",
                ),
            ));
        }

        // Distributing a literal array value over a literal-size component
        // array: the sizes must match without `each`.
        if comp.shape.len() == 1
            && let Some(type_class) = component_type_class(comp, def)
        {
            check_distributed_modification_sizes(name, comp, type_class, diags);
        }
    }
}

fn check_distributed_modification_sizes(
    name: &str,
    comp: &ast::Component,
    type_class: &ClassDef,
    diags: &mut Vec<Diagnostic>,
) {
    let array_len = comp.shape[0];
    for (member_name, value) in &comp.modifications {
        if comp.each_modifications.contains(member_name) {
            continue;
        }
        let Some(member) = type_class.components.get(member_name) else {
            continue;
        };
        if !member.shape.is_empty() || !member.shape_expr.is_empty() {
            continue;
        }
        let Expression::Array { elements, .. } = value else {
            continue;
        };
        if elements.len() != array_len {
            diags.push(semantic_error(
                ER105_MODIFICATION_SIZE_MISMATCH,
                format!(
                    "modification '{member_name}' supplies {} value(s) for component array '{name}' of size {array_len}; sizes must match without 'each' (MLS §7.2.5)",
                    elements.len()
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/modification_size",
                    "array modification sizes must match the component array",
                ),
            ));
        }
    }
}

/// MLS §7.4 / INST-047: components deselected with `break` must be models,
/// blocks, or connectors.
pub(super) fn check_break_target_kinds(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    for ext in &class.extends {
        if ext.break_names.is_empty() {
            continue;
        }
        let Some(def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base) = find_class_by_def_id(def, def_id) else {
            continue;
        };
        for name in &ext.break_names {
            let Some(member) = base.components.get(name) else {
                continue;
            };
            let allowed = matches!(
                resolve_component_type_root(member, def),
                Some(ResolvedTypeRoot::Class(type_class)) if matches!(
                    type_class.class_type,
                    ClassType::Model | ClassType::Block | ClassType::Connector
                )
            );
            if !allowed {
                diags.push(semantic_error(
                    ER106_BREAK_TARGET_KIND,
                    format!(
                        "'break {name}' deselects a component that is not a model, block, or connector (MLS §7.4)"
                    ),
                    label_from_token(
                        &class.name,
                        "restrictions/break_target_kind",
                        "selective extension may only deselect model, block, or connector components",
                    ),
                ));
            }
        }
    }
}

/// Model/block algorithm statement restrictions: assignments to
/// parameters/constants (MLS §3.8 / FUNC-008) and to whole model/block
/// instances (MLS §11.1 / ALG-002).
pub(super) fn check_model_algorithm_statements(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    // Initial algorithms are exempt from the assign-to-parameter rule:
    // parameters with fixed=false are legitimately assigned at
    // initialization (MLS §8.6).
    for section in &class.algorithms {
        for stmt in section {
            check_model_statement(stmt, class, def, diags, false);
        }
    }
    for section in &class.initial_algorithms {
        for stmt in section {
            check_model_statement(stmt, class, def, diags, true);
        }
    }
}

fn check_model_statement(
    stmt: &Statement,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
    in_initial: bool,
) {
    match stmt {
        Statement::Assignment { comp, .. } => {
            let Some(target) = resolve_component_reference_target(class, comp, def) else {
                return;
            };
            if !in_initial
                && matches!(
                    target.component.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
            {
                diags.push(semantic_error(
                    ER102_ASSIGN_TO_PARAMETER,
                    format!(
                        "cannot assign to '{}': higher-variability expressions cannot be assigned to parameters or constants (MLS §3.8)",
                        reference_text(comp)
                    ),
                    label_from_token(
                        target.token,
                        "restrictions/assign_to_parameter",
                        "parameters and constants cannot be assigned in algorithms",
                    ),
                ));
            }
            if let Some(type_class) = target.type_class
                && matches!(
                    type_class.class_type,
                    ClassType::Model | ClassType::Block | ClassType::Package
                )
            {
                diags.push(semantic_error(
                    ER111_ASSIGN_TO_CLASS_INSTANCE,
                    format!(
                        "cannot assign to '{}': only components of type, record, operator record, or connector kind may appear on the left-hand side (MLS §11.1)",
                        reference_text(comp)
                    ),
                    label_from_token(
                        target.token,
                        "restrictions/assign_to_class_instance",
                        "model and block instances cannot be assignment targets",
                    ),
                ));
            }
        }
        Statement::For {
            indices, equations, ..
        } => {
            check_event_for_statement(indices, equations, class, def, diags);
            for inner in equations {
                check_model_statement(inner, class, def, diags, in_initial);
            }
        }
        Statement::While(block) => {
            check_event_while_statement(block, class, def, diags);
            for inner in &block.stmts {
                check_model_statement(inner, class, def, diags, in_initial);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner in &block.stmts {
                    check_model_statement(inner, class, def, diags, in_initial);
                }
            }
            for inner in else_block.iter().flatten() {
                check_model_statement(inner, class, def, diags, in_initial);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for inner in &block.stmts {
                    check_model_statement(inner, class, def, diags, in_initial);
                }
            }
        }
        _ => {}
    }
}

/// MLS §3.7.1 / EXPR-021: enumeration conversion E(i) requires i to map to an
/// enumeration value; literal violations are rejected.
pub(super) fn check_enum_conversion_ranges(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let mut collector = EnumConversionCollector { class, def, diags };
    for (_, comp) in &class.components {
        if let Some(binding) = comp.binding.as_ref() {
            let _ = collector.visit_expression(binding);
        }
    }
    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        let _ = collector.visit_equation(eq);
    }
}

struct EnumConversionCollector<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    diags: &'a mut Vec<Diagnostic>,
}

impl ast::Visitor for EnumConversionCollector<'_> {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if let Some(target) = resolve_local_class_path(comp, self.class, self.def)
            && !target.enum_literals.is_empty()
            && let [arg] = args
            && let Some(value) = integer_literal_value_restrictions(arg)
            && (value < 1 || value as usize > target.enum_literals.len())
            && let Some(token) = comp.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER107_ENUM_CONVERSION_RANGE,
                format!(
                    "{} has {} literal(s); conversion from {value} is out of range (MLS §4.9.5.2)",
                    reference_text(comp),
                    target.enum_literals.len()
                ),
                label_from_token(
                    token,
                    "restrictions/enum_conversion_range",
                    "Integer-to-enumeration conversions must map to a literal",
                ),
            ));
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

fn integer_literal_value_restrictions(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus,
            rhs,
            ..
        } => integer_literal_value_restrictions(rhs).map(|value| -value),
        Expression::Parenthesized { inner, .. } => integer_literal_value_restrictions(inner),
        _ => None,
    }
}

/// MLS §12.4.4 / FUNC-023: binding execution order in functions must not have
/// cycles.
pub(super) fn check_function_binding_cycles(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let mut visiting: HashSet<&str> = HashSet::new();
    let mut done: HashSet<&str> = HashSet::new();
    for (name, _) in &class.components {
        if has_binding_cycle(class, name, &mut visiting, &mut done) {
            let comp = &class.components[name.as_str()];
            diags.push(semantic_error(
                ER007_CYCLIC_PARAMETER_BINDING,
                format!(
                    "cyclic binding involving '{name}' in function '{}' (MLS §12.4.4)",
                    class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/function_binding_cycle",
                    "function default bindings must be acyclic",
                ),
            ));
            return;
        }
    }
}

fn has_binding_cycle<'a>(
    class: &'a ClassDef,
    name: &'a str,
    visiting: &mut HashSet<&'a str>,
    done: &mut HashSet<&'a str>,
) -> bool {
    if done.contains(name) {
        return false;
    }
    if !visiting.insert(name) {
        return true;
    }
    if let Some(comp) = class.components.get(name)
        && let Some(binding) = comp.binding.as_ref()
        && binding_targets(class, binding)
            .into_iter()
            .any(|target_key| has_binding_cycle(class, target_key, visiting, done))
    {
        return true;
    }
    visiting.remove(name);
    done.insert(name);
    false
}

/// Component names of this class referenced by single-part references in a
/// binding expression.
fn binding_targets<'a>(class: &'a ClassDef, binding: &Expression) -> Vec<&'a str> {
    rumoca_ir_ast::collect_component_refs(binding)
        .into_iter()
        .filter_map(|cref| match cref.parts.as_slice() {
            [part] => class
                .components
                .get_key_value(part.ident.text.as_ref())
                .map(|(key, _)| key.as_str()),
            _ => None,
        })
        .collect()
}

/// MLS §14 / OPREC-006: the first two inputs of an operator function shall
/// not have default values.
pub(super) fn check_operator_function_defaults(
    class: &ClassDef,
    ancestors: &[ClassContext],
    diags: &mut Vec<Diagnostic>,
) {
    let parent_is_operator_scope = ancestors
        .last()
        .is_some_and(|parent| parent.operator_record || parent.class_type == ClassType::Operator);
    if !parent_is_operator_scope || !class.name.text.starts_with('\'') {
        return;
    }
    // The first-two-inputs-no-defaults rule applies to the binary operator
    // functions; constructors and conversion operators ('constructor', '0',
    // 'String') routinely default trailing arguments (e.g. Complex(re, im=0),
    // 'String'(c, significantDigits=6)).
    if !matches!(
        class.name.text.as_ref(),
        "'+'" | "'-'" | "'*'" | "'/'" | "'^'" | "'=='" | "'<>'" | "'and'" | "'or'"
    ) {
        return;
    }
    for (name, comp) in class
        .components
        .iter()
        .filter(|(_, comp)| is_input(comp))
        .take(2)
    {
        if comp.binding.is_some() {
            diags.push(semantic_error(
                ER110_OPERATOR_FUNCTION_DEFAULTS,
                format!(
                    "operator function '{}' input '{name}' may not have a default value (MLS §14)",
                    class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/operator_function_defaults",
                    "the first two operator inputs must not have defaults",
                ),
            ));
        }
    }
}

// ---------------------------------------------------------------------------
// Event-generating expression rules (MLS §3.7.4, §11.2): while-statements,
// for-statement indices, and array-comprehension iterators
// (ALG-003, ALG-006, ARR-035).
// ---------------------------------------------------------------------------

/// MLS §3.7.4 (conservative): relations over continuous operands and the
/// event-triggering numeric conversions generate events. Subtrees under
/// noEvent()/smooth() are exempt.
fn expression_generates_events(
    class: &ClassDef,
    def: &StoredDefinition,
    expr: &Expression,
) -> bool {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            let relational = matches!(
                op,
                OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge
            );
            if relational
                && (expression_has_continuous_ref(class, def, lhs)
                    || expression_has_continuous_ref(class, def, rhs))
            {
                return true;
            }
            expression_generates_events(class, def, lhs)
                || expression_generates_events(class, def, rhs)
        }
        Expression::Unary { rhs, .. } => expression_generates_events(class, def, rhs),
        Expression::Parenthesized { inner, .. } => expression_generates_events(class, def, inner),
        Expression::FunctionCall { comp, args, .. } => {
            if let [part] = comp.parts.as_slice() {
                let name = part.ident.text.as_ref();
                if matches!(name, "noEvent" | "smooth") {
                    return false;
                }
                if matches!(name, "div" | "mod" | "rem" | "ceil" | "floor" | "integer")
                    && args
                        .iter()
                        .any(|arg| expression_has_continuous_ref(class, def, arg))
                {
                    return true;
                }
            }
            args.iter()
                .any(|arg| expression_generates_events(class, def, arg))
        }
        Expression::If { .. } => rumoca_ir_ast::collect_component_refs(expr)
            .iter()
            .any(|cref| component_ref_is_continuous(class, def, cref)),
        _ => false,
    }
}

fn expression_has_continuous_ref(
    class: &ClassDef,
    def: &StoredDefinition,
    expr: &Expression,
) -> bool {
    rumoca_ir_ast::collect_component_refs(expr)
        .iter()
        .any(|cref| component_ref_is_continuous(class, def, cref))
}

/// `time` or a locally declared Real-rooted component without
/// discrete/parameter/constant variability counts as continuous. Integer,
/// Boolean, String, and enumeration components are discrete by type
/// (MLS §3.8.3) regardless of variability prefix.
fn component_ref_is_continuous(
    class: &ClassDef,
    def: &StoredDefinition,
    cref: &ComponentReference,
) -> bool {
    let [part] = cref.parts.as_slice() else {
        return false;
    };
    let name = part.ident.text.as_ref();
    if name == "time" {
        return true;
    }
    class.components.get(name).is_some_and(|component| {
        matches!(
            component.variability,
            Variability::Continuous(_) | Variability::Empty
        ) && matches!(
            resolve_component_type_root(component, def),
            Some(ResolvedTypeRoot::Builtin("Real"))
        )
    })
}

/// MLS §10.3.4 / ARR-035: event-generating expressions in array-comprehension
/// iterators require evaluable iteration ranges.
pub(super) fn check_event_generating_iterators(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let mut collector = ComprehensionCollector { found: Vec::new() };
    for (_, comp) in &class.components {
        if let Some(binding) = comp.binding.as_ref() {
            let _ = collector.visit_expression(binding);
        }
    }
    for eq in class.equations.iter().chain(class.initial_equations.iter()) {
        let _ = collector.visit_equation(eq);
    }
    for (body, iterator_names, ranges) in collector.found {
        // Iterator variables bound to continuous ranges make relations in the
        // body event-generating; treat them as continuous references.
        let body_generates = expression_generates_events(class, def, &body)
            || rumoca_ir_ast::collect_component_refs(&body)
                .iter()
                .any(|cref| matches!(cref.parts.as_slice(), [part] if iterator_names.contains(&part.ident.text.to_string())))
                && expression_contains_relation(&body);
        if !body_generates {
            continue;
        }
        for range in ranges {
            let evaluable = rumoca_ir_ast::collect_component_refs(&range)
                .iter()
                .all(|cref| !component_ref_is_continuous(class, def, cref));
            if !evaluable && let Some(token) = first_expression_token(Some(&range)) {
                diags.push(semantic_error(
                    ER116_EVENT_ITERATOR_EVALUABLE,
                    "array-comprehension iterator with event-generating body must have an evaluable range (MLS §10.3.4)"
                        .to_string(),
                    label_from_token(
                        &token,
                        "restrictions/event_iterator_evaluable",
                        "use a parameter or constant iteration range",
                    ),
                ));
            }
        }
    }
}

struct ComprehensionCollector {
    found: Vec<(Expression, Vec<String>, Vec<Expression>)>,
}

impl ast::Visitor for ComprehensionCollector {
    fn visit_expression(&mut self, expr: &Expression) -> std::ops::ControlFlow<()> {
        if let Expression::ArrayComprehension {
            expr: body,
            indices,
            ..
        } = expr
        {
            self.found.push((
                body.as_ref().clone(),
                indices
                    .iter()
                    .map(|index| index.ident.text.to_string())
                    .collect(),
                indices.iter().map(|index| index.range.clone()).collect(),
            ));
        }
        walk_expression_default(self, expr)
    }
}

/// Whether the expression contains a relational operator or if-expression.
fn expression_contains_relation(expr: &Expression) -> bool {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            matches!(
                op,
                OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge
            ) || expression_contains_relation(lhs)
                || expression_contains_relation(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_relation(rhs),
        Expression::Parenthesized { inner, .. } => expression_contains_relation(inner),
        Expression::If { .. } => true,
        Expression::FunctionCall { args, .. } => args.iter().any(expression_contains_relation),
        _ => false,
    }
}

/// Whether any expression in the statement (recursively) generates events.
fn statement_generates_events(class: &ClassDef, def: &StoredDefinition, stmt: &Statement) -> bool {
    match stmt {
        Statement::Assignment { value, .. } => expression_generates_events(class, def, value),
        Statement::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expression_generates_events(class, def, arg)),
        Statement::For { equations, .. } => equations
            .iter()
            .any(|inner| statement_generates_events(class, def, inner)),
        Statement::While(block) => {
            expression_generates_events(class, def, &block.cond)
                || block
                    .stmts
                    .iter()
                    .any(|inner| statement_generates_events(class, def, inner))
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks.iter().any(|block| {
                expression_generates_events(class, def, &block.cond)
                    || block
                        .stmts
                        .iter()
                        .any(|inner| statement_generates_events(class, def, inner))
            }) || else_block
                .iter()
                .flatten()
                .any(|inner| statement_generates_events(class, def, inner))
        }
        _ => false,
    }
}

/// MLS §11.2 / ALG-003: if the loop body generates events, the iteration
/// range must be evaluable during translation.
fn check_event_for_statement(
    indices: &[ast::ForIndex],
    body: &[Statement],
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if !body
        .iter()
        .any(|inner| statement_generates_events(class, def, inner))
    {
        return;
    }
    for index in indices {
        let evaluable = rumoca_ir_ast::collect_component_refs(&index.range)
            .iter()
            .all(|cref| !component_ref_is_continuous(class, def, cref));
        if !evaluable {
            diags.push(semantic_error(
                ER115_EVENT_FOR_INDEX_EVALUABLE,
                "for-statement with event-generating expressions must have an evaluable iteration range (MLS §11.2)"
                    .to_string(),
                label_from_token(
                    &index.ident,
                    "restrictions/event_for_index",
                    "use a parameter or constant iteration range",
                ),
            ));
        }
    }
}

/// MLS §11.2 / ALG-006: event-generating expressions are not allowed in
/// while conditions or bodies.
fn check_event_while_statement(
    block: &ast::StatementBlock,
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let generates = expression_generates_events(class, def, &block.cond)
        || block
            .stmts
            .iter()
            .any(|inner| statement_generates_events(class, def, inner));
    if generates && let Some(token) = first_expression_token(Some(&block.cond)) {
        diags.push(semantic_error(
            ER114_EVENT_IN_WHILE,
            "event-generating expressions are not allowed in while-statement conditions or bodies (MLS §11.2)"
                .to_string(),
            label_from_token(
                &token,
                "restrictions/event_in_while",
                "wrap the expression in noEvent() or restructure the loop",
            ),
        ));
    }
}

/// MLS §9.4.1 / CONN-024, CONN-025: the `equalityConstraint` function of an
/// overconstrained type must take two inputs of the enclosing type and return
/// one Real output whose dimension is a literal constant n >= 0.
pub(super) fn check_equality_constraint_prototype(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    let Some(func) = class.classes.get("equalityConstraint") else {
        return;
    };
    if func.class_type != ClassType::Function {
        return;
    }
    // MLS §9.4.1 / CONN-022: an overdetermined type/record may not have flow
    // components.
    for (name, comp) in &class.components {
        if !matches!(comp.connection, Connection::Empty) {
            diags.push(semantic_error(
                ER118_OVERDETERMINED_FLOW_MEMBER,
                format!(
                    "overdetermined type '{}' may not declare flow/stream component '{name}' (MLS §9.4.1)",
                    class.name.text
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/overdetermined_flow_member",
                    "overdetermined types carry only potential variables",
                ),
            ));
        }
    }
    let inputs: Vec<&ast::Component> = func
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, Causality::Input(_)))
        .map(|(_, comp)| comp)
        .collect();
    let outputs: Vec<&ast::Component> = func
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, Causality::Output(_)))
        .map(|(_, comp)| comp)
        .collect();

    let enclosing = class.name.text.as_ref();
    let inputs_ok = inputs.len() == 2
        && inputs.iter().all(|comp| {
            let type_name = comp.type_name.to_string();
            type_name == enclosing || type_name.ends_with(&format!(".{enclosing}"))
        });
    let outputs_ok = outputs.len() == 1
        && outputs[0].type_name.to_string() == "Real"
        && outputs[0].shape_expr.len() == 1
        && match outputs[0].shape_expr.first() {
            Some(Subscript::Expression(expr)) => {
                integer_literal_value_restrictions(expr).is_some_and(|n| n >= 0)
            }
            _ => false,
        };

    if !inputs_ok || !outputs_ok {
        diags.push(semantic_error(
            ER117_EQUALITY_CONSTRAINT_PROTOTYPE,
            format!(
                "equalityConstraint of '{enclosing}' must be `function equalityConstraint(input {enclosing} a, input {enclosing} b) output Real residue[n]` with literal n >= 0 (MLS §9.4.1)"
            ),
            label_from_token(
                &func.name,
                "restrictions/equality_constraint_prototype",
                "fix the equalityConstraint signature",
            ),
        ));
    }
}

/// MLS §12.7.1 / FUNC-030, FUNC-031: a `derivative` annotation must reference
/// a function with at least one output, and `zeroDerivative` must name an
/// input of the annotated function.
pub(super) fn check_derivative_annotations(
    class: &ClassDef,
    def: &StoredDefinition,
    ancestors: &[ClassContext],
    diags: &mut Vec<Diagnostic>,
) {
    for entry in &class.annotation {
        let (target, value, modifications) = match entry {
            Expression::Modification { target, value, .. } => {
                (target, Some(value.as_ref()), &[][..])
            }
            Expression::ClassModification {
                target,
                modifications,
                ..
            } => (target, None, modifications.as_slice()),
            // `annotation(derivative(zeroDerivative = w) = dF)` parses as an
            // Assign with the class modification on the left.
            Expression::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } => match lhs.as_ref() {
                Expression::ClassModification {
                    target,
                    modifications,
                    ..
                } => (target, Some(rhs.as_ref()), modifications.as_slice()),
                _ => continue,
            },
            _ => continue,
        };
        if target.parts.first().map(|p| p.ident.text.as_ref()) != Some("derivative") {
            continue;
        }

        // FUNC-030: the referenced derivative function needs outputs.
        if let Some(Expression::ComponentReference(cref)) = value
            && let Some(derivative_fn) = resolve_sibling_class(cref, def, ancestors)
            && derivative_fn.class_type == ClassType::Function
            && !derivative_fn
                .components
                .iter()
                .any(|(_, comp)| matches!(comp.causality, Causality::Output(_)))
            && let Some(token) = cref.parts.first().map(|part| &part.ident)
        {
            diags.push(semantic_error(
                ER120_DERIVATIVE_ANNOTATION,
                format!(
                    "derivative function '{}' has no outputs (MLS §12.7.1)",
                    reference_text(cref)
                ),
                label_from_token(
                    token,
                    "restrictions/derivative_annotation_outputs",
                    "the derivative output list shall not be empty",
                ),
            ));
        }

        // FUNC-031: zeroDerivative must name an input of the annotated
        // function.
        for modification in modifications {
            let Expression::Modification {
                target: mod_target,
                value: mod_value,
                ..
            } = modification
            else {
                continue;
            };
            if mod_target.parts.first().map(|p| p.ident.text.as_ref()) != Some("zeroDerivative") {
                continue;
            }
            let Expression::ComponentReference(arg) = mod_value.as_ref() else {
                continue;
            };
            let [part] = arg.parts.as_slice() else {
                continue;
            };
            let names_input = class
                .components
                .get(part.ident.text.as_ref())
                .is_some_and(is_input);
            if !names_input {
                diags.push(semantic_error(
                    ER120_DERIVATIVE_ANNOTATION,
                    format!(
                        "zeroDerivative '{}' does not name an input of function '{}' (MLS §12.7.1)",
                        part.ident.text, class.name.text
                    ),
                    label_from_token(
                        &part.ident,
                        "restrictions/derivative_annotation_zero_derivative",
                        "zeroDerivative must reference an input of the annotated function",
                    ),
                ));
            }
        }
    }
}

/// Resolve a sibling class reference: the annotated function's parent scope
/// first, then top-level classes.
fn resolve_sibling_class<'a>(
    cref: &ComponentReference,
    def: &'a StoredDefinition,
    ancestors: &[ClassContext],
) -> Option<&'a ClassDef> {
    let [part] = cref.parts.as_slice() else {
        return None;
    };
    let name = part.ident.text.as_ref();
    if let Some(parent) = ancestors.last().and_then(|context| context.def_id)
        && let Some(parent_class) = find_class_by_def_id(def, parent)
        && let Some(sibling) = parent_class.classes.get(name)
    {
        return Some(sibling);
    }
    def.classes.get(name)
}

/// MLS §11.2 / ALG-004: inside a for-loop, assigning the entire array is not
/// allowed when the same array is also subscripted with the loop variable.
pub(super) fn check_whole_array_assignment_in_for(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for section in class
        .algorithms
        .iter()
        .chain(class.initial_algorithms.iter())
    {
        for stmt in section {
            scan_for_whole_array_assignment(stmt, class, diags);
        }
    }
}

fn scan_for_whole_array_assignment(
    stmt: &Statement,
    class: &ClassDef,
    diags: &mut Vec<Diagnostic>,
) {
    match stmt {
        Statement::For {
            indices, equations, ..
        } => {
            let loop_vars: HashSet<&str> = indices
                .iter()
                .map(|index| index.ident.text.as_ref())
                .collect();
            // Arrays subscripted with a loop variable anywhere in the body.
            let mut indexed: HashSet<String> = HashSet::new();
            for inner in equations {
                collect_loop_indexed_arrays(inner, &loop_vars, &mut indexed);
            }
            for inner in equations {
                flag_whole_array_assignments(inner, class, &indexed, diags);
            }
            for inner in equations {
                scan_for_whole_array_assignment(inner, class, diags);
            }
        }
        Statement::While(block) => {
            for inner in &block.stmts {
                scan_for_whole_array_assignment(inner, class, diags);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for inner in &block.stmts {
                    scan_for_whole_array_assignment(inner, class, diags);
                }
            }
            for inner in else_block.iter().flatten() {
                scan_for_whole_array_assignment(inner, class, diags);
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                for inner in &block.stmts {
                    scan_for_whole_array_assignment(inner, class, diags);
                }
            }
        }
        _ => {}
    }
}

fn collect_loop_indexed_arrays(
    stmt: &Statement,
    loop_vars: &HashSet<&str>,
    indexed: &mut HashSet<String>,
) {
    if let Statement::Assignment { comp, .. } = stmt
        && let [part] = comp.parts.as_slice()
        && part.subs.as_ref().is_some_and(|subs| {
            subs.iter().any(|sub| {
                matches!(
                    sub,
                    Subscript::Expression(Expression::ComponentReference(cref))
                        if matches!(cref.parts.as_slice(), [p] if loop_vars.contains(p.ident.text.as_ref()))
                )
            })
        })
    {
        indexed.insert(part.ident.text.to_string());
    }
    if let Statement::For { equations, .. } = stmt {
        for inner in equations {
            collect_loop_indexed_arrays(inner, loop_vars, indexed);
        }
    }
}

fn flag_whole_array_assignments(
    stmt: &Statement,
    class: &ClassDef,
    indexed: &HashSet<String>,
    diags: &mut Vec<Diagnostic>,
) {
    let Statement::Assignment { comp, .. } = stmt else {
        return;
    };
    let [part] = comp.parts.as_slice() else {
        return;
    };
    if part.subs.as_ref().is_some_and(|subs| !subs.is_empty()) {
        return;
    }
    let name = part.ident.text.as_ref();
    let is_array = class
        .components
        .get(name)
        .is_some_and(|component| !component.shape.is_empty() || !component.shape_expr.is_empty());
    if is_array && indexed.contains(name) {
        diags.push(semantic_error(
            ER121_WHOLE_ARRAY_IN_FOR,
            format!(
                "assignment to the entire array '{name}' inside a for-loop that also subscripts it with the loop variable (MLS §11.2)"
            ),
            label_from_token(
                &part.ident,
                "restrictions/whole_array_in_for",
                "assign elements individually or move the whole-array assignment out of the loop",
            ),
        ));
    }
}

// ---------------------------------------------------------------------------
// Advisory (warning-severity) annotation rules: FUNC-032, ANN-002, ANN-013,
// ANN-014.
// ---------------------------------------------------------------------------

pub(super) fn check_annotation_advisories(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    // MLS §12.9 / FUNC-032: external functions without an explicit
    // pure/impure declaration are deprecated.
    if class.class_type == ClassType::Function && class.external.is_some() && !class.purity_declared
    {
        diags.push(Diagnostic::warning(
            WR001_EXTERNAL_PURITY_UNDECLARED,
            format!(
                "external function '{}' should declare `pure` or `impure` explicitly; the bare form is deprecated (MLS §12.9)",
                class.name.text
            ),
            label_from_token(
                &class.name,
                "restrictions/external_purity_undeclared",
                "add `pure` or `impure` to the function declaration",
            ),
        ));
    }

    for entry in &class.annotation {
        // MLS §18.1 / ANN-013: standard annotations only where their
        // semantics is defined: `experiment` belongs on simulation models.
        if annotation_entry_name(entry) == Some("experiment")
            && !matches!(
                class.class_type,
                ClassType::Model | ClassType::Block | ClassType::Class
            )
        {
            diags.push(Diagnostic::warning(
                WR003_ANNOTATION_CONTEXT,
                format!(
                    "experiment annotation on {} '{}' has no defined semantics (MLS §18.1)",
                    class.class_type.as_str(),
                    class.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/annotation_context",
                    "experiment annotations belong on simulation models",
                ),
            ));
        }
    }

    // MLS §18.7 / ANN-014: a class with a TestCase annotation should only be
    // used inside classes that also carry one.
    if !class_has_annotation(class, "TestCase") {
        for (name, comp) in &class.components {
            let Some(ResolvedTypeRoot::Class(type_class)) = resolve_component_type_root(comp, def)
            else {
                continue;
            };
            if class_has_annotation(type_class, "TestCase") {
                diags.push(Diagnostic::warning(
                    WR004_TESTCASE_USAGE,
                    format!(
                        "component '{name}' uses TestCase-annotated class '{}' inside a class without a TestCase annotation (MLS §18.7)",
                        type_class.name.text
                    ),
                    label_from_token(
                        &comp.name_token,
                        "restrictions/testcase_usage",
                        "TestCase classes are not intended for reuse in other models",
                    ),
                ));
            }
        }
    }
}

/// MLS §18.6 / ANN-009: Evaluate on a parameter without an evaluable binding
/// has no effect; warn so the annotation is not silently ignored.
pub(super) fn check_evaluate_annotations(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (name, comp) in &class.components {
        let has_evaluate = comp.annotation.iter().any(|entry| {
            matches!(
                entry,
                Expression::Modification { target, value, .. }
                    if target.parts.first().map(|p| p.ident.text.as_ref()) == Some("Evaluate")
                        && matches!(
                            value.as_ref(),
                            Expression::Terminal { token, .. } if token.text.as_ref() == "true"
                        )
            )
        });
        if !has_evaluate {
            continue;
        }
        let evaluable = comp
            .binding
            .as_ref()
            .is_some_and(|binding| binding_is_translation_evaluable(class, binding));
        if !evaluable {
            diags.push(Diagnostic::warning(
                WR005_EVALUATE_NOT_EVALUABLE,
                format!(
                    "Evaluate=true on '{name}' has no effect: the parameter has no evaluable binding (MLS §18.6)"
                ),
                label_from_token(
                    &comp.name_token,
                    "restrictions/evaluate_not_evaluable",
                    "give the parameter an evaluable binding or drop the annotation",
                ),
            ));
        }
    }
}

fn annotation_entry_name(entry: &Expression) -> Option<&str> {
    match entry {
        Expression::Modification { target, .. } | Expression::ClassModification { target, .. } => {
            target.parts.first().map(|part| part.ident.text.as_ref())
        }
        Expression::ComponentReference(cref) => {
            cref.parts.first().map(|part| part.ident.text.as_ref())
        }
        _ => None,
    }
}

fn class_has_annotation(class: &ClassDef, name: &str) -> bool {
    class
        .annotation
        .iter()
        .any(|entry| annotation_entry_name(entry) == Some(name))
}

/// MLS §14 / OPREC-005, OPREC-007: two overloads of the same operator with
/// identical input signatures make every potential call ambiguous.
pub(super) fn check_ambiguous_operator_overloads(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    if class.class_type != ClassType::Operator && !class.name.text.starts_with('\'') {
        return;
    }
    if matches!(class.name.text.as_ref(), "'constructor'" | "'0'") {
        return;
    }
    let signatures: Vec<(String, Vec<String>)> = class
        .classes
        .iter()
        .filter(|(_, nested)| nested.class_type == ClassType::Function)
        .map(|(name, nested)| {
            let inputs: Vec<String> = nested
                .components
                .iter()
                .filter(|(_, comp)| is_input(comp))
                .map(|(_, comp)| {
                    // Dimension count is part of the signature: scalar and
                    // array overloads of the same type are distinguishable.
                    format!(
                        "{}#{}",
                        comp.type_name,
                        comp.shape_expr.len().max(comp.shape.len())
                    )
                })
                .collect();
            (name.clone(), inputs)
        })
        .collect();
    for (i, (name_a, sig_a)) in signatures.iter().enumerate() {
        for (name_b, sig_b) in signatures.iter().skip(i + 1) {
            if sig_a == sig_b {
                diags.push(semantic_error(
                    ER122_AMBIGUOUS_OPERATOR_OVERLOAD,
                    format!(
                        "operator '{}' overloads '{name_a}' and '{name_b}' have identical input signatures; every call is ambiguous (MLS §14)",
                        class.name.text
                    ),
                    label_from_token(
                        &class.name,
                        "restrictions/ambiguous_operator_overload",
                        "give the overloads distinguishable input types",
                    ),
                ));
            }
        }
    }
}

/// MLS §7.3.1 / INST-026: `redeclare model extends B(...)` requires the
/// original class B (from the base classes) to be replaceable.
pub(super) fn check_class_extends_redeclare(
    class: &ClassDef,
    def: &StoredDefinition,
    ancestors: &[ClassContext],
    diags: &mut Vec<Diagnostic>,
) {
    if !class.is_redeclare {
        return;
    }
    // class-extends form: the redeclared class extends its own name.
    let extends_self = class
        .extends
        .iter()
        .any(|ext| ext.base_name.to_string() == class.name.text.as_ref());
    if !extends_self {
        return;
    }
    let Some(parent_def_id) = ancestors.last().and_then(|context| context.def_id) else {
        return;
    };
    let Some(parent) = find_class_by_def_id(def, parent_def_id) else {
        return;
    };
    for ext in &parent.extends {
        let Some(base) = ext
            .base_def_id
            .and_then(|def_id| find_class_by_def_id(def, def_id))
        else {
            continue;
        };
        if let Some(original) = base.classes.get(class.name.text.as_ref())
            && !original.is_replaceable
        {
            diags.push(semantic_error(
                ER123_CLASS_EXTENDS_NON_REPLACEABLE,
                format!(
                    "'redeclare {} extends {}' requires the original class in '{}' to be replaceable (MLS §7.3.1)",
                    class.class_type.as_str(),
                    class.name.text,
                    base.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/class_extends_non_replaceable",
                    "mark the original class replaceable",
                ),
            ));
        }
    }
}

/// Conservative translation-time evaluability of a binding: every unqualified
/// reference is a parameter/constant of this class (or unknown / non-`time`).
fn binding_is_translation_evaluable(class: &ClassDef, binding: &Expression) -> bool {
    rumoca_ir_ast::collect_component_refs(binding)
        .iter()
        .all(|cref| match cref.parts.as_slice() {
            [part] => {
                let referenced = part.ident.text.as_ref();
                referenced != "time"
                    && class.components.get(referenced).is_none_or(|component| {
                        matches!(
                            component.variability,
                            Variability::Parameter(_) | Variability::Constant(_)
                        )
                    })
            }
            _ => true,
        })
}

/// MLS §14.3 / OPREC-009: for operator record classes C and D, at most one of
/// C.'constructor'(d) and D.'constructor'(c) shall be legal. Reject when both
/// records declare constructors taking the other record.
pub(super) fn check_operator_constructor_pairing(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    let records: Vec<&ClassDef> = class
        .classes
        .iter()
        .map(|(_, nested)| nested)
        .filter(|nested| nested.operator_record)
        .collect();
    let _ = def;
    for (i, c) in records.iter().enumerate() {
        for d in records.iter().skip(i + 1) {
            if constructor_takes(c, d.name.text.as_ref())
                && constructor_takes(d, c.name.text.as_ref())
            {
                diags.push(semantic_error(
                    ER125_OPERATOR_CONSTRUCTOR_PAIR,
                    format!(
                        "operator records '{}' and '{}' both declare constructors taking the other; at most one direction may be legal (MLS §14.3)",
                        c.name.text, d.name.text
                    ),
                    label_from_token(
                        &c.name,
                        "restrictions/operator_constructor_pairing",
                        "remove one of the cross-constructors",
                    ),
                ));
            }
        }
    }
}

/// True when any declared dimension of the component is a literal zero.
fn component_has_literal_zero_dimension(comp: &ast::Component) -> bool {
    comp.shape.contains(&0)
        || comp.shape_expr.iter().any(|sub| {
            matches!(
                sub,
                Subscript::Expression(Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token,
                    ..
                }) if token.text.as_ref() == "0"
            )
        })
}

/// Whether the operator record declares a 'constructor' operator with an
/// input of the named type.
fn constructor_takes(record: &ClassDef, other: &str) -> bool {
    let Some(ctor) = record.classes.get("'constructor'") else {
        return false;
    };
    let functions: Vec<&ClassDef> = if ctor.class_type == ClassType::Function {
        vec![ctor]
    } else {
        ctor.classes
            .iter()
            .map(|(_, nested)| nested)
            .filter(|nested| nested.class_type == ClassType::Function)
            .collect()
    };
    functions.iter().any(|function| {
        function
            .components
            .iter()
            .filter(|(_, comp)| is_input(comp))
            .any(|(_, comp)| {
                let type_name = comp.type_name.to_string();
                type_name == other || type_name.ends_with(&format!(".{other}"))
            })
    })
}

/// Whether the component's declared type resolves (through alias chains) to
/// a class carrying array subscripts (`type Vector3 = Real[3]`).
fn type_alias_is_array(comp: &ast::Component, def: &StoredDefinition) -> bool {
    let mut current = comp
        .type_def_id
        .and_then(|def_id| find_class_by_def_id(def, def_id));
    for _ in 0..8 {
        let Some(class) = current else {
            return false;
        };
        if !class.array_subscripts.is_empty() {
            return true;
        }
        let Some(ext) = class.extends.first() else {
            return false;
        };
        current = ext
            .base_def_id
            .and_then(|def_id| find_class_by_def_id(def, def_id));
    }
    false
}
