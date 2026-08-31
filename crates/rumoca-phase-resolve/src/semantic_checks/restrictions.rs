//! Structural restriction checks: evaluable-context rules for equations
//! (MLS §8.3), function purity/default rules (MLS §12), and declaration /
//! inheritance restrictions (MLS §4, §5, §7).

mod decl;

use super::*;
use decl::*;
use rumoca_eval_ast::ast_scalar::{self, AstScalarContext};
use std::cell::RefCell;

pub(super) const ER083_CONNECT_EVALUABLE_CONTEXT: &str = "ER083";
pub(super) const ER084_WHEN_EVALUABLE_CONTEXT: &str = "ER084";
pub(super) const ER085_CONNECT_SUBSCRIPTS: &str = "ER085";
pub(super) const ER086_ASSERT_LEVEL_EVALUABLE: &str = "ER086";
pub(super) const ER087_FOR_LHS_INDICES: &str = "ER087";
pub(super) const ER088_IMPURE_CALL_CONTEXT: &str = "ER088";
pub(super) const ER089_PURE_EXTENDS_IMPURE: &str = "ER089";
pub(super) const ER090_INPUT_DEFAULT_DEPENDENCY: &str = "ER090";
pub(super) const ER091_INHERITANCE_COMPATIBILITY: &str = "ER091";
pub(super) const ER092_OPERATOR_PLACEMENT: &str = "ER092";
pub(super) const ER093_DECLARATION_EQUATION_CLASS: &str = "ER093";
pub(super) const ER094_EXTENDS_BUILTIN_WITH_ELEMENTS: &str = "ER094";
pub(super) const ER095_INNER_OUTER_CONNECTOR_INPUT: &str = "ER095";
pub(super) const ER099_CARDINALITY_CONTEXT: &str = "ER099";
pub(super) const ER101_CONNECT_MULTIPLE_SOURCES: &str = "ER101";
pub(super) const ER102_ASSIGN_TO_PARAMETER: &str = "ER102";
pub(super) const ER103_CONNECT_OUTER_PAIR: &str = "ER103";
pub(super) const ER104_EACH_ON_SCALAR: &str = "ER104";
pub(super) const ER105_MODIFICATION_SIZE_MISMATCH: &str = "ER105";
pub(super) const ER106_BREAK_TARGET_KIND: &str = "ER106";
pub(super) const ER107_ENUM_CONVERSION_RANGE: &str = "ER107";
pub(super) const ER108_WHEN_TARGET_IN_SUBCOMPONENT: &str = "ER108";
pub(super) const ER110_OPERATOR_FUNCTION_DEFAULTS: &str = "ER110";
pub(super) const ER111_ASSIGN_TO_CLASS_INSTANCE: &str = "ER111";
pub(super) const ER112_AMBIGUOUS_UNQUALIFIED_IMPORT: &str = "ER112";
pub(super) const ER113_CONNECT_PROTECTED_CONNECTOR: &str = "ER113";
pub(super) const ER114_EVENT_IN_WHILE: &str = "ER114";
pub(super) const ER115_EVENT_FOR_INDEX_EVALUABLE: &str = "ER115";
pub(super) const ER116_EVENT_ITERATOR_EVALUABLE: &str = "ER116";
pub(super) const ER117_EQUALITY_CONSTRAINT_PROTOTYPE: &str = "ER117";
pub(super) const ER118_OVERDETERMINED_FLOW_MEMBER: &str = "ER118";
pub(super) const ER120_DERIVATIVE_ANNOTATION: &str = "ER120";
pub(super) const WR001_EXTERNAL_PURITY_UNDECLARED: &str = "WR001";
pub(super) const WR003_ANNOTATION_CONTEXT: &str = "WR003";
pub(super) const WR004_TESTCASE_USAGE: &str = "WR004";
pub(super) const ER122_AMBIGUOUS_OPERATOR_OVERLOAD: &str = "ER122";
pub(super) const ER123_CLASS_EXTENDS_NON_REPLACEABLE: &str = "ER123";
pub(super) const WR005_EVALUATE_NOT_EVALUABLE: &str = "WR005";
pub(super) const ER124_NONEVAL_NESTED_FOR_RANGE: &str = "ER124";
pub(super) const ER125_OPERATOR_CONSTRUCTOR_PAIR: &str = "ER125";
pub(super) const ER129_IMPLICIT_FOR_RANGE_UNSUPPORTED: &str = "ER129";

pub(super) fn run_restriction_semantic_checks(
    def: &StoredDefinition,
    connection_operators: &ast::ConnectionOperatorCatalog,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let evaluability = ComponentEvaluabilityIndex::new(def);
    check_unsupported_implicit_for_ranges(def, &mut diags);
    for (_, class) in &def.classes {
        check_class_restrictions(
            class,
            def,
            &evaluability,
            connection_operators,
            &mut Vec::new(),
            &mut diags,
        );
    }
    diags
}

/// CONN-024/CONN-025 checks require resolved declaration and predefined-type
/// identities. A potentially legal extent that depends on an effective
/// occurrence modifier remains provisional here and is completed by
/// Instantiate; a settled invalid or provably non-constant extent is rejected.
pub(super) fn run_resolved_equality_constraint_checks(tree: &ast::ClassTree) -> Vec<Diagnostic> {
    let evaluability = ComponentEvaluabilityIndex::new(&tree.definitions);
    let declarations = ast::EqualityConstraintDeclarationIndex::new(tree);
    let mut diagnostics = Vec::new();
    for class in tree.definitions.classes.values() {
        check_resolved_equality_constraint_tree(
            tree,
            class,
            &declarations,
            &evaluability,
            &mut diagnostics,
        );
    }
    diagnostics
}

fn check_resolved_equality_constraint_tree(
    tree: &ast::ClassTree,
    class: &ClassDef,
    declarations: &ast::EqualityConstraintDeclarationIndex,
    evaluability: &ComponentEvaluabilityIndex<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_equality_constraint_prototype(tree, class, declarations, evaluability, diagnostics);
    for nested in class.classes.values() {
        check_resolved_equality_constraint_tree(
            tree,
            nested,
            declarations,
            evaluability,
            diagnostics,
        );
    }
}

fn check_class_restrictions(
    class: &ClassDef,
    def: &StoredDefinition,
    evaluability: &ComponentEvaluabilityIndex<'_>,
    connection_operators: &ast::ConnectionOperatorCatalog,
    ancestors: &mut Vec<ClassContext>,
    diags: &mut Vec<Diagnostic>,
) {
    check_inheritance_compatibility(class, def, diags);
    check_operator_placement(class, ancestors, diags);
    check_extends_builtin_with_elements(class, def, diags);

    if class.class_type == ClassType::Function {
        check_pure_extends_impure(class, def, diags);
        check_function_input_defaults(class, diags);
        check_function_binding_cycles(class, diags);
        check_operator_function_defaults(class, ancestors, diags);
        check_derivative_annotations(class, def, ancestors, diags);
    } else {
        check_component_restrictions(class, def, diags);
        check_modification_restrictions(class, def, diags);
        check_break_target_kinds(class, def, diags);
        let mut scan = EquationScan {
            class,
            def,
            evaluability,
            connection_operators,
            diags,
            iterators: HashSet::new(),
        };
        scan.scan_equations(&class.equations, EqContext::default());
        scan.scan_equations(
            &class.initial_equations,
            EqContext {
                in_initial: true,
                ..EqContext::default()
            },
        );
        check_impure_bindings(class, def, diags);
        check_model_algorithm_statements(class, def, diags);
        check_enum_conversion_ranges(class, def, diags);
        check_event_generating_iterators(class, def, diags);
    }
    check_ambiguous_unqualified_imports(class, def, diags);
    check_annotation_advisories(class, def, diags);
    check_ambiguous_operator_overloads(class, diags);
    check_evaluate_annotations(class, diags);
    check_operator_constructor_pairing(class, def, diags);
    check_class_extends_redeclare(class, def, ancestors, diags);

    ancestors.push(ClassContext {
        def_id: class.def_id,
        class_type: class.class_type.clone(),
        operator_record: class.operator_record,
    });
    for (_, nested) in &class.classes {
        check_class_restrictions(
            nested,
            def,
            evaluability,
            connection_operators,
            ancestors,
            diags,
        );
    }
    ancestors.pop();
}

struct ClassContext {
    def_id: Option<DefId>,
    class_type: ClassType,
    operator_record: bool,
}

#[derive(Clone, Copy, Default)]
struct EqContext {
    /// Inside an if-equation whose condition is not evaluable during
    /// translation.
    in_noneval_if: bool,
    /// Inside a when-equation body.
    in_when: bool,
    /// Inside a for-equation body.
    in_for: bool,
    /// Inside the initial-equation section.
    in_initial: bool,
}

struct EquationScan<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    evaluability: &'a ComponentEvaluabilityIndex<'a>,
    connection_operators: &'a ast::ConnectionOperatorCatalog,
    diags: &'a mut Vec<Diagnostic>,
    iterators: HashSet<String>,
}

impl EquationScan<'_> {
    fn scan_equations(&mut self, equations: &[Equation], ctx: EqContext) {
        for eq in equations {
            self.scan_equation(eq, ctx);
        }
    }

    fn scan_equation(&mut self, eq: &Equation, ctx: EqContext) {
        match eq {
            Equation::Connect { lhs, rhs } => {
                self.check_connect_context(lhs, ctx);
                self.check_connect_subscripts(lhs);
                self.check_connect_subscripts(rhs);
                self.check_connect_sources(lhs, rhs);
                self.check_connect_outer_pair(lhs, rhs);
                self.check_connect_protected(lhs);
                self.check_connect_protected(rhs);
            }
            Equation::FunctionCall { comp, args, .. } => {
                match comp
                    .target_def_id()
                    .and_then(|declaration| self.connection_operators.role(declaration))
                {
                    Some(
                        rumoca_core::ConnectionGraphOperatorRole::Branch
                        | rumoca_core::ConnectionGraphOperatorRole::Root
                        | rumoca_core::ConnectionGraphOperatorRole::PotentialRoot,
                    ) => self.check_connect_context(comp, ctx),
                    Some(
                        rumoca_core::ConnectionGraphOperatorRole::IsRoot
                        | rumoca_core::ConnectionGraphOperatorRole::Rooted,
                    )
                    | None => {}
                }
                if comp.parts.len() == 1 && comp.parts[0].ident.text.as_ref() == "assert" {
                    self.check_assert_level(args);
                }
                self.check_impure_call_in_equation(comp, args, ctx);
            }
            Equation::Simple { lhs, rhs } => {
                if ctx.in_for
                    && let Expression::ComponentReference(comp) = lhs
                {
                    self.check_for_lhs_indices(comp);
                }
                if ctx.in_when
                    && let Expression::ComponentReference(comp) = lhs
                {
                    self.check_when_target_scope(comp);
                }
                self.check_impure_in_expression(lhs, ctx);
                self.check_impure_in_expression(rhs, ctx);
                self.check_cardinality_in_expression(lhs);
                self.check_cardinality_in_expression(rhs);
            }
            Equation::For {
                indices, equations, ..
            } => {
                // MLS §8.3.4 / EQN-028: inside an if-equation with a
                // non-evaluable condition, nested for-equations must have
                // evaluable ranges so per-branch equation counts are static.
                if ctx.in_noneval_if {
                    self.check_noneval_nested_for_ranges(indices);
                }
                let added: Vec<String> = indices
                    .iter()
                    .filter(|index| self.iterators.insert(index.ident.text.to_string()))
                    .map(|index| index.ident.text.to_string())
                    .collect();
                self.scan_equations(
                    equations,
                    EqContext {
                        in_for: true,
                        ..ctx
                    },
                );
                for name in added {
                    self.iterators.remove(&name);
                }
            }
            Equation::When(blocks) => self.scan_when_equation(blocks, ctx),
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                let evaluable = cond_blocks
                    .iter()
                    .all(|block| self.is_evaluable_expression(&block.cond));
                let inner = EqContext {
                    in_noneval_if: ctx.in_noneval_if || !evaluable,
                    ..ctx
                };
                for block in cond_blocks {
                    self.scan_equations(&block.eqs, inner);
                }
                if let Some(else_eqs) = else_block {
                    self.scan_equations(else_eqs, inner);
                }
            }
            Equation::Assert {
                level: Some(level_expr),
                ..
            } => {
                self.check_assert_level_expression(level_expr);
            }
            _ => {}
        }
    }

    /// MLS §8.3.7 / EQN-036: the optional assertionLevel argument (third
    /// positional or named `level`) must be evaluable during translation.
    fn check_assert_level(&mut self, args: &[Expression]) {
        let named = args.iter().find_map(|arg| match arg {
            Expression::NamedArgument { name, value, .. } if name.text.as_ref() == "level" => {
                Some(value.as_ref())
            }
            _ => None,
        });
        let level = named.or_else(|| {
            args.iter()
                .filter(|arg| !matches!(arg, Expression::NamedArgument { .. }))
                .nth(2)
        });
        if let Some(level_expr) = level {
            self.check_assert_level_expression(level_expr);
        }
    }

    fn check_assert_level_expression(&mut self, level_expr: &Expression) {
        if !self.is_evaluable_expression(level_expr)
            && let Some(token) = first_expression_token(Some(level_expr))
        {
            self.diags.push(semantic_error(
                ER086_ASSERT_LEVEL_EVALUABLE,
                "assert() level argument must be evaluable during translation (MLS §8.3.7)"
                    .to_string(),
                label_from_token(
                    &token,
                    "restrictions/assert_level",
                    "assertion level must be an evaluable expression",
                ),
            ));
        }
    }

    /// MLS §8.3.3-§8.3.4 / EQN-019, EQN-026, EQN-038: connect() and
    /// Connections.* may not appear inside when-equations or if-equations
    /// with non-evaluable conditions.
    fn check_connect_context(&mut self, comp: &ComponentReference, ctx: EqContext) {
        let context_name = if ctx.in_when {
            "when-equation"
        } else if ctx.in_noneval_if {
            "if-equation with non-evaluable condition"
        } else {
            return;
        };
        let Some(token) = comp.parts.first().map(|part| &part.ident) else {
            return;
        };
        self.diags.push(semantic_error(
            ER083_CONNECT_EVALUABLE_CONTEXT,
            format!("connection statement is not allowed inside a {context_name} (MLS §8.3.3)"),
            label_from_token(
                token,
                "restrictions/connect_context",
                "connections require evaluable enclosing conditions",
            ),
        ));
    }

    /// MLS §9.3 / CONN-019: connector reference subscripts in connect() must
    /// be evaluable expressions (or the special operator `:`).
    fn check_connect_subscripts(&mut self, comp: &ComponentReference) {
        for part in non_evaluable_subscript_parts(self, comp) {
            self.diags.push(semantic_error(
                ER085_CONNECT_SUBSCRIPTS,
                format!(
                    "connect() subscript on '{}' must be evaluable during translation (MLS §9.3)",
                    part.text
                ),
                label_from_token(
                    &part,
                    "restrictions/connect_subscripts",
                    "connector subscripts must be parameter expressions",
                ),
            ));
        }
    }

    /// MLS §8.3.5 / EQN-014: indices on the left-hand side of equations in
    /// for-equation bodies must be evaluable expressions.
    fn check_for_lhs_indices(&mut self, comp: &ComponentReference) {
        for part in non_evaluable_subscript_parts(self, comp) {
            self.diags.push(semantic_error(
                ER087_FOR_LHS_INDICES,
                format!(
                    "left-hand side index on '{}' must be evaluable during translation (MLS §8.3.5)",
                    part.text
                ),
                label_from_token(
                    &part,
                    "restrictions/for_lhs_indices",
                    "equation left-hand side indices must be evaluable",
                ),
            ));
        }
    }

    /// MLS §8.3.4 / EQN-028 helper: flag non-evaluable nested for ranges.
    fn check_noneval_nested_for_ranges(&mut self, indices: &[ast::ForIndex]) {
        for index in indices {
            if !self.is_evaluable_expression(&index.range) {
                self.diags.push(semantic_error(
                    ER124_NONEVAL_NESTED_FOR_RANGE,
                    "for-equation inside an if-equation with non-evaluable condition must have an evaluable range (MLS §8.3.4)"
                        .to_string(),
                    label_from_token(
                        &index.ident,
                        "restrictions/noneval_nested_for_range",
                        "use a parameter or constant iteration range",
                    ),
                ));
            }
        }
    }

    fn scan_when_equation(&mut self, blocks: &[ast::EquationBlock], ctx: EqContext) {
        if ctx.in_noneval_if
            && let Some(token) = first_expression_token(blocks.first().map(|b| &b.cond))
        {
            // MLS §8.3.5.2 / EQN-030: when-equations may only occur inside
            // if-equations with evaluable conditions.
            self.diags.push(semantic_error(
                ER084_WHEN_EVALUABLE_CONTEXT,
                "when-equation inside an if-equation with non-evaluable condition (MLS §8.3.5.2)"
                    .to_string(),
                label_from_token(
                    &token,
                    "restrictions/when_context",
                    "enclosing if-equation condition must be evaluable during translation",
                ),
            ));
        }
        for block in blocks {
            self.scan_equations(
                &block.eqs,
                EqContext {
                    in_when: true,
                    ..ctx
                },
            );
        }
    }

    /// MLS §9.2 / CONN-016: a protected connector of a sub-component cannot
    /// be connected from outside.
    fn check_connect_protected(&mut self, comp: &ComponentReference) {
        if comp.parts.len() < 2 {
            return;
        }
        let Some(target) = resolve_component_reference_target(self.class, comp, self.def) else {
            return;
        };
        if target.component.is_protected {
            self.diags.push(semantic_error(
                ER113_CONNECT_PROTECTED_CONNECTOR,
                format!(
                    "cannot connect to protected connector '{}' from outside its class (MLS §9.2)",
                    reference_text(comp)
                ),
                label_from_token(
                    target.token,
                    "restrictions/connect_protected",
                    "protected connectors are only connectable inside their declaring class",
                ),
            ));
        }
    }

    /// MLS §9.2 / CONN-004: a connection set may contain at most one source
    /// (inside output connector or public outside input connector).
    fn check_connect_sources(&mut self, lhs: &ComponentReference, rhs: &ComponentReference) {
        let Some(lhs_source) = connect_endpoint_is_source(self.class, lhs, self.def) else {
            return;
        };
        let Some(rhs_source) = connect_endpoint_is_source(self.class, rhs, self.def) else {
            return;
        };
        if lhs_source
            && rhs_source
            && let Some(token) = lhs.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER101_CONNECT_MULTIPLE_SOURCES,
                format!(
                    "connect({}, {}): both endpoints are inside output connectors; a connection set may have at most one source (MLS §9.2)",
                    reference_text(lhs),
                    reference_text(rhs)
                ),
                label_from_token(
                    token,
                    "restrictions/connect_sources",
                    "both connect() endpoints drive the connection",
                ),
            ));
        }
    }

    /// MLS §9.2 / CONN-006: two connectors of outer elements cannot be
    /// connected.
    fn check_connect_outer_pair(&mut self, lhs: &ComponentReference, rhs: &ComponentReference) {
        let outer_root = |comp: &ComponentReference| {
            comp.parts.first().is_some_and(|part| {
                self.class
                    .components
                    .get(part.ident.text.as_ref())
                    .is_some_and(|component| component.outer)
            })
        };
        if outer_root(lhs)
            && outer_root(rhs)
            && let Some(token) = lhs.parts.first().map(|part| &part.ident)
        {
            self.diags.push(semantic_error(
                ER103_CONNECT_OUTER_PAIR,
                "cannot connect two connectors of outer elements (MLS §9.2)".to_string(),
                label_from_token(
                    token,
                    "restrictions/connect_outer_pair",
                    "both connect() endpoints reference outer components",
                ),
            ));
        }
    }

    /// MLS §4.5.3 / DECL-033: a variable assigned in a when-clause shall not
    /// live inside a model/block sub-component.
    fn check_when_target_scope(&mut self, comp: &ComponentReference) {
        if comp.parts.len() < 2 {
            return;
        }
        let Some(first) = comp.parts.first() else {
            return;
        };
        let Some(component) = self.class.components.get(first.ident.text.as_ref()) else {
            return;
        };
        let Some(ResolvedTypeRoot::Class(type_class)) =
            resolve_component_type_root(component, self.def)
        else {
            return;
        };
        if matches!(type_class.class_type, ClassType::Model | ClassType::Block) {
            self.diags.push(semantic_error(
                ER108_WHEN_TARGET_IN_SUBCOMPONENT,
                format!(
                    "when-clause assigns '{}' inside {} sub-component '{}' (MLS §4.5.3)",
                    reference_text(comp),
                    type_class.class_type.as_str(),
                    first.ident.text
                ),
                label_from_token(
                    &first.ident,
                    "restrictions/when_target_scope",
                    "when-clause targets must be declared in the same model or block",
                ),
            ));
        }
    }

    /// MLS §3.7.5.3 / EXPR-032: cardinality() may only be used in conditions
    /// of assert and if-statements, not in plain equations.
    fn check_cardinality_in_expression(&mut self, expr: &Expression) {
        let mut collector = NamedCallCollector {
            name: "cardinality",
            found: Vec::new(),
        };
        let _visit_outcome = collector.visit_expression(expr);
        for token in collector.found {
            self.diags.push(semantic_error(
                ER099_CARDINALITY_CONTEXT,
                "cardinality() may only be used in assert or if-equation conditions (MLS §3.7.5.3)"
                    .to_string(),
                label_from_token(
                    &token,
                    "restrictions/cardinality_context",
                    "cardinality() is not allowed in plain equations",
                ),
            ));
        }
    }

    /// MLS §12.3 / FUNC-022: calls to impure functions in plain equations.
    fn check_impure_call_in_equation(
        &mut self,
        comp: &ComponentReference,
        _args: &[Expression],
        ctx: EqContext,
    ) {
        if ctx.in_when || ctx.in_initial {
            return;
        }
        if !is_impure_function_call(comp, self.class, self.def) {
            return;
        }
        let Some(token) = comp.parts.first().map(|part| &part.ident) else {
            return;
        };
        self.diags.push(semantic_error(
            ER088_IMPURE_CALL_CONTEXT,
            format!(
                "impure function '{}' may only be called from an impure function, a \
                 `when` equation or statement, an initial equation or initial algorithm, or a \
                 `parameter` binding (MLS §12.3)",
                reference_text(comp)
            ),
            label_from_token(
                token,
                "restrictions/impure_call_context",
                "impure call in a continuous-time equation",
            ),
        ));
    }

    fn check_impure_in_expression(&mut self, expr: &Expression, ctx: EqContext) {
        if ctx.in_when || ctx.in_initial {
            return;
        }
        let mut collector = ImpureCallCollector {
            class: self.class,
            def: self.def,
            found: Vec::new(),
        };
        let _visit_outcome = collector.visit_expression(expr);
        for (name, token) in collector.found {
            self.diags.push(semantic_error(
                ER088_IMPURE_CALL_CONTEXT,
                format!(
                    "impure function '{name}' may only be called from an impure function, a \
                     `when` equation or statement, an initial equation or initial algorithm, or a \
                     `parameter` binding (MLS §12.3)"
                ),
                label_from_token(
                    &token,
                    "restrictions/impure_call_context",
                    "impure call in a continuous-time equation",
                ),
            ));
        }
    }

    /// Translation-time evaluability follows the resolved declaration identity
    /// of every component reference. Reference depth is only syntax: a
    /// qualified continuous variable is no more structural than an unqualified
    /// one. `size()`/`ndims()` of a declared array are structural, so their
    /// array argument does not affect evaluability (MLS §3.7.2).
    fn is_evaluable_expression(&self, expr: &Expression) -> bool {
        let mut walker = EvaluableExprWalker {
            scan: self,
            evaluable: true,
        };
        let _visit_outcome = walker.visit_expression(expr);
        walker.evaluable
    }

    /// Evaluability of one component reference (see
    /// [`Self::is_evaluable_expression`] for the classification rules).
    fn is_evaluable_component_ref(&self, cref: &ComponentReference) -> bool {
        if let [part] = cref.parts.as_slice() {
            let name = part.ident.text.as_ref();
            if name == "time" {
                return false;
            }
            if self.iterators.contains(name) {
                return true;
            }
        }
        if cref.parts.len() > 1 && cref.root_def_id() == cref.target_def_id() {
            // Enumeration literals carry their owning enum type's DefId on
            // both the root and literal segment; the literal has no separate
            // component declaration (registration.rs).
            return true;
        }
        let mut proof = EvaluationProof::Evaluable;
        let mut saw_component = false;
        for (index, part) in cref.parts.iter().enumerate() {
            let Some(def_id) = part.def_id else {
                continue;
            };
            let Some(part_proof) = self.evaluability.proof(def_id) else {
                continue;
            };
            let is_target = index + 1 == cref.parts.len();
            if !is_target && !self.evaluability.variability_propagates(def_id) {
                // Ordinary model/block/record instances are namespace
                // containers here. Their fields keep their own variability;
                // only a parameter/constant aggregate propagates structural
                // evaluability (and its exclusions) to the selected field.
                continue;
            }
            saw_component = true;
            proof = proof.and(part_proof);
            if proof == EvaluationProof::NonEvaluable {
                return false;
            }
        }
        if saw_component {
            return proof != EvaluationProof::NonEvaluable;
        }
        match cref.parts.as_slice() {
            [part] => self
                .class
                .components
                .get(part.ident.text.as_ref())
                .is_none_or(|component| {
                    component
                        .def_id
                        .and_then(|def_id| self.evaluability.proof(def_id))
                        != Some(EvaluationProof::NonEvaluable)
                }),
            // A tail deferred across a replaceable/redeclared root has no
            // target identity yet. That is unknown here, not proof of a
            // continuous dependency; instantiation owns the fail-closed check
            // after selecting the concrete type.
            _ => true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EvaluationProof {
    Evaluable,
    Unknown,
    NonEvaluable,
}

impl EvaluationProof {
    fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::NonEvaluable, _) | (_, Self::NonEvaluable) => Self::NonEvaluable,
            (Self::Unknown, _) | (_, Self::Unknown) => Self::Unknown,
            (Self::Evaluable, Self::Evaluable) => Self::Evaluable,
        }
    }
}

struct ComponentEvaluabilityIndex<'a> {
    proofs: HashMap<DefId, EvaluationProof>,
    components: HashMap<DefId, &'a ast::Component>,
}

impl<'a> ComponentEvaluabilityIndex<'a> {
    fn new(def: &'a StoredDefinition) -> Self {
        let mut components = HashMap::new();
        let mut scalar_type_ids = HashSet::new();
        let mut attribute_references = AttributeReferenceIndex::default();
        for class in def.classes.values() {
            collect_evaluability_declarations(
                class,
                &mut components,
                &mut scalar_type_ids,
                &mut attribute_references,
            );
        }
        let mut proofs = HashMap::new();
        for def_id in components.keys().copied().collect::<Vec<_>>() {
            let mut visiting = HashSet::new();
            compute_component_evaluability(
                def_id,
                &components,
                &scalar_type_ids,
                &attribute_references,
                &mut proofs,
                &mut visiting,
            );
        }
        Self { proofs, components }
    }

    fn proof(&self, def_id: DefId) -> Option<EvaluationProof> {
        self.proofs.get(&def_id).copied()
    }

    fn variability_propagates(&self, def_id: DefId) -> bool {
        self.components.get(&def_id).is_some_and(|component| {
            matches!(
                component.variability,
                Variability::Parameter(_) | Variability::Constant(_)
            )
        })
    }

    fn expression_is_definitively_nonevaluable(&self, expression: &Expression) -> bool {
        ast::collect_component_refs(expression)
            .iter()
            .any(|reference| {
                reference.parts.iter().any(|part| {
                    part.def_id.and_then(|def_id| self.proof(def_id))
                        == Some(EvaluationProof::NonEvaluable)
                }) || reference.parts.len() == 1 && reference.parts[0].ident.text.as_ref() == "time"
            })
    }
}

fn collect_evaluability_declarations<'a>(
    class: &'a ClassDef,
    components: &mut HashMap<DefId, &'a ast::Component>,
    scalar_type_ids: &mut HashSet<DefId>,
    attribute_references: &mut AttributeReferenceIndex,
) {
    if class.class_type == ClassType::Type
        && let Some(def_id) = class.def_id
    {
        scalar_type_ids.insert(def_id);
    }
    for component in class.components.values() {
        if let Some(def_id) = component.def_id {
            components.insert(def_id, component);
            if let Some(owner) = class.def_id {
                attribute_references
                    .owner_by_component
                    .insert(def_id, owner);
                attribute_references
                    .component_by_owner_name
                    .insert((owner, component.name.clone()), def_id);
            }
        }
    }
    for nested in class.classes.values() {
        collect_evaluability_declarations(
            nested,
            components,
            scalar_type_ids,
            attribute_references,
        );
    }
}

#[derive(Default)]
struct AttributeReferenceIndex {
    owner_by_component: HashMap<DefId, DefId>,
    component_by_owner_name: HashMap<(DefId, String), DefId>,
}

fn compute_component_evaluability(
    def_id: DefId,
    components: &HashMap<DefId, &ast::Component>,
    scalar_type_ids: &HashSet<DefId>,
    attribute_references: &AttributeReferenceIndex,
    proofs: &mut HashMap<DefId, EvaluationProof>,
    visiting: &mut HashSet<DefId>,
) -> EvaluationProof {
    if let Some(proof) = proofs.get(&def_id) {
        return *proof;
    }
    if !visiting.insert(def_id) {
        return EvaluationProof::NonEvaluable;
    }
    let proof = components
        .get(&def_id)
        .map_or(EvaluationProof::Unknown, |component| {
            component_declaration_evaluability(
                component,
                components,
                scalar_type_ids,
                attribute_references,
                proofs,
                visiting,
            )
        });
    visiting.remove(&def_id);
    proofs.insert(def_id, proof);
    proof
}

fn component_declaration_evaluability(
    component: &ast::Component,
    components: &HashMap<DefId, &ast::Component>,
    scalar_type_ids: &HashSet<DefId>,
    attribute_references: &AttributeReferenceIndex,
    proofs: &mut HashMap<DefId, EvaluationProof>,
    visiting: &mut HashSet<DefId>,
) -> EvaluationProof {
    match component.variability {
        Variability::Constant(_) => component.binding.as_ref().map_or_else(
            || missing_structural_binding_proof(component, scalar_type_ids),
            |binding| {
                binding_evaluability(
                    binding,
                    components,
                    scalar_type_ids,
                    attribute_references,
                    proofs,
                    visiting,
                )
            },
        ),
        Variability::Parameter(_) => {
            if parameter_is_explicitly_nonevaluable(component, components, attribute_references) {
                return EvaluationProof::NonEvaluable;
            }
            component.binding.as_ref().map_or_else(
                || missing_structural_binding_proof(component, scalar_type_ids),
                |binding| {
                    binding_evaluability(
                        binding,
                        components,
                        scalar_type_ids,
                        attribute_references,
                        proofs,
                        visiting,
                    )
                },
            )
        }
        _ => EvaluationProof::NonEvaluable,
    }
}

fn missing_structural_binding_proof(
    component: &ast::Component,
    scalar_type_ids: &HashSet<DefId>,
) -> EvaluationProof {
    let type_name = component.type_name.to_string();
    let is_predefined_scalar = ["Real", "Integer", "Boolean", "String", "Clock"]
        .iter()
        .any(|builtin| rumoca_core::qualified_type_name_matches(&type_name, builtin));
    let is_resolved_scalar = component
        .type_def_id
        .is_some_and(|def_id| scalar_type_ids.contains(&def_id));
    if !is_predefined_scalar && !is_resolved_scalar {
        // A structured aggregate may obtain a field value from the selected
        // type's declaration or an occurrence modifier. Resolve deliberately
        // leaves that instance-dependent proof to Instantiate.
        EvaluationProof::Unknown
    } else {
        // A scalar parameter/constant without a declaration equation has no
        // translation-time value. Treating it as merely unknown would permit
        // it to select connection topology until a later phase guessed.
        EvaluationProof::NonEvaluable
    }
}

fn parameter_is_explicitly_nonevaluable(
    component: &ast::Component,
    components: &HashMap<DefId, &ast::Component>,
    attribute_references: &AttributeReferenceIndex,
) -> bool {
    !ResolveAttributeContext::new(component, components, attribute_references)
        .parameter_attributes_allow(component)
}

struct ResolveAttributeContext<'a> {
    components: &'a HashMap<DefId, &'a ast::Component>,
    references: &'a AttributeReferenceIndex,
    component_scope: RefCell<Vec<DefId>>,
    active_attributes: RefCell<HashSet<DefId>>,
    active_values: RefCell<HashSet<DefId>>,
}

impl<'a> ResolveAttributeContext<'a> {
    fn new(
        component: &ast::Component,
        components: &'a HashMap<DefId, &'a ast::Component>,
        references: &'a AttributeReferenceIndex,
    ) -> Self {
        Self {
            components,
            references,
            component_scope: RefCell::new(component.def_id.into_iter().collect()),
            active_attributes: RefCell::new(HashSet::new()),
            active_values: RefCell::new(HashSet::new()),
        }
    }

    fn parameter_attributes_allow(&self, component: &ast::Component) -> bool {
        let Some(def_id) = component.def_id else {
            return false;
        };
        if !self.active_attributes.borrow_mut().insert(def_id) {
            return false;
        }
        let allows = component
            .modifications
            .get("fixed")
            .is_none_or(|expression| self.proves_true(expression))
            && evaluate_annotation_expression(component)
                .is_none_or(|expression| self.proves_true(expression));
        self.active_attributes.borrow_mut().remove(&def_id);
        allows
    }

    fn proves_true(&self, expression: &Expression) -> bool {
        ast_scalar::eval_boolean(expression, self, "", 0) == Some(true)
    }

    fn referenced_component(&self, expression: &Expression) -> Option<(DefId, &ast::Component)> {
        let Expression::ComponentReference(reference) = expression else {
            return None;
        };
        let target_def_id = reference.target_def_id().or_else(|| {
            let [part] = reference.parts.as_slice() else {
                return None;
            };
            let current = self.component_scope.borrow().last().copied()?;
            let owner = self.references.owner_by_component.get(&current)?;
            self.references
                .component_by_owner_name
                .get(&(*owner, part.ident.text.to_string()))
                .copied()
        })?;
        for (index, part) in reference.parts.iter().enumerate() {
            let Some(def_id) = part.def_id else {
                continue;
            };
            let Some(component) = self.components.get(&def_id).copied() else {
                continue;
            };
            let is_target = index + 1 == reference.parts.len();
            if !is_target
                && !matches!(
                    component.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
            {
                continue;
            }
            match component.variability {
                Variability::Constant(_) => {}
                Variability::Parameter(_) if self.parameter_attributes_allow(component) => {}
                _ => return None,
            }
        }
        self.components
            .get(&target_def_id)
            .copied()
            .map(|component| (target_def_id, component))
    }

    fn evaluate_reference<T>(
        &self,
        expression: &Expression,
        depth: usize,
        eval: impl FnOnce(&Expression, &Self, usize) -> Option<T>,
    ) -> Option<T> {
        let (def_id, component) = self.referenced_component(expression)?;
        match component.variability {
            Variability::Constant(_) => {}
            Variability::Parameter(_) if self.parameter_attributes_allow(component) => {}
            _ => return None,
        }
        if !self.active_values.borrow_mut().insert(def_id) {
            return None;
        }
        let result = component.binding.as_ref().and_then(|binding| {
            self.component_scope.borrow_mut().push(def_id);
            let result = eval(binding, self, depth);
            self.component_scope.borrow_mut().pop();
            result
        });
        self.active_values.borrow_mut().remove(&def_id);
        result
    }
}

impl AstScalarContext for ResolveAttributeContext<'_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(20)
    }

    fn lookup_integer(&self, expression: &Expression, _scope: &str, depth: usize) -> Option<i64> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_integer(binding, ctx, "", depth)
        })
    }

    fn lookup_real(&self, expression: &Expression, _scope: &str, depth: usize) -> Option<f64> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_real(binding, ctx, "", depth)
        })
    }

    fn lookup_boolean(&self, expression: &Expression, _scope: &str, depth: usize) -> Option<bool> {
        self.evaluate_reference(expression, depth, |binding, ctx, depth| {
            ast_scalar::eval_boolean(binding, ctx, "", depth)
        })
    }

    fn call_integer(
        &self,
        _function: &ComponentReference,
        _args: &[Expression],
        _scope: &str,
        _depth: usize,
        _span: Span,
    ) -> Option<i64> {
        None
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: Span,
    ) -> Option<i64> {
        rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
    }
}

fn evaluate_annotation_expression(component: &ast::Component) -> Option<&Expression> {
    component.annotation.iter().find_map(|entry| match entry {
        Expression::Modification { target, value, .. }
            if reference_is_single_name(target, "Evaluate") =>
        {
            Some(value.as_ref())
        }
        Expression::NamedArgument { name, value, .. } if name.text.as_ref() == "Evaluate" => {
            Some(value.as_ref())
        }
        _ => None,
    })
}

fn reference_is_single_name(reference: &ComponentReference, expected: &str) -> bool {
    reference.parts.len() == 1
        && reference.parts[0].subs.is_none()
        && reference.parts[0].ident.text.as_ref() == expected
}

fn binding_evaluability(
    binding: &Expression,
    components: &HashMap<DefId, &ast::Component>,
    scalar_type_ids: &HashSet<DefId>,
    attribute_references: &AttributeReferenceIndex,
    proofs: &mut HashMap<DefId, EvaluationProof>,
    visiting: &mut HashSet<DefId>,
) -> EvaluationProof {
    let mut proof = EvaluationProof::Evaluable;
    let mut collector = BindingReferenceCollector::default();
    let _visit_outcome = collector.visit_expression(binding);
    for reference in collector.references {
        if reference.parts.len() == 1 && reference.parts[0].ident.text.as_ref() == "time" {
            return EvaluationProof::NonEvaluable;
        }
        if reference.parts.len() > 1 && reference.root_def_id() == reference.target_def_id() {
            continue;
        }
        let mut saw_component = false;
        for (index, part) in reference.parts.iter().enumerate() {
            let Some(def_id) = part.def_id else {
                continue;
            };
            let Some(component) = components.get(&def_id) else {
                continue;
            };
            let is_target = index + 1 == reference.parts.len();
            if !is_target
                && !matches!(
                    component.variability,
                    Variability::Parameter(_) | Variability::Constant(_)
                )
            {
                continue;
            }
            saw_component = true;
            proof = proof.and(compute_component_evaluability(
                def_id,
                components,
                scalar_type_ids,
                attribute_references,
                proofs,
                visiting,
            ));
            if proof == EvaluationProof::NonEvaluable {
                return proof;
            }
        }
        if !saw_component && reference.target_def_id().is_none() {
            proof = proof.and(EvaluationProof::Unknown);
        }
    }
    proof
}

#[derive(Default)]
struct BindingReferenceCollector {
    references: Vec<ComponentReference>,
}

impl ast::Visitor for BindingReferenceCollector {
    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> std::ops::ControlFlow<()> {
        if matches!(builtin_name(comp), Some("size" | "ndims")) {
            // The array value is irrelevant; only the optional dimension
            // argument participates in translation-time evaluability.
            return self.visit_each(args.get(1..).unwrap_or(&[]), Self::visit_expression);
        }
        self.visit_each(args, Self::visit_expression)
    }

    fn visit_component_reference(
        &mut self,
        reference: &ComponentReference,
    ) -> std::ops::ControlFlow<()> {
        self.references.push(reference.clone());
        ast::visitor::walk_component_reference_default(self, reference)
    }
}

/// Expression walker behind [`EquationScan::is_evaluable_expression`]: flags
/// non-evaluable component references while treating the array argument of
/// `size()`/`ndims()` as structural (only its declared dimensions matter).
struct EvaluableExprWalker<'a, 'b> {
    scan: &'a EquationScan<'b>,
    evaluable: bool,
}

impl ast::Visitor for EvaluableExprWalker<'_, '_> {
    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> std::ops::ControlFlow<()> {
        if matches!(builtin_name(comp), Some("size" | "ndims")) {
            // The array argument contributes only its declared dimensions;
            // the optional dimension-index argument must still be evaluable.
            return self.visit_each(args.get(1..).unwrap_or(&[]), Self::visit_expression);
        }
        self.visit_each(args, Self::visit_expression)
    }

    fn visit_component_reference(
        &mut self,
        cref: &ComponentReference,
    ) -> std::ops::ControlFlow<()> {
        if !self.scan.is_evaluable_component_ref(cref) {
            self.evaluable = false;
            return std::ops::ControlFlow::Break(());
        }
        ast::visitor::walk_component_reference_default(self, cref)
    }
}

/// Tokens of reference parts whose subscripts contain non-evaluable
/// expressions (shared by the connect-subscript and for-LHS-index rules).
fn non_evaluable_subscript_parts(scan: &EquationScan<'_>, comp: &ComponentReference) -> Vec<Token> {
    let mut parts = Vec::new();
    for part in &comp.parts {
        let Some(subs) = part.subs.as_ref() else {
            continue;
        };
        let has_non_evaluable = subs.iter().any(
            |sub| matches!(sub, Subscript::Expression(expr) if !scan.is_evaluable_expression(expr)),
        );
        if has_non_evaluable {
            parts.push(part.ident.clone());
        }
    }
    parts
}

struct ImpureCallCollector<'a> {
    class: &'a ClassDef,
    def: &'a StoredDefinition,
    found: Vec<(String, Token)>,
}

impl ast::Visitor for ImpureCallCollector<'_> {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> std::ops::ControlFlow<()> {
        if is_impure_function_call(comp, self.class, self.def)
            && let Some(token) = comp.parts.first().map(|part| part.ident.clone())
        {
            self.found.push((reference_text(comp), token));
        }
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }
}

/// True when the call target resolves (within this stored definition) to a
/// function class declared `impure`. Unresolvable names are assumed pure.
fn is_impure_function_call(
    comp: &ComponentReference,
    class: &ClassDef,
    def: &StoredDefinition,
) -> bool {
    let target = resolve_local_class_path(comp, class, def);
    target.is_some_and(|target| target.class_type == ClassType::Function && !target.pure)
}

/// Resolve a (possibly dotted) class path starting from the current class's
/// nested classes, then from the top-level classes of the stored definition.
fn resolve_local_class_path<'a>(
    comp: &ComponentReference,
    class: &'a ClassDef,
    def: &'a StoredDefinition,
) -> Option<&'a ClassDef> {
    let mut names = comp.parts.iter().map(|part| part.ident.text.as_ref());
    let first = names.next()?;
    let mut current = class
        .classes
        .get(first)
        .or_else(|| def.classes.get(first))?;
    for segment in names {
        current = current.classes.get(segment)?;
    }
    Some(current)
}

fn reference_text(comp: &ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".")
}

fn first_expression_token(expr: Option<&Expression>) -> Option<Token> {
    let cref = rumoca_ir_ast::collect_component_refs(expr?)
        .into_iter()
        .next()?;
    cref.parts.first().map(|part| part.ident.clone())
}

/// MLS §12.3 / FUNC-022: impure calls in component bindings outside functions.
fn check_impure_bindings(class: &ClassDef, def: &StoredDefinition, diags: &mut Vec<Diagnostic>) {
    for (_, comp) in &class.components {
        // MLS §12.3 lists "Binding equations for components declared as
        // parameter" among the legal contexts, "which is seen as syntactic
        // sugar for having a parameter with fixed=false and the binding as an
        // initial equation", so a parameter binding is skipped by the rule.
        //
        // A `constant` binding is not on that list. Accepting it would let an
        // impure effect enter a context whose value is required to be
        // translation-time invariant, so only the explicitly admitted
        // parameter form bypasses this rejection.
        if matches!(comp.variability, Variability::Parameter(_)) {
            continue;
        }
        let Some(binding) = comp.binding.as_ref() else {
            continue;
        };
        let mut collector = ImpureCallCollector {
            class,
            def,
            found: Vec::new(),
        };
        let _visit_outcome = collector.visit_expression(binding);
        for (name, token) in collector.found {
            diags.push(semantic_error(
                ER088_IMPURE_CALL_CONTEXT,
                format!(
                    "impure function '{name}' may only be called from an impure function, a \
                     `when` equation or statement, an initial equation or initial algorithm, or a \
                     `parameter` binding (MLS §12.3)"
                ),
                label_from_token(
                    &token,
                    "restrictions/impure_call_context",
                    "impure call in a continuous-time binding",
                ),
            ));
        }
    }
}

/// MLS §12.3 / FUNC-021: a function extending an impure function must itself
/// be declared impure.
fn check_pure_extends_impure(
    class: &ClassDef,
    def: &StoredDefinition,
    diags: &mut Vec<Diagnostic>,
) {
    if !class.pure {
        return;
    }
    for ext in &class.extends {
        let Some(def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base) = find_class_by_def_id(def, def_id) else {
            continue;
        };
        if base.class_type == ClassType::Function && !base.pure {
            diags.push(semantic_error(
                ER089_PURE_EXTENDS_IMPURE,
                format!(
                    "function '{}' extends impure function '{}' and must be declared impure (MLS §12.3)",
                    class.name.text, base.name.text
                ),
                label_from_token(
                    &class.name,
                    "restrictions/pure_extends_impure",
                    "add the impure keyword",
                ),
            ));
        }
    }
}

/// MLS §12.4.1 / FUNC-034: default values of function inputs must not depend
/// on non-input variables of the function.
fn check_function_input_defaults(class: &ClassDef, diags: &mut Vec<Diagnostic>) {
    for (_, comp) in &class.components {
        if !is_input(comp) {
            continue;
        }
        let Some(binding) = comp.binding.as_ref() else {
            continue;
        };
        for cref in rumoca_ir_ast::collect_component_refs(binding) {
            let [part] = cref.parts.as_slice() else {
                continue;
            };
            let name = part.ident.text.as_ref();
            let Some(referenced) = class.components.get(name) else {
                continue;
            };
            let allowed =
                is_input(referenced) || matches!(referenced.variability, Variability::Constant(_));
            if !allowed {
                diags.push(semantic_error(
                    ER090_INPUT_DEFAULT_DEPENDENCY,
                    format!(
                        "default of input '{}' depends on non-input '{}' of function '{}' (MLS §12.4.1)",
                        comp.name, name, class.name.text
                    ),
                    label_from_token(
                        &part.ident,
                        "restrictions/input_default_dependency",
                        "input defaults may reference only inputs and constants",
                    ),
                ));
            }
        }
    }
}

fn is_input(comp: &ast::Component) -> bool {
    matches!(comp.causality, Causality::Input(_))
}
