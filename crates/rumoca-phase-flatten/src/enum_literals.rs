//! Canonicalize enum literal references in flattened expressions.
//!
//! MLS §4.9.5: enumeration literals are constants of an enumeration type.
//! Flat output should keep enum literal references resolvable by emitting
//! canonical qualification paths where possible.

use rumoca_core::{ComponentPath, ExpressionRewriter, Reference, StatementRewriter, VarName};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use rustc_hash::{FxHashMap, FxHashSet};

/// Canonicalize enum literal references in the final flat model.
pub(crate) fn canonicalize_flat_enum_literals(
    flat: &mut flat::Model,
    tree: &ast::ClassTree,
    known_enum_values: &FxHashMap<String, String>,
) {
    let enum_literals = EnumLiteralIndex::new(tree, known_enum_values);
    if enum_literals.is_empty() {
        return;
    }

    let variable_names: FxHashSet<VarName> = flat.variables.keys().cloned().collect();

    for var in flat.variables.values_mut() {
        canonicalize_optional_expr(var.binding.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.start.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.min.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.max.as_mut(), &enum_literals, &variable_names);
        canonicalize_optional_expr(var.nominal.as_mut(), &enum_literals, &variable_names);
    }

    for eq in &mut flat.equations {
        canonicalize_expr(&mut eq.residual, &enum_literals, &variable_names);
    }
    for eq in &mut flat.initial_equations {
        canonicalize_expr(&mut eq.residual, &enum_literals, &variable_names);
    }

    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            canonicalize_expr(&mut branch.condition, &enum_literals, &variable_names);
            canonicalize_when_equations(&mut branch.equations, &enum_literals, &variable_names);
        }
    }

    for algorithm in &mut flat.algorithms {
        canonicalize_statements(&mut algorithm.statements, &enum_literals, &variable_names);
    }
    for algorithm in &mut flat.initial_algorithms {
        canonicalize_statements(&mut algorithm.statements, &enum_literals, &variable_names);
    }
}

#[derive(Debug, Default)]
struct EnumLiteralIndex {
    by_identity: FxHashMap<(rumoca_core::DefId, String), String>,
}

impl EnumLiteralIndex {
    fn new(tree: &ast::ClassTree, known_enum_values: &FxHashMap<String, String>) -> Self {
        let mut index = Self::default();
        for value in known_enum_values.values() {
            index.record_resolved_value(tree, value);
        }
        for (def_id, qualified_name) in &tree.def_map {
            let Some(class_def) = tree.get_class_by_def_id(*def_id) else {
                continue;
            };
            if class_def.enum_literals.is_empty() {
                continue;
            }
            for literal in &class_def.enum_literals {
                let literal_name = literal.ident.text.as_ref();
                index.record_identity(
                    *def_id,
                    literal_name,
                    format!("{qualified_name}.{literal_name}"),
                );
            }
        }

        index
    }

    fn is_empty(&self) -> bool {
        self.by_identity.is_empty()
    }

    fn record_resolved_value(&mut self, tree: &ast::ClassTree, value: &str) {
        let path = ComponentPath::from_flat_path(value);
        let Some(literal_name) = path.parts().last() else {
            return;
        };
        let Some(type_path) = path.prefix(path.len().saturating_sub(1)) else {
            return;
        };
        let Some(def_id) = tree.name_map.get(type_path.as_str()).copied().or_else(|| {
            tree.def_map
                .iter()
                .find_map(|(def_id, name)| (name == type_path.as_str()).then_some(*def_id))
        }) else {
            return;
        };
        let Some(class_def) = tree.get_class_by_def_id(def_id) else {
            return;
        };
        if !class_def
            .enum_literals
            .iter()
            .any(|literal| literal.ident.text.as_ref() == literal_name)
        {
            return;
        }
        self.record_identity(def_id, literal_name, value.to_string());
    }

    /// A colliding key names the same literal of the same enum class (the
    /// `DefId` pins the exact declaration), so entries can differ only in
    /// rendered spelling — e.g. the declaration path versus a use-site path
    /// from `known_enum_values`. Preferring the longest spelling
    /// deterministically selects the most qualified display form; it cannot
    /// change which literal is meant.
    fn record_identity(&mut self, def_id: rumoca_core::DefId, literal: &str, value: String) {
        let key = (def_id, literal.to_string());
        match self.by_identity.entry(key) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(value);
            }
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let current = entry.get_mut();
                let current_rank = (
                    ComponentPath::from_flat_path(current).len(),
                    current.as_str(),
                );
                let candidate_rank = (ComponentPath::from_flat_path(&value).len(), value.as_str());
                if candidate_rank > current_rank {
                    *current = value;
                }
            }
        }
    }

    fn canonicalize<'index>(&'index self, literal: &Reference) -> Option<&'index str> {
        let reference = literal.component_ref()?;
        let literal_name = reference.parts().last()?.ident.clone();
        self.by_identity
            .get(&(reference.target_def_id(), literal_name))
            .map(String::as_str)
    }
}

fn canonicalize_optional_expr(
    expr: Option<&mut rumoca_core::Expression>,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    if let Some(expr) = expr {
        canonicalize_expr(expr, enum_literals, variable_names);
    }
}

fn canonicalize_expr(
    expr: &mut rumoca_core::Expression,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            canonicalize_var_ref_if_enum_literal(
                name,
                subscripts.is_empty(),
                enum_literals,
                variable_names,
            );
            for sub in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = sub {
                    canonicalize_expr(expr, enum_literals, variable_names);
                }
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            canonicalize_expr(lhs, enum_literals, variable_names);
            canonicalize_expr(rhs, enum_literals, variable_names);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            canonicalize_expr(rhs, enum_literals, variable_names);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                canonicalize_expr(arg, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::StringConversion { value, format, .. } => {
            canonicalize_expr(value, enum_literals, variable_names);
            canonicalize_string_conversion_format(format, enum_literals, variable_names);
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, then_expr) in branches {
                canonicalize_expr(cond, enum_literals, variable_names);
                canonicalize_expr(then_expr, enum_literals, variable_names);
            }
            canonicalize_expr(else_branch, enum_literals, variable_names);
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for elem in elements {
                canonicalize_expr(elem, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            canonicalize_expr(start, enum_literals, variable_names);
            if let Some(step) = step {
                canonicalize_expr(step, enum_literals, variable_names);
            }
            canonicalize_expr(end, enum_literals, variable_names);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            for index in indices {
                canonicalize_expr(&mut index.range, enum_literals, variable_names);
            }
            canonicalize_expr(expr, enum_literals, variable_names);
            if let Some(filter) = filter {
                canonicalize_expr(filter, enum_literals, variable_names);
            }
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            canonicalize_expr(base, enum_literals, variable_names);
            for sub in subscripts {
                if let rumoca_core::Subscript::Expr { expr, .. } = sub {
                    canonicalize_expr(expr, enum_literals, variable_names);
                }
            }
        }
        rumoca_core::Expression::FieldAccess { base, .. } => {
            canonicalize_expr(base, enum_literals, variable_names);
        }
        rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => {}
    }
}

fn canonicalize_string_conversion_format(
    format: &mut rumoca_core::StringConversionFormat,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    match format {
        rumoca_core::StringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => {
            for operand in [minimum_length, left_justified, significant_digits]
                .into_iter()
                .flatten()
            {
                canonicalize_expr(operand, enum_literals, variable_names);
            }
        }
        rumoca_core::StringConversionFormat::Format { value } => {
            canonicalize_expr(value, enum_literals, variable_names);
        }
    }
}

fn canonicalize_var_ref_if_enum_literal(
    name: &mut rumoca_core::Reference,
    scalar_ref: bool,
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    if !scalar_ref {
        return;
    }

    let raw = name.as_str();
    if variable_names.contains(name.var_name()) || name.component_ref().is_none() {
        return;
    }

    if let Some(canonical) = enum_literals.canonicalize(name)
        && canonical != raw
    {
        *name = name.with_var_name(VarName::new(canonical));
    }
}

fn canonicalize_when_equations(
    equations: &mut [flat::WhenEquation],
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    for equation in equations.iter_mut() {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_expr(value, enum_literals, variable_names);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                canonicalize_expr(condition, enum_literals, variable_names);
                canonicalize_expr(message, enum_literals, variable_names);
                if let Some(level) = level {
                    canonicalize_expr(level, enum_literals, variable_names);
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_expr(message, enum_literals, variable_names);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_expr(condition, enum_literals, variable_names);
                    canonicalize_when_equations(branch_equations, enum_literals, variable_names);
                }
                if let Some(else_branch) = else_branch {
                    canonicalize_when_equations(else_branch, enum_literals, variable_names);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_expr(function, enum_literals, variable_names);
            }
        }
    }
}

fn canonicalize_statements(
    statements: &mut [rumoca_core::Statement],
    enum_literals: &EnumLiteralIndex,
    variable_names: &FxHashSet<VarName>,
) {
    let mut rewriter = EnumLiteralStatementCanonicalizer {
        enum_literals,
        variable_names,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

struct EnumLiteralStatementCanonicalizer<'a> {
    enum_literals: &'a EnumLiteralIndex,
    variable_names: &'a FxHashSet<VarName>,
}

impl ExpressionRewriter for EnumLiteralStatementCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let mut rewritten = expr.clone();
        canonicalize_expr(&mut rewritten, self.enum_literals, self.variable_names);
        rewritten
    }
}

impl StatementRewriter for EnumLiteralStatementCanonicalizer<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("enum_literals_test.mo"),
            1,
            2,
        )
    }

    fn resolved_reference(
        parts: &[&str],
        def_id: rumoca_core::DefId,
        span: Span,
    ) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference::construct(
            false,
            span,
            parts
                .iter()
                .map(|part| rumoca_core::ComponentRefPart {
                    ident: (*part).to_string(),
                    span,
                    subs: Vec::new(),
                    def_id,
                })
                .collect(),
        )
        .expect("test reference is nonempty and resolved")
    }

    fn var_ref(
        display: &str,
        parts: &[&str],
        def_id: rumoca_core::DefId,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                display,
                resolved_reference(parts, def_id, test_span()),
            ),
            subscripts: vec![],
            span: test_span(),
        }
    }

    /// Build a class tree holding one enum type with one literal.
    ///
    /// The enum's identity is given structurally: the optionally enclosing
    /// package and the enum class name. The canonical dotted spelling is
    /// derived from those parts, never parsed back out of display text.
    fn enum_tree(
        def_id: rumoca_core::DefId,
        enclosing_package: Option<&str>,
        class_name: &str,
        literal: &str,
    ) -> ast::ClassTree {
        let mut tree = ast::ClassTree::new();
        let canonical_type = match enclosing_package {
            Some(package_name) => format!("{package_name}.{class_name}"),
            None => class_name.to_string(),
        };
        let mut enum_class = ast::ClassDef {
            name: rumoca_core::Token {
                text: class_name.into(),
                ..Default::default()
            },
            class_type: rumoca_core::ClassType::Type,
            def_id: Some(def_id),
            ..Default::default()
        };
        enum_class.enum_literals.push(ast::EnumLiteral {
            ident: rumoca_core::Token {
                text: literal.into(),
                ..Default::default()
            },
            description: Vec::new(),
        });
        if let Some(package_name) = enclosing_package {
            let package_def_id = rumoca_core::DefId::new(def_id.index() + 1_000);
            let mut package = ast::ClassDef {
                name: rumoca_core::Token {
                    text: package_name.into(),
                    ..Default::default()
                },
                class_type: rumoca_core::ClassType::Package,
                def_id: Some(package_def_id),
                ..Default::default()
            };
            package.classes.insert(class_name.to_string(), enum_class);
            tree.definitions
                .classes
                .insert(package_name.to_string(), package);
            tree.def_map
                .insert(package_def_id, package_name.to_string());
            tree.name_map
                .insert(package_name.to_string(), package_def_id);
        } else {
            tree.definitions
                .classes
                .insert(class_name.to_string(), enum_class);
        }
        tree.def_map.insert(def_id, canonical_type.clone());
        tree.name_map.insert(canonical_type, def_id);
        tree.name_map.insert(class_name.to_string(), def_id);
        tree
    }

    #[test]
    fn rewrites_short_enum_literal_to_most_qualified_candidate() {
        let enum_def_id = rumoca_core::DefId::new(41);
        let variable_def_id = rumoca_core::DefId::new(42);
        let tree = enum_tree(enum_def_id, Some("TypesPkg"), "Init", "NoInit");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("y"),
            flat::Variable {
                binding: Some(var_ref("Init.NoInit", &["Init", "NoInit"], enum_def_id)),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.equations.push(rumoca_ir_flat::Equation::new(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Eq,
                lhs: Box::new(var_ref("y", &["y"], variable_def_id)),
                rhs: Box::new(var_ref("Init.NoInit", &["Init", "NoInit"], enum_def_id)),
                span: test_span(),
            },
            test_span(),
            rumoca_ir_flat::EquationOrigin::Binding {
                variable: "y".to_string(),
            },
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert("enumParam".to_string(), "TypesPkg.Init.NoInit".to_string());
        // Also include short form to ensure canonicalization prefers the most-qualified path.
        known_enums.insert("short".to_string(), "Init.NoInit".to_string());

        canonicalize_flat_enum_literals(&mut flat, &tree, &known_enums);

        let binding = flat
            .variables
            .get(&rumoca_core::VarName::new("y"))
            .and_then(|var| var.binding.as_ref())
            .expect("binding should exist");
        let rumoca_core::Expression::VarRef { name, .. } = binding else {
            panic!("binding should remain a var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");

        let rumoca_core::Expression::Binary { rhs, .. } = &flat.equations[0].residual else {
            panic!("expected binary equation");
        };
        let rumoca_core::Expression::VarRef { name, .. } = rhs.as_ref() else {
            panic!("rhs should be enum literal var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");
    }

    #[test]
    fn canonical_spelling_preserves_resolved_enum_identity_and_source_occurrence() {
        let source = "model M\n  L a(start=L.U);\nend M;\n";
        let source_id = rumoca_core::SourceId::from_source_name("enum_identity.mo");
        let start = source.find("L.U").expect("fixture contains enum literal");
        let occurrence = Span::from_offsets(source_id, start, start + "L.U".len());
        let enum_type = rumoca_core::DefId::new(42);
        let mut tree = enum_tree(enum_type, None, "L", "U");
        tree.name_map.insert("P.L".to_string(), enum_type);
        let resolved_reference = resolved_reference(&["L", "U"], enum_type, occurrence);
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("a"),
            flat::Variable {
                start: Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::with_component_reference(
                        "L.U",
                        resolved_reference.clone(),
                    ),
                    subscripts: vec![],
                    span: occurrence,
                }),
                ..flat::Variable::empty_with_span(occurrence)
            },
        );
        let mut known_enums = FxHashMap::default();
        known_enums.insert("a".to_string(), "P.L.U".to_string());

        canonicalize_flat_enum_literals(&mut flat, &tree, &known_enums);

        let rumoca_core::Expression::VarRef { name, span, .. } = flat
            .variables
            .get(&rumoca_core::VarName::new("a"))
            .and_then(|variable| variable.start.as_ref())
            .expect("start expression is retained")
        else {
            panic!("start expression remains an enum literal reference");
        };
        assert_eq!(name.as_str(), "P.L.U");
        assert_eq!(name.component_ref(), Some(&resolved_reference));
        assert_eq!(name.target_def_id(), Some(enum_type));
        assert_eq!(*span, occurrence);
        assert_eq!(&source[span.start.0..span.end.0], "L.U");
    }

    #[test]
    fn does_not_rewrite_regular_variable_references() {
        let variable_def_id = rumoca_core::DefId::new(51);
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("Plant.Init.NoInit"),
            flat::Variable {
                binding: Some(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                }),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        flat.equations.push(rumoca_ir_flat::Equation::new(
            var_ref("Plant.Init.NoInit", &["Plant.Init.NoInit"], variable_def_id),
            test_span(),
            rumoca_ir_flat::EquationOrigin::Binding {
                variable: "x".to_string(),
            },
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert(
            "enumParam".to_string(),
            "Modelica.Blocks.Types.Init.NoInit".to_string(),
        );

        canonicalize_flat_enum_literals(&mut flat, &ast::ClassTree::new(), &known_enums);

        let rumoca_core::Expression::VarRef { name, .. } = &flat.equations[0].residual else {
            panic!("expected var ref");
        };
        assert_eq!(name.as_str(), "Plant.Init.NoInit");
    }

    #[test]
    fn rewrites_enum_literals_inside_algorithm_statements() {
        let enum_def_id = rumoca_core::DefId::new(61);
        let output_def_id = rumoca_core::DefId::new(62);
        let tree = enum_tree(enum_def_id, Some("TypesPkg"), "Init", "NoInit");
        let mut flat = flat::Model::new();
        flat.algorithms.push(flat::Algorithm::new(
            vec![rumoca_core::Statement::Assignment {
                comp: resolved_reference(&["y"], output_def_id, test_span()),
                value: var_ref("Init.NoInit", &["Init", "NoInit"], enum_def_id),
                span: test_span(),
            }],
            test_span(),
            "test",
        ));

        let mut known_enums = FxHashMap::default();
        known_enums.insert("enumParam".to_string(), "TypesPkg.Init.NoInit".to_string());

        canonicalize_flat_enum_literals(&mut flat, &tree, &known_enums);

        let rumoca_core::Statement::Assignment { value, .. } = &flat.algorithms[0].statements[0]
        else {
            panic!("expected assignment statement");
        };
        let rumoca_core::Expression::VarRef { name, .. } = value else {
            panic!("expected enum literal var ref");
        };
        assert_eq!(name.as_str(), "TypesPkg.Init.NoInit");
    }
}
