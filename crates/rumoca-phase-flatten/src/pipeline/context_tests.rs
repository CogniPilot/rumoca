use super::*;

#[cfg(test)]
mod tests {
    mod dimension_inference;
    mod enum_and_overrides;

    use super::*;
    use rumoca_core::InstanceId;
    use rumoca_core::{ClassType, DefId};
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::{ClassDef, ClassTree, InstanceData};
    use rumoca_ir_flat as flat;
    use std::sync::Arc;

    const TEST_FILE: &str = "context_tests.mo";

    fn test_real_type() -> rumoca_core::TypeId {
        rumoca_core::TypeId::new(1)
    }

    fn test_integer_type() -> rumoca_core::TypeId {
        rumoca_core::TypeId::new(2)
    }

    fn resolved_enum_value(
        owner: rumoca_core::DefId,
        type_name: &str,
        literal: &str,
    ) -> rumoca_eval_flat::constant::ResolvedEnumValue {
        let catalog = rumoca_eval_flat::constant::ResolvedEnumCatalog::try_from_declarations(vec![
            rumoca_eval_flat::constant::ResolvedEnumDeclaration {
                declaration: owner,
                type_name: type_name.to_string(),
                literals: vec![literal.to_string()],
            },
        ])
        .unwrap();
        catalog.get(owner, literal).unwrap().clone()
    }

    fn enum_display(ctx: &Context, name: &str) -> Option<String> {
        let value = ctx.enum_parameter_values.get(name)?;
        Some(crate::boolean_eval::resolved_enum_display_name(value))
    }

    fn typed_flat_model() -> flat::Model {
        flat::Model {
            predefined_types: flat::PredefinedTypeIds {
                real: test_real_type(),
                integer: test_integer_type(),
                boolean: rumoca_core::TypeId::new(3),
                string: rumoca_core::TypeId::new(4),
                clock: rumoca_core::TypeId::new(5),
            },
            ..flat::Model::default()
        }
    }

    fn test_source_location() -> rumoca_core::Location {
        rumoca_core::Location {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 2,
            start: 0,
            end: 1,
            source: rumoca_core::SourceId::from_source_name(TEST_FILE),
        }
    }

    fn add_test_source(tree: &mut ClassTree) {
        tree.source_map.add(TEST_FILE, "component x;");
    }

    fn source_backed_tree() -> ClassTree {
        let mut tree = ClassTree::default();
        add_test_source(&mut tree);
        tree
    }

    fn symbolic_instance(
        instance_id: InstanceId,
        qualified_name: &str,
        dims_expr: Vec<ast::Subscript>,
    ) -> InstanceData {
        InstanceData {
            declaration_def_id: None,
            owner_class_id: None,
            instance_id,
            component_ref: None,
            qualified_name: QualifiedName::from_dotted(qualified_name),
            source_location: test_source_location(),
            dims: Vec::new(),
            dims_expr,
            type_id: rumoca_core::TypeId::default(),
            type_name: String::new(),
            type_def_id: None,
            type_reference_root_def_id: None,
            declaration_source_scope: None,
            class_overrides: ast::ClassOverrideMap::default(),
            has_forwarding_class_redeclare: false,
            had_redeclare: false,
            variability: rumoca_core::Variability::Empty,
            causality: rumoca_core::Causality::Empty,
            flow: false,
            stream: false,
            start: None,
            fixed: None,
            min: None,
            max: None,
            nominal: None,
            quantity: None,
            unit: None,
            display_unit: None,
            description: None,
            state_select: rumoca_core::StateSelect::default(),
            binding: None,
            binding_source: None,
            binding_source_scope: None,
            attribute_source_scopes: ast::AstIndexMap::default(),
            binding_from_modification: false,
            is_primitive: true,
            is_discrete_type: false,
            from_expandable_connector: false,
            evaluate: false,
            is_final: false,
            is_protected: false,
            is_connector_type: false,
            is_expandable_connector_type: false,
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("flatten_context_test.mo"),
            8,
            32,
        )
    }

    fn test_enum_type() -> rumoca_core::TypeId {
        rumoca_core::TypeId::new(91)
    }

    fn test_enum_tree() -> ClassTree {
        let enum_def_id = DefId::new(601);
        let mut enum_class = ClassDef {
            name: token("Dynamics"),
            class_type: ClassType::Type,
            def_id: Some(enum_def_id),
            ..ClassDef::default()
        };
        for literal in ["SteadyState", "SteadyStateInitial"] {
            enum_class.enum_literals.push(ast::EnumLiteral {
                ident: token(literal),
                description: Vec::new(),
            });
        }
        let mut tree = ClassTree::new();
        tree.definitions
            .classes
            .insert("Dynamics".to_string(), enum_class);
        tree.def_map.insert(enum_def_id, "Dynamics".to_string());
        tree.name_map.insert("Dynamics".to_string(), enum_def_id);
        tree
    }

    fn enum_literal_expr(literal: &str, literal_def: DefId) -> Expression {
        Expression::VarRef {
            name: core_reference(&[("Dynamics", DefId::new(601)), (literal, literal_def)]),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn parameter_reference_expr(parts: &[(&str, DefId)], instance_id: InstanceId) -> Expression {
        Expression::VarRef {
            name: core_reference(parts).with_instance_id(instance_id),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn add_enum_parameter(
        flat: &mut flat::Model,
        name: &str,
        parts: &[(&str, DefId)],
        instance_id: InstanceId,
        binding: Expression,
    ) {
        flat.enumeration_types.insert(test_enum_type());
        let variable_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            variable_name.clone(),
            flat::Variable {
                name: variable_name,
                type_id: test_enum_type(),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(binding),
                component_ref: Some(core_component_ref(parts)),
                instance_id,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    fn add_boolean_parameter(
        flat: &mut flat::Model,
        name: &str,
        parts: &[(&str, DefId)],
        instance_id: InstanceId,
        value: bool,
    ) {
        let variable_name = rumoca_core::VarName::new(name);
        flat.add_variable(
            variable_name.clone(),
            flat::Variable {
                name: variable_name,
                type_id: flat.predefined_types.boolean,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Boolean(value),
                    span: test_span(),
                }),
                component_ref: Some(core_component_ref(parts)),
                instance_id,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    fn build_test_parameter_context(flat: &flat::Model) -> Context {
        let mut ctx = Context::new();
        ctx.build_parameter_lookup(flat, &test_enum_tree())
            .expect("resolved test parameter inventory must evaluate");
        ctx
    }

    #[test]
    fn public_parameter_lookup_closes_a_twelve_node_exact_dependency_chain() {
        let mut flat = typed_flat_model();
        for index in 0..12_u32 {
            let name = rumoca_core::VarName::new(format!("p{index}"));
            let def_id = DefId::new(7_000 + index);
            let instance_id = InstanceId::new(8_000 + index);
            let binding = if index == 0 {
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(17),
                    span: test_span(),
                }
            } else {
                let previous = index - 1;
                Expression::VarRef {
                    name: core_reference(&[(
                        &format!("p{previous}"),
                        DefId::new(7_000 + previous),
                    )])
                    .with_instance_id(InstanceId::new(8_000 + previous)),
                    subscripts: Vec::new(),
                    span: test_span(),
                }
            };
            flat.add_variable(
                name.clone(),
                flat::Variable {
                    name,
                    type_id: test_integer_type(),
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    component_ref: Some(core_component_ref(&[(&format!("p{index}"), def_id)])),
                    instance_id,
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        let mut context = Context::new();
        context
            .build_parameter_lookup(&flat, &ClassTree::default())
            .expect("exact dependency closure has no depth cap");
        assert_eq!(context.parameter_values.get("p11"), Some(&17));
    }

    #[test]
    fn public_parameter_lookup_rejects_an_exact_dependency_cycle() {
        let mut flat = typed_flat_model();
        for (name, def_id, instance_id, target, target_def, target_instance) in [
            ("left", 9_001, 9_101, "right", 9_002, 9_102),
            ("right", 9_002, 9_102, "left", 9_001, 9_101),
        ] {
            let name = rumoca_core::VarName::new(name);
            let component_ref = core_component_ref(&[(name.as_str(), DefId::new(def_id))]);
            flat.add_variable(
                name.clone(),
                flat::Variable {
                    name,
                    type_id: test_integer_type(),
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(Expression::VarRef {
                        name: core_reference(&[(target, DefId::new(target_def))])
                            .with_instance_id(InstanceId::new(target_instance)),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    component_ref: Some(component_ref),
                    instance_id: InstanceId::new(instance_id),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        let mut context = Context::new();
        assert!(
            context
                .build_parameter_lookup(&flat, &ClassTree::default())
                .is_err()
        );
    }

    fn component_ref_expr(path: &str) -> ast::Expression {
        let parts = crate::path_utils::segments(path)
            .into_iter()
            .map(|segment| ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from(segment),
                    ..rumoca_core::Token::default()
                },
                subs: None,
                def_id: None,
            })
            .collect();

        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts,
            span: test_span(),
            qualified_display_name: None,
        })
    }

    fn resolved_component_ref_expr(parts: &[(&str, DefId)]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|(name, def_id)| ast::ComponentRefPart {
                    ident: token(name),
                    subs: None,
                    def_id: Some(*def_id),
                })
                .collect(),
            span: test_span(),
            qualified_display_name: None,
        })
    }

    fn core_component_ref(parts: &[(&str, DefId)]) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            parts
                .iter()
                .map(|(ident, def_id)| rumoca_core::ComponentRefPart {
                    ident: (*ident).to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: *def_id,
                })
                .collect(),
        )
        .expect("test Flat reference carries exact per-segment identities")
    }

    fn core_reference(parts: &[(&str, DefId)]) -> rumoca_core::Reference {
        let display = parts
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
            .join(".");
        rumoca_core::Reference::with_component_reference(&display, core_component_ref(parts))
    }

    fn token(name: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(name.to_string()),
            ..rumoca_core::Token::default()
        }
    }

    fn int_lit(value: i64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: test_span(),
        }
    }

    fn real_lit(value: f64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: test_span(),
        }
    }

    fn var_ref(parts: &[(&str, DefId)], instance_id: InstanceId) -> Expression {
        Expression::VarRef {
            name: core_reference(parts).with_instance_id(instance_id),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn div_expr(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(),
        }
    }

    fn mul_expr(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: test_span(),
        }
    }

    fn sqrt_expr(arg: Expression) -> Expression {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sqrt,
            args: vec![arg],
            span: test_span(),
        }
    }

    fn int_array(values: &[i64]) -> Expression {
        Expression::Array {
            elements: values.iter().copied().map(int_lit).collect(),
            is_matrix: false,
            span: test_span(),
        }
    }

    fn fill_expr(value: i64, dims: &[i64]) -> Expression {
        let mut args = vec![int_lit(value)];
        args.extend(dims.iter().copied().map(int_lit));
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            span: test_span(),
        }
    }

    fn size_dim_expr(parts: &[(&str, DefId)], instance_id: InstanceId, dim: i64) -> Expression {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![var_ref(parts, instance_id), int_lit(dim)],
            span: test_span(),
        }
    }

    #[test]
    fn test_is_in_disabled_component_handles_dot_inside_bracket_expression() {
        let qn = QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
                ("v".to_string(), Vec::new()),
            ],
        };
        let mut disabled = indexmap::IndexSet::new();
        disabled.insert(rumoca_core::ComponentPath::from_parts([
            "bus[data.medium]",
            "pin",
        ]));

        assert!(is_in_disabled_component(&qn, &disabled));
    }

    #[test]
    fn test_is_in_disabled_component_ignores_non_matching_bracket_expression() {
        let qn = QualifiedName {
            parts: vec![
                ("bus[data.medium]".to_string(), Vec::new()),
                ("pin".to_string(), Vec::new()),
                ("v".to_string(), Vec::new()),
            ],
        };
        let mut disabled = indexmap::IndexSet::new();
        disabled.insert(rumoca_core::ComponentPath::from_parts([
            "bus[data.other]",
            "pin",
        ]));

        assert!(!is_in_disabled_component(&qn, &disabled));
    }

    #[test]
    fn post_resolve_parameter_without_occurrence_identity_fails_before_evaluation() {
        let mut ctx = Context::new();
        let mut flat = typed_flat_model();
        let name = rumoca_core::VarName::new("model.n");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                type_id: test_integer_type(),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Empty { span: test_span() }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let error = ctx
            .build_parameter_lookup(&flat, &ClassTree::default())
            .expect_err("post-Resolve parameter inventory requires exact occurrence identity");
        assert!(matches!(
            error,
            FlattenError::MissingFlatVariableIdentity { .. }
        ));
    }

    #[test]
    fn unbound_post_resolve_variable_without_component_identity_is_not_omitted() {
        let mut flat = typed_flat_model();
        let name = rumoca_core::VarName::new("model.unbound");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                type_id: test_integer_type(),
                variability: rumoca_core::Variability::Empty,
                binding: None,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let error = Context::new()
            .build_parameter_lookup(&flat, &ClassTree::default())
            .expect_err("an unbound post-Resolve variable still requires component identity");
        assert!(matches!(
            error,
            FlattenError::MissingFlatVariableIdentity { .. }
        ));
    }

    #[test]
    fn post_resolve_enum_dependency_cycle_is_rejected_by_occurrence_identity() {
        let mut flat = typed_flat_model();
        let enum_type = rumoca_core::TypeId::new(92);
        flat.enumeration_types.insert(enum_type);
        let a_def = DefId::new(701);
        let b_def = DefId::new(702);
        let a_instance = InstanceId::new(801);
        let b_instance = InstanceId::new(802);
        for (name, own_def, own_instance, target, target_def, target_instance) in [
            ("a", a_def, a_instance, "b", b_def, b_instance),
            ("b", b_def, b_instance, "a", a_def, a_instance),
        ] {
            let variable_name = rumoca_core::VarName::new(name);
            flat.add_variable(
                variable_name.clone(),
                flat::Variable {
                    name: variable_name,
                    type_id: enum_type,
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(Expression::VarRef {
                        name: rumoca_core::Reference::with_component_reference(
                            target,
                            core_component_ref(&[(target, target_def)]),
                        )
                        .with_instance_id(target_instance),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    component_ref: Some(core_component_ref(&[(name, own_def)])),
                    instance_id: own_instance,
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }
        let error = Context::new()
            .build_parameter_lookup(&flat, &ClassTree::default())
            .expect_err("exact enum dependency cycle cannot settle");
        assert!(matches!(error, FlattenError::Internal(_)));
    }

    #[test]
    fn runtime_dependent_parameter_binding_remains_deferred() {
        let mut ctx = Context::new();
        let mut flat = typed_flat_model();
        let name = rumoca_core::VarName::new("model.n");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                type_id: test_integer_type(),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::VarRef {
                    name: core_reference(&[("runtime_value", DefId::new(20_000))])
                        .with_instance_id(InstanceId::new(21_000)),
                    subscripts: Vec::new(),
                    span: test_span(),
                }),
                component_ref: Some(core_component_ref(&[("model.n", DefId::new(20_001))])),
                instance_id: InstanceId::new(21_001),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        ctx.build_parameter_lookup(&flat, &ClassTree::default())
            .expect("a runtime-dependent parameter is a valid deferred value");
        assert_eq!(ctx.get_integer_param("model.n"), None);
    }

    #[test]
    fn real_modifier_bindings_resolve_in_enclosing_scope_for_sibling_instances() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = typed_flat_model();

        // A modification written in the enclosing scope keeps that scope's
        // qualification, so both siblings reach the one `td` declaration.
        for (index, (name, binding, from_modification)) in [
            ("td", real_lit(0.002), false),
            (
                "line1.TD",
                div_expr(
                    var_ref(&[("td", DefId::new(1))], InstanceId::new(1)),
                    int_lit(2),
                ),
                true,
            ),
            (
                "line2.TD",
                div_expr(
                    var_ref(&[("td", DefId::new(1))], InstanceId::new(1)),
                    int_lit(2),
                ),
                true,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let var_name = rumoca_core::VarName::new(name);
            let declaration = DefId::new(1 + index as u32);
            flat.add_variable(
                var_name.clone(),
                flat::Variable {
                    name: var_name,
                    type_id: test_real_type(),
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    binding_from_modification: from_modification,
                    component_ref: Some(core_component_ref(&[(name, declaration)])),
                    instance_id: InstanceId::new(1 + index as u32),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        assert_eq!(ctx.real_parameter_values.get("line1.TD"), Some(&0.001));
        assert_eq!(ctx.real_parameter_values.get("line2.TD"), Some(&0.001));
    }

    #[test]
    fn a_modifier_binding_qualified_with_its_own_instance_is_not_recovered() {
        // `line2.td` names a member of `line2`, and no declaration provides
        // one — the value lives on the enclosing `td`. Reading this binding
        // used to work only by stripping path segments until something matched,
        // which is a guess about which declaration was meant. A rendering this
        // wrong has to be corrected where it is produced, so the binding now
        // stays unevaluated instead of silently resolving to a neighbour.
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = typed_flat_model();

        for (index, (name, binding, from_modification)) in [
            ("td", real_lit(0.002), false),
            (
                "line2.TD",
                div_expr(
                    var_ref(
                        &[("line2", DefId::new(2)), ("td", DefId::new(3))],
                        InstanceId::new(3),
                    ),
                    int_lit(2),
                ),
                true,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let var_name = rumoca_core::VarName::new(name);
            let declaration = DefId::new(1 + index as u32);
            flat.add_variable(
                var_name.clone(),
                flat::Variable {
                    name: var_name,
                    type_id: test_real_type(),
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    binding_from_modification: from_modification,
                    component_ref: Some(core_component_ref(&[(name, declaration)])),
                    instance_id: InstanceId::new(1 + index as u32),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        assert_eq!(ctx.real_parameter_values.get("td"), Some(&0.002));
        assert_eq!(ctx.real_parameter_values.get("line2.TD"), None);
    }

    #[test]
    fn real_modifier_bindings_resolve_transmission_line_delay_chain() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = typed_flat_model();

        let c0 = div_expr(
            real_lit(1.0),
            sqrt_expr(mul_expr(
                var_ref(&[("l", DefId::new(1))], InstanceId::new(1)),
                var_ref(&[("c", DefId::new(2))], InstanceId::new(2)),
            )),
        );
        let td = div_expr(
            var_ref(&[("len", DefId::new(3))], InstanceId::new(3)),
            var_ref(&[("c0", DefId::new(4))], InstanceId::new(4)),
        );
        for (index, (name, binding, from_modification)) in [
            ("l", real_lit(1.0e-6), false),
            ("c", real_lit(15.0e-12), false),
            ("len", real_lit(100.0e3), false),
            ("c0", c0, false),
            ("td", td, false),
            (
                "line1.TD",
                div_expr(
                    var_ref(&[("td", DefId::new(5))], InstanceId::new(5)),
                    int_lit(2),
                ),
                true,
            ),
            (
                "line2.TD",
                div_expr(
                    var_ref(&[("td", DefId::new(5))], InstanceId::new(5)),
                    int_lit(2),
                ),
                true,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let var_name = rumoca_core::VarName::new(name);
            let declaration = DefId::new(1 + index as u32);
            flat.add_variable(
                var_name.clone(),
                flat::Variable {
                    name: var_name,
                    type_id: test_real_type(),
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    binding_from_modification: from_modification,
                    component_ref: Some(core_component_ref(&[(name, declaration)])),
                    instance_id: InstanceId::new(1 + index as u32),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        let expected = 100.0e3 * (1.0e-6_f64 * 15.0e-12_f64).sqrt() / 2.0;
        for name in ["line1.TD", "line2.TD"] {
            let value = ctx
                .real_parameter_values
                .get(name)
                .copied()
                .expect("line delay should evaluate");
            assert!((value - expected).abs() < 1.0e-15);
        }
    }

    #[test]
    fn test_get_integer_param_ignores_an_integral_real_without_integer_ownership() {
        let mut ctx = Context::new();
        ctx.real_parameter_values
            .insert("pipe.flowModel.n".to_string(), 2.0);

        assert_eq!(ctx.get_integer_param("pipe.flowModel.n"), None);
    }

    #[test]
    fn parameter_lookup_uses_canonical_type_to_keep_numeric_caches_disjoint() {
        let real = rumoca_core::TypeId::new(1);
        let integer = rumoca_core::TypeId::new(2);
        let boolean = rumoca_core::TypeId::new(3);
        let string = rumoca_core::TypeId::new(4);
        let clock = rumoca_core::TypeId::new(5);
        let effective_integer = rumoca_core::TypeId::new(20);
        let mut flat = flat::Model {
            predefined_types: flat::PredefinedTypeIds {
                real,
                integer,
                boolean,
                string,
                clock,
            },
            ..flat::Model::default()
        };
        flat.effective_types.insert(
            effective_integer,
            rumoca_core::EffectiveType::new(integer, integer, Vec::new())
                .expect("fixture Integer type is valid"),
        );
        let name = rumoca_core::VarName::new("maxWaypoints");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                type_id: effective_integer,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_lit(8)),
                component_ref: Some(core_component_ref(&[("maxWaypoints", DefId::new(20_400))])),
                instance_id: InstanceId::new(21_400),
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut ctx = Context::new();
        ctx.build_parameter_lookup(&flat, &ClassTree::default())
            .unwrap();

        assert_eq!(ctx.parameter_values.get("maxWaypoints"), Some(&8));
        assert!(
            !ctx.real_parameter_values.contains_key("maxWaypoints"),
            "an Integer declaration must have exactly one typed cache owner"
        );
    }

    #[test]
    fn incomplete_type_metadata_does_not_guess_a_typed_cache_owner() {
        let mut flat = flat::Model::default();
        let name = rumoca_core::VarName::new("n");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_lit(8)),
                component_ref: Some(core_component_ref(&[("n", DefId::new(20_401))])),
                instance_id: InstanceId::new(21_401),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut ctx = Context::new();

        ctx.build_parameter_lookup(&flat, &ClassTree::default())
            .unwrap();

        assert!(!ctx.parameter_values.contains_key("n"));
        assert!(!ctx.real_parameter_values.contains_key("n"));
        assert!(!ctx.boolean_parameter_values.contains_key("n"));
        assert!(!ctx.enum_parameter_values.contains_key("n"));
    }

    #[test]
    fn real_alias_propagation_cannot_copy_a_source_integer_cache() {
        let real = rumoca_core::TypeId::new(1);
        let integer = rumoca_core::TypeId::new(2);
        let boolean = rumoca_core::TypeId::new(3);
        let string = rumoca_core::TypeId::new(4);
        let clock = rumoca_core::TypeId::new(5);
        let mut flat = flat::Model {
            predefined_types: flat::PredefinedTypeIds {
                real,
                integer,
                boolean,
                string,
                clock,
            },
            ..flat::Model::default()
        };
        for (index, (name, binding)) in [
            (
                "source",
                Expression::Literal {
                    value: rumoca_core::Literal::Real(2.5),
                    span: test_span(),
                },
            ),
            (
                "alias",
                Expression::VarRef {
                    name: core_reference(&[("source", DefId::new(20_499))])
                        .with_instance_id(InstanceId::new(21_499)),
                    subscripts: Vec::new(),
                    span: test_span(),
                },
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let variable_name = rumoca_core::VarName::new(name);
            let declaration = DefId::new(20_500 + index as u32);
            flat.add_variable(
                variable_name.clone(),
                flat::Variable {
                    name: variable_name,
                    type_id: real,
                    variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                    binding: Some(binding),
                    component_ref: Some(core_component_ref(&[(name, declaration)])),
                    instance_id: InstanceId::new(21_500 + index as u32),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }
        let mut ctx = Context::new();
        ctx.parameter_values.insert("source".to_string(), 2);
        ctx.real_parameter_values.insert("source".to_string(), 2.5);
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("alias"),
            rumoca_core::ComponentPath::from_flat_path("source"),
        );

        ctx.build_parameter_lookup(&flat, &ClassTree::default())
            .unwrap();

        assert_eq!(ctx.real_parameter_values.get("alias"), Some(&2.5));
        assert!(!ctx.parameter_values.contains_key("alias"));
        assert!(!ctx.boolean_parameter_values.contains_key("alias"));
        assert!(!ctx.enum_parameter_values.contains_key("alias"));
    }

    #[test]
    fn unknown_alias_metadata_copies_no_typed_cache() {
        let mut flat = flat::Model::default();
        let alias = rumoca_core::VarName::new("alias");
        flat.add_variable(
            alias.clone(),
            flat::Variable {
                name: alias,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::VarRef {
                    name: core_reference(&[("source", DefId::new(20_599))])
                        .with_instance_id(InstanceId::new(21_599)),
                    subscripts: Vec::new(),
                    span: test_span(),
                }),
                component_ref: Some(core_component_ref(&[("alias", DefId::new(20_600))])),
                instance_id: InstanceId::new(21_600),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut ctx = Context::new();
        ctx.parameter_values.insert("source".to_string(), 2);
        ctx.real_parameter_values.insert("source".to_string(), 2.5);
        ctx.boolean_parameter_values
            .insert("source".to_string(), true);
        ctx.enum_parameter_values.insert(
            "source".to_string(),
            resolved_enum_value(rumoca_core::DefId::new(159), "E", "a"),
        );
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("alias"),
            rumoca_core::ComponentPath::from_flat_path("source"),
        );

        ctx.build_parameter_lookup(&flat, &ClassTree::default())
            .unwrap();

        assert!(!ctx.parameter_values.contains_key("alias"));
        assert!(!ctx.real_parameter_values.contains_key("alias"));
        assert!(!ctx.boolean_parameter_values.contains_key("alias"));
        assert!(!ctx.enum_parameter_values.contains_key("alias"));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_prepends_parent_dims_to_field_arrays() {
        let mut flat = flat::Model::default();

        let mut field = flat::Variable {
            name: rumoca_core::VarName::new("world.cylinders.R.T"),
            is_primitive: true,
            dims: vec![3, 3],
            ..flat::Variable::empty_with_span(test_span())
        };
        flat.add_variable(field.name.clone(), field.clone());

        field.name = rumoca_core::VarName::new("world.cylinders.r_shape");
        field.dims = vec![3];
        field.binding = Some(Expression::Array {
            elements: vec![
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
                Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: test_span(),
                },
            ],
            is_matrix: false,
            span: test_span(),
        });
        flat.add_variable(field.name.clone(), field);

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        assert_eq!(
            flat.variables
                .get(&rumoca_core::VarName::new("world.cylinders.R.T"))
                .expect("missing field R.T")
                .dims,
            vec![2, 3, 3]
        );
        assert_eq!(
            flat.variables
                .get(&rumoca_core::VarName::new("world.cylinders.r_shape"))
                .expect("missing field r_shape")
                .dims,
            vec![2, 3]
        );
        let binding = flat
            .variables
            .get(&rumoca_core::VarName::new("world.cylinders.r_shape"))
            .and_then(|variable| variable.binding.as_ref())
            .expect("missing field r_shape binding");
        let Expression::ArrayComprehension { expr, indices, .. } = binding else {
            panic!("declaration binding should repeat over the parent component dimensions");
        };
        assert_eq!(indices.len(), 1);
        assert!(matches!(expr.as_ref(), Expression::Array { elements, .. } if elements.len() == 3));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_does_not_double_prefix_dims() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("world.cylinders.R.T");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                dims: vec![2, 3, 3],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        assert_eq!(
            flat.variables
                .get(&var_name)
                .expect("missing R.T field")
                .dims,
            vec![2, 3, 3]
        );
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_repeats_each_modifier_binding() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("world.cylinders.widthDirection");
        let component_ref = core_component_ref(&[
            ("world", DefId::new(100)),
            ("cylinders", DefId::new(101)),
            ("widthDirection", DefId::new(102)),
        ]);
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                component_ref: Some(component_ref.clone()),
                dims: vec![3],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }),
                binding_from_modification: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );
        overlay.each_modifier_bindings.insert(
            rumoca_core::ComponentPath::from_component_reference(&component_ref),
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        let variable = flat.variables.get(&var_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3]);
        assert!(matches!(
            variable.binding,
            Some(Expression::ArrayComprehension { ref indices, .. }) if indices.len() == 1
        ));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_repeats_record_alias_binding() {
        let mut flat = flat::Model::default();
        let source_name = rumoca_core::VarName::new("world.x_label.R.T");
        flat.add_variable(
            source_name.clone(),
            flat::Variable {
                name: source_name,
                dims: vec![3, 3],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let target_name = rumoca_core::VarName::new("world.x_label.cylinders.R.T");
        let component_ref = core_component_ref(&[
            ("world", DefId::new(110)),
            ("x_label", DefId::new(111)),
            ("cylinders", DefId::new(112)),
            ("R", DefId::new(113)),
            ("T", DefId::new(114)),
        ]);
        flat.add_variable(
            target_name.clone(),
            flat::Variable {
                name: target_name.clone(),
                component_ref: Some(component_ref.clone()),
                dims: vec![3, 3],
                binding: Some(Expression::FieldAccess {
                    base: Box::new(Expression::VarRef {
                        name: rumoca_core::Reference::with_component_reference(
                            "world.x_label.R",
                            core_component_ref(&[
                                ("world", DefId::new(110)),
                                ("x_label", DefId::new(111)),
                                ("R", DefId::new(113)),
                            ]),
                        ),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    field: "T".to_string(),
                    field_def_id: DefId::new(114),
                    span: test_span(),
                }),
                binding_from_modification: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.x_label.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );
        overlay.each_modifier_bindings.insert(
            rumoca_core::ComponentPath::from_component_reference(&component_ref),
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        let variable = flat.variables.get(&target_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3, 3]);
        assert!(matches!(
            variable.binding,
            Some(Expression::ArrayComprehension { ref indices, .. }) if indices.len() == 1
        ));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_does_not_repeat_non_each_modifier() {
        let mut flat = flat::Model::default();
        let target_name = rumoca_core::VarName::new("world.cylinders.R.T");
        let binding = Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "world.R.T",
                core_component_ref(&[
                    ("world", DefId::new(120)),
                    ("R", DefId::new(122)),
                    ("T", DefId::new(123)),
                ]),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        };
        flat.add_variable(
            target_name.clone(),
            flat::Variable {
                name: target_name.clone(),
                component_ref: Some(core_component_ref(&[
                    ("world", DefId::new(120)),
                    ("cylinders", DefId::new(121)),
                    ("R", DefId::new(122)),
                    ("T", DefId::new(123)),
                ])),
                dims: vec![3, 3],
                binding: Some(binding.clone()),
                binding_from_modification: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        let variable = flat.variables.get(&target_name).expect("missing field");
        assert_eq!(variable.dims, vec![2, 3, 3]);
        assert_eq!(variable.binding.as_ref(), Some(&binding));
    }

    #[test]
    fn test_propagate_unexpanded_record_array_dims_combines_parent_and_child_from_start() {
        let mut flat = flat::Model::default();
        let var_name = rumoca_core::VarName::new("world.cylinders.lengthDirection");
        flat.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                is_primitive: true,
                dims: vec![2],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::VarRef {
                            name: rumoca_core::Reference::with_component_reference(
                                "a",
                                core_component_ref(&[("a", DefId::new(130))]),
                            ),
                            subscripts: vec![],
                            span: test_span(),
                        },
                        Expression::VarRef {
                            name: rumoca_core::Reference::with_component_reference(
                                "b",
                                core_component_ref(&[("b", DefId::new(131))]),
                            ),
                            subscripts: vec![],
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }),
                start: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }),
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            InstanceData {
                instance_id: InstanceId::new(1),
                qualified_name: QualifiedName::from_dotted("world.cylinders"),
                dims: vec![2],
                is_primitive: false,
                ..Default::default()
            },
        );

        propagate_unexpanded_record_array_dims(&mut flat, &overlay).unwrap();

        assert_eq!(
            flat.variables
                .get(&var_name)
                .expect("missing world.cylinders.lengthDirection")
                .dims,
            vec![2, 3]
        );
    }

    #[test]
    fn explicit_component_dimensions_are_not_reissued_in_flatten() {
        let real_fft_def = DefId::new(300);
        let nf_def = DefId::new(301);
        let abs_def = DefId::new(302);
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = typed_flat_model();
        let nf_name = rumoca_core::VarName::new("realFFT.nf");
        flat.add_variable(
            nf_name.clone(),
            flat::Variable {
                name: nf_name,
                type_id: test_integer_type(),
                component_ref: Some(core_component_ref(&[
                    ("realFFT", real_fft_def),
                    ("nf", nf_def),
                ])),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Integer(401),
                    span: test_span(),
                }),
                is_discrete_type: true,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let abs_name = rumoca_core::VarName::new("realFFT.abs");
        flat.add_variable(
            abs_name.clone(),
            flat::Variable {
                name: abs_name.clone(),
                type_id: test_real_type(),
                component_ref: Some(core_component_ref(&[
                    ("realFFT", real_fft_def),
                    ("abs", abs_def),
                ])),
                dims: vec![4],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "realFFT.abs",
                vec![ast::Subscript::Expression(resolved_component_ref_expr(&[
                    ("realFFT", real_fft_def),
                    ("nf", nf_def),
                ]))],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        assert_eq!(ctx.array_dimensions.get("realFFT.abs"), Some(&vec![4]));

        let changed = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("explicit dimensions are already typechecked");

        assert!(!changed);
        assert_eq!(
            flat.variables.get(&abs_name).expect("abs variable").dims,
            vec![4]
        );
        assert_eq!(ctx.array_dimensions.get("realFFT.abs"), Some(&vec![4]));
    }

    #[test]
    fn deferred_colon_record_field_dimensions_are_idempotent() {
        let model_def = DefId::new(310);
        let records_def = DefId::new(312);
        let values_def = DefId::new(313);
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = typed_flat_model();
        let field_name = rumoca_core::VarName::new("model.records.values");
        flat.add_variable(
            field_name.clone(),
            flat::Variable {
                name: field_name.clone(),
                type_id: test_real_type(),
                component_ref: Some(core_component_ref(&[
                    ("model", model_def),
                    ("records", records_def),
                    ("values", values_def),
                ])),
                dims: vec![1],
                binding: Some(int_array(&[1, 2])),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        let mut record_parent = symbolic_instance(InstanceId::new(1), "model.records", Vec::new());
        record_parent.is_primitive = false;
        record_parent.dims = vec![7];
        overlay
            .components
            .insert(record_parent.instance_id, record_parent);
        overlay.components.insert(
            InstanceId::new(2),
            symbolic_instance(
                InstanceId::new(2),
                "model.records.values",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        assert!(
            ctx.discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
                .expect("record field colon dimension should resolve")
        );
        assert_eq!(
            flat.variables
                .get(&field_name)
                .expect("record field variable")
                .dims,
            vec![7, 2]
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        let changed_again = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("an already discharged colon dimension remains valid");

        assert!(!changed_again, "colon discharge must consume exactly once");
        assert_eq!(
            flat.variables
                .get(&field_name)
                .expect("record field variable")
                .dims,
            vec![7, 2]
        );
        assert_eq!(
            ctx.array_dimensions.get("model.records.values"),
            Some(&vec![7, 2])
        );
    }

    #[test]
    fn equal_parent_and_local_extents_are_appended_not_deduplicated() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = typed_flat_model();
        let field_name = rumoca_core::VarName::new("model.records.values");
        flat.add_variable(
            field_name.clone(),
            flat::Variable {
                name: field_name.clone(),
                type_id: test_real_type(),
                component_ref: Some(core_component_ref(&[
                    ("model", DefId::new(314)),
                    ("records", DefId::new(315)),
                    ("values", DefId::new(316)),
                ])),
                dims: vec![3],
                binding: Some(int_array(&[1, 2, 3])),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        let mut record_parent = symbolic_instance(InstanceId::new(1), "model.records", Vec::new());
        record_parent.is_primitive = false;
        record_parent.dims = vec![3];
        overlay
            .components
            .insert(record_parent.instance_id, record_parent);
        overlay.components.insert(
            InstanceId::new(2),
            symbolic_instance(
                InstanceId::new(2),
                "model.records.values",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        assert!(
            ctx.discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
                .expect("equal-valued parent and local axes remain distinct")
        );
        assert_eq!(
            flat.variables
                .get(&field_name)
                .expect("record field variable")
                .dims,
            vec![3, 3]
        );
    }

    #[test]
    fn colon_component_dimensions_use_enum_range_binding_shape() {
        let mut ctx = Context::new();
        let mut tree = ClassTree::new();
        add_test_source(&mut tree);
        let logic_def_id = DefId::new(42);
        let mut logic = ClassDef {
            name: token("Logic"),
            class_type: ClassType::Type,
            def_id: Some(logic_def_id),
            ..Default::default()
        };
        for literal in ["U", "X", "0", "1", "Z", "W", "L", "H", "-"] {
            logic.enum_literals.push(ast::EnumLiteral {
                ident: token(literal),
                description: Vec::new(),
            });
        }
        tree.definitions.classes.insert("Logic".to_string(), logic);
        tree.def_map.insert(logic_def_id, "Logic".to_string());
        tree.name_map.insert("Logic".to_string(), logic_def_id);

        let enum_ref = |literal: &str| {
            let literal_offset = ["U", "X", "0", "1", "Z", "W", "L", "H", "-"]
                .iter()
                .position(|candidate| *candidate == literal)
                .expect("fixture literal belongs to Logic");
            let literal_def_id =
                DefId::new(43 + u32::try_from(literal_offset).expect("literal index fits u32"));
            let component_ref =
                core_component_ref(&[("Logic", logic_def_id), (literal, literal_def_id)]);
            Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(component_ref),
                subscripts: Vec::new(),
                span: test_span(),
            }
        };

        let mut flat = flat::Model::default();
        let values_name = rumoca_core::VarName::new("delay.LogicValues");
        flat.add_variable(
            values_name.clone(),
            flat::Variable {
                name: values_name.clone(),
                component_ref: Some(core_component_ref(&[(
                    "delay.LogicValues",
                    DefId::new(20_701),
                )])),
                instance_id: InstanceId::new(21_701),
                dims: vec![1],
                binding: Some(Expression::Range {
                    start: Box::new(enum_ref("U")),
                    step: None,
                    end: Box::new(enum_ref("-")),
                    span: test_span(),
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "delay.LogicValues",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        let changed = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("enum range binding should resolve colon dimension");

        assert!(changed);
        assert_eq!(
            flat.variables
                .get(&values_name)
                .expect("LogicValues variable")
                .dims,
            vec![9]
        );
        assert_eq!(
            ctx.array_dimensions.get("delay.LogicValues"),
            Some(&vec![9])
        );
    }

    #[test]
    fn colon_component_dimensions_use_binding_shape() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name.clone(),
            flat::Variable {
                name: x_name.clone(),
                component_ref: Some(core_component_ref(&[("a.x", DefId::new(20_702))])),
                instance_id: InstanceId::new(21_702),
                dims: vec![1],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(1),
                            span: test_span(),
                        },
                        Expression::Literal {
                            value: rumoca_core::Literal::Integer(0),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        let changed = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("colon dimension should resolve from binding shape");

        assert!(changed);
        assert_eq!(
            flat.variables.get(&x_name).expect("a.x variable").dims,
            vec![4]
        );
        assert_eq!(ctx.array_dimensions.get("a.x"), Some(&vec![4]));
    }

    #[test]
    fn colon_component_dimension_rejects_conflicting_concrete_shapes() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name.clone(),
            flat::Variable {
                name: x_name,
                component_ref: Some(core_component_ref(&[("a.x", DefId::new(20_704))])),
                instance_id: InstanceId::new(21_704),
                dims: vec![5],
                binding: Some(int_array(&[1, 2, 3, 4])),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        let declaration_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(TEST_FILE),
            0,
            1,
        );
        let error = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect_err("conflicting concrete owners must not be ranked heuristically");
        assert!(
            matches!(
                &error,
                FlattenError::ConflictingComponentDimension {
                    name,
                    axis: 1,
                    admitted: 5,
                    inferred: 4,
                    span,
                } if name == "a.x"
                    && *span == declaration_span
            ),
            "unexpected refusal: {error:?}"
        );
    }

    #[test]
    fn colon_component_dimensions_accept_zero_sized_binding_shape() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let table_name = rumoca_core::VarName::new("table");
        flat.add_variable(
            table_name.clone(),
            flat::Variable {
                name: table_name.clone(),
                component_ref: Some(core_component_ref(&[("table", DefId::new(20_703))])),
                instance_id: InstanceId::new(21_703),
                dims: vec![0, 2],
                binding: Some(fill_expr(0, &[0, 2])),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "table",
                vec![
                    ast::Subscript::Range {
                        token: rumoca_core::Token::default(),
                    },
                    ast::Subscript::Range {
                        token: rumoca_core::Token::default(),
                    },
                ],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        let changed = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("zero-sized binding shape should satisfy colon dimensions");

        assert!(!changed);
        assert_eq!(
            flat.variables
                .get(&table_name)
                .expect("table variable")
                .dims,
            vec![0, 2]
        );
        assert_eq!(ctx.array_dimensions.get("table"), Some(&vec![0, 2]));
    }

    #[test]
    fn colon_component_dimensions_without_shape_fail_fast() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let x_name = rumoca_core::VarName::new("a.x");
        flat.add_variable(
            x_name,
            flat::Variable {
                name: rumoca_core::VarName::new("a.x"),
                component_ref: Some(core_component_ref(&[("a.x", DefId::new(20_705))])),
                instance_id: InstanceId::new(21_705),
                dims: Vec::new(),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "a.x",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        let err = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect_err("colon dimension without a known shape should fail");

        assert!(matches!(
            err,
            FlattenError::UnresolvedComponentDimension { .. }
        ));
    }

    #[test]
    fn deferred_colon_accepts_an_already_issued_axis() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = flat::Model::default();
        let table_name = rumoca_core::VarName::new("model.table");
        flat.add_variable(
            table_name.clone(),
            flat::Variable {
                name: rumoca_core::VarName::new("model.table"),
                component_ref: Some(core_component_ref(&[("model.table", DefId::new(20_706))])),
                instance_id: InstanceId::new(21_706),
                dims: vec![8],
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "model.table",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();
        let changed = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect("known flat dimensions should satisfy colon resolution");

        assert!(!changed);
        assert_eq!(
            flat.variables
                .get(&table_name)
                .expect("table variable should remain present")
                .dims,
            vec![8]
        );
    }

    #[test]
    fn required_component_binding_shape_empty_fails_ef034_at_the_expression_span() {
        let mut ctx = Context::new();
        let tree = source_backed_tree();
        let mut flat = typed_flat_model();
        let name = rumoca_core::VarName::new("model.table");
        flat.add_variable(
            name.clone(),
            flat::Variable {
                name,
                type_id: test_integer_type(),
                dims: vec![1],
                binding: Some(Expression::Empty { span: test_span() }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let mut overlay = InstanceOverlay::default();
        overlay.components.insert(
            InstanceId::new(1),
            symbolic_instance(
                InstanceId::new(1),
                "model.table",
                vec![ast::Subscript::Range {
                    token: rumoca_core::Token::default(),
                }],
            ),
        );

        let error = ctx
            .discharge_deferred_colon_dimensions(&mut flat, &overlay, &tree)
            .expect_err("a recovery expression cannot define a required component shape");
        assert!(
            matches!(
                &error,
                FlattenError::ConstantEvaluationFailed { span, .. } if *span == test_span()
            ),
            "unexpected refusal: {error:?}"
        );
    }

    #[test]
    fn lookup_infers_array_literal_dims_inside_array_component_element() {
        let mut ctx = Context::new();
        let tree = ClassTree::default();
        let mut flat = typed_flat_model();
        let a_name = rumoca_core::VarName::new("adaptor.filter[1].transferFunction[1].a");
        flat.add_variable(
            a_name.clone(),
            flat::Variable {
                name: a_name,
                type_id: test_integer_type(),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_array(&[-1, 1])),
                binding_from_modification: true,
                component_ref: Some(core_component_ref(&[(
                    "adaptor.filter[1].transferFunction[1].a",
                    DefId::new(1),
                )])),
                instance_id: InstanceId::new(1),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        let nx_name = rumoca_core::VarName::new("adaptor.filter[1].transferFunction[1].nx");
        flat.add_variable(
            nx_name.clone(),
            flat::Variable {
                name: nx_name,
                type_id: test_integer_type(),
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Binary {
                    op: rumoca_core::OpBinary::Sub,
                    lhs: Box::new(size_dim_expr(
                        &[("a", DefId::new(1))],
                        InstanceId::new(1),
                        1,
                    )),
                    rhs: Box::new(int_lit(1)),
                    span: test_span(),
                }),
                component_ref: Some(core_component_ref(&[(
                    "adaptor.filter[1].transferFunction[1].nx",
                    DefId::new(2),
                )])),
                instance_id: InstanceId::new(2),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );

        ctx.build_parameter_lookup(&flat, &tree).unwrap();

        assert_eq!(
            ctx.array_dimensions
                .get("adaptor.filter[1].transferFunction[1].a"),
            Some(&vec![2])
        );
        assert_eq!(
            ctx.parameter_values
                .get("adaptor.filter[1].transferFunction[1].nx"),
            Some(&1_i64)
        );
    }

    #[test]
    fn test_eval_enum_params_resolves_alias_target_reference() {
        let mut flat = typed_flat_model();
        add_enum_parameter(
            &mut flat,
            "system.energyDynamics",
            &[
                ("system", DefId::new(700)),
                ("energyDynamics", DefId::new(701)),
            ],
            InstanceId::new(800),
            enum_literal_expr("SteadyStateInitial", DefId::new(603)),
        );
        add_enum_parameter(
            &mut flat,
            "pipe1.system.energyDynamics",
            &[
                ("pipe1", DefId::new(702)),
                ("system", DefId::new(703)),
                ("energyDynamics", DefId::new(704)),
            ],
            InstanceId::new(801),
            parameter_reference_expr(
                &[("unresolved", DefId::new(705)), ("value", DefId::new(706))],
                InstanceId::new(802),
            ),
        );
        let mut ctx = Context::new();
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("pipe1.system"),
            rumoca_core::ComponentPath::from_flat_path("system"),
        );
        ctx.build_parameter_lookup(&flat, &test_enum_tree())
            .expect("record alias should propagate a resolved enum value");

        assert_eq!(
            enum_display(&ctx, "pipe1.system.energyDynamics"),
            Some("Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_requires_alias_for_outer_like_reference() {
        let mut flat = typed_flat_model();
        add_enum_parameter(
            &mut flat,
            "system.energyDynamics",
            &[
                ("system", DefId::new(710)),
                ("energyDynamics", DefId::new(711)),
            ],
            InstanceId::new(810),
            enum_literal_expr("SteadyState", DefId::new(602)),
        );
        add_enum_parameter(
            &mut flat,
            "pipe1.system.energyDynamics",
            &[
                ("pipe1", DefId::new(712)),
                ("system", DefId::new(713)),
                ("energyDynamics", DefId::new(714)),
            ],
            InstanceId::new(811),
            parameter_reference_expr(
                &[
                    ("pipe1", DefId::new(712)),
                    ("system", DefId::new(713)),
                    ("energyDynamics", DefId::new(714)),
                ],
                InstanceId::new(812),
            ),
        );
        let ctx = build_test_parameter_context(&flat);

        // Non-vacuity guard: the enclosing declaration really is resolvable in
        // this same context, so the rejection below is about name lookup and
        // not about evaluation having failed outright.
        assert_eq!(
            enum_display(&ctx, "system.energyDynamics"),
            Some("Dynamics.SteadyState".to_string())
        );
        // MLS 3.7 §5.3.2: every identifier after the first must name an element
        // of the instance found so far, so `pipe1.system.energyDynamics` cannot
        // silently degrade to the top-level `system.energyDynamics`.
        assert_eq!(enum_display(&ctx, "pipe1.system.energyDynamics"), None);
    }

    #[test]
    fn test_eval_enum_params_resolves_uppercase_scoped_reference_through_alias() {
        let mut flat = typed_flat_model();
        add_enum_parameter(
            &mut flat,
            "system.energyDynamics",
            &[
                ("system", DefId::new(720)),
                ("energyDynamics", DefId::new(721)),
            ],
            InstanceId::new(820),
            enum_literal_expr("SteadyStateInitial", DefId::new(603)),
        );
        add_enum_parameter(
            &mut flat,
            "HEX.system.energyDynamics",
            &[
                ("HEX", DefId::new(722)),
                ("system", DefId::new(723)),
                ("energyDynamics", DefId::new(724)),
            ],
            InstanceId::new(821),
            parameter_reference_expr(
                &[("unresolved", DefId::new(725)), ("value", DefId::new(726))],
                InstanceId::new(822),
            ),
        );
        let mut ctx = Context::new();
        ctx.record_aliases.insert(
            rumoca_core::ComponentPath::from_flat_path("HEX.system"),
            rumoca_core::ComponentPath::from_flat_path("system"),
        );
        ctx.build_parameter_lookup(&flat, &test_enum_tree())
            .expect("uppercase record alias should propagate a resolved enum value");

        assert_eq!(
            enum_display(&ctx, "HEX.system.energyDynamics"),
            Some("Dynamics.SteadyStateInitial".to_string())
        );
    }

    #[test]
    fn test_eval_enum_params_normalizes_transitive_reference_values() {
        let mut flat = typed_flat_model();
        let system_instance = InstanceId::new(830);
        add_enum_parameter(
            &mut flat,
            "system.energyDynamics",
            &[
                ("system", DefId::new(730)),
                ("energyDynamics", DefId::new(731)),
            ],
            system_instance,
            enum_literal_expr("SteadyStateInitial", DefId::new(603)),
        );
        let pipe_instance = InstanceId::new(831);
        add_enum_parameter(
            &mut flat,
            "pipe.energyDynamics",
            &[
                ("pipe", DefId::new(732)),
                ("energyDynamics", DefId::new(733)),
            ],
            pipe_instance,
            parameter_reference_expr(
                &[
                    ("system", DefId::new(730)),
                    ("energyDynamics", DefId::new(731)),
                ],
                system_instance,
            ),
        );
        add_enum_parameter(
            &mut flat,
            "HEX.energyDynamics",
            &[
                ("HEX", DefId::new(734)),
                ("energyDynamics", DefId::new(735)),
            ],
            InstanceId::new(832),
            parameter_reference_expr(
                &[
                    ("pipe", DefId::new(732)),
                    ("energyDynamics", DefId::new(733)),
                ],
                pipe_instance,
            ),
        );
        let ctx = build_test_parameter_context(&flat);

        assert_eq!(
            enum_display(&ctx, "HEX.energyDynamics"),
            Some("Dynamics.SteadyStateInitial".to_string())
        );
    }
}
