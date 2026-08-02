//! Unit tests for instantiation-time function and array-dimension evaluation.

use std::sync::Arc;

use super::super::{
    InstantiateEvalCtx, enum_values_equal, eval_integer_binary, evaluate_component_condition,
    try_eval_integer_expr,
};
use super::evaluate_array_dimensions;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

fn no_op_resolve_class_components(
    _tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> ast::AstIndexMap<String, ast::Component> {
    class.components.clone()
}

#[test]
fn enum_values_equal_accepts_different_qualification_prefixes() {
    let a = "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve";
    let b = "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve";
    assert!(enum_values_equal(a, b));
}

#[test]
fn enum_values_equal_rejects_different_enum_types() {
    let a = "Modelica.Blocks.Types.SimpleController.PI";
    let b = "Modelica.Blocks.Types.Init.PI";
    assert!(!enum_values_equal(a, b));
}

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..rumoca_core::Token::default()
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("instantiate_function_eval_test.mo"),
        1,
        2,
    )
}

fn cref(path: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: rumoca_core::ComponentPath::from_flat_path(path)
            .into_parts()
            .into_iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(&part),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

/// A tree containing `package <package_name> type <enum_name> = enumeration(...)`.
fn tree_with_enumeration(package_name: &str, enum_name: &str, literals: &[&str]) -> ast::ClassTree {
    let enum_class = ast::ClassDef {
        name: token(enum_name),
        class_type: rumoca_core::ClassType::Type,
        enum_literals: literals
            .iter()
            .map(|literal| ast::EnumLiteral {
                ident: token(literal),
                description: Vec::new(),
            })
            .collect(),
        ..Default::default()
    };
    let mut package = ast::ClassDef {
        name: token(package_name),
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    package.classes.insert(enum_name.to_string(), enum_class);

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert(package_name.to_string(), package);
    tree
}

fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn mul_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn lt_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn func_call(path: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: cref(path),
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn if_expr(
    branches: Vec<(ast::Expression, ast::Expression)>,
    else_branch: ast::Expression,
) -> ast::Expression {
    ast::Expression::If {
        branches,
        else_branch: Arc::new(else_branch),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: i64) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: token(&value.to_string()),

        span: rumoca_core::Span::DUMMY,
    }
}

fn real_expr(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_expr(value: bool) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),

        span: rumoca_core::Span::DUMMY,
    }
}

fn input_int_component(name: &str) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Input(token("input")),
        variability: rumoca_core::Variability::Parameter(token("parameter")),
        ..ast::Component::empty_with_span(test_span())
    }
}

fn output_bool_component(name: &str) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Output(token("output")),
        start: bool_expr(false),
        ..ast::Component::empty_with_span(test_span())
    }
}

fn output_int_component(name: &str) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Output(token("output")),
        start: int_expr(0),
        ..ast::Component::empty_with_span(test_span())
    }
}

/// `protected Integer <name> = <initial_value>;`
///
/// The initial value is a declaration assignment (MLS §12.4.1), not a
/// `start` attribute: a function variable read before assignment has no
/// value the evaluator may invent.
fn local_int_component(name: &str, initial_value: i64) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        binding: Some(int_expr(initial_value)),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    }
}

/// `output Integer <name> = <initial_value>;`
fn output_int_component_initialized(name: &str, initial_value: i64) -> ast::Component {
    ast::Component {
        name: name.to_string(),
        causality: rumoca_core::Causality::Output(token("output")),
        binding: Some(int_expr(initial_value)),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    }
}

fn assignment(path: &str, value: ast::Expression) -> ast::Statement {
    ast::Statement::Assignment {
        comp: cref(path),
        value,
    }
}

fn tree_with_function(name: &str, function: ast::ClassDef) -> ast::ClassTree {
    let function_id = function.def_id.expect("test function DefId");
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert(name.to_string(), function);
    tree.def_map.insert(function_id, name.to_string());
    tree.name_map.insert(name.to_string(), function_id);
    tree
}

fn msl_math_tree() -> ast::ClassTree {
    let modelica_id = rumoca_core::DefId::new(1);
    let math_id = rumoca_core::DefId::new(2);
    let function_id = rumoca_core::DefId::new(3);

    let function = msl_is_power_of_two_function(function_id);

    let mut math = ast::ClassDef {
        def_id: Some(math_id),
        name: token("Math"),
        class_type: rumoca_core::ClassType::Package,
        ..ast::ClassDef::default()
    };
    math.classes.insert("isPowerOf2".to_string(), function);

    let mut modelica = ast::ClassDef {
        def_id: Some(modelica_id),
        name: token("Modelica"),
        class_type: rumoca_core::ClassType::Package,
        ..ast::ClassDef::default()
    };
    modelica.classes.insert("Math".to_string(), math);

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    tree.def_map.insert(modelica_id, "Modelica".to_string());
    tree.def_map.insert(math_id, "Modelica.Math".to_string());
    tree.def_map
        .insert(function_id, "Modelica.Math.isPowerOf2".to_string());
    tree.name_map.insert("Modelica".to_string(), modelica_id);
    tree.name_map.insert("Modelica.Math".to_string(), math_id);
    tree.name_map
        .insert("Modelica.Math.isPowerOf2".to_string(), function_id);
    tree
}

fn msl_is_power_of_two_function(function_id: rumoca_core::DefId) -> ast::ClassDef {
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("isPowerOf2"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("i".to_string(), input_int_component("i"));
    function
        .components
        .insert("result".to_string(), output_bool_component("result"));
    function
        .components
        .insert("target".to_string(), local_int_component("target", 0));
    function
        .components
        .insert("powOf2".to_string(), local_int_component("powOf2", 1));
    function.algorithms.push(msl_is_power_of_two_algorithm());
    function
}

fn msl_is_power_of_two_algorithm() -> Vec<ast::Statement> {
    vec![
        ast::Statement::FunctionCall {
            comp: cref("assert"),
            args: vec![
                rumoca_ir_ast::Expression::Binary {
                    op: rumoca_core::OpBinary::Ge,
                    lhs: Arc::new(ast::Expression::ComponentReference(cref("i"))),
                    rhs: Arc::new(int_expr(1)),
                    span: rumoca_core::Span::DUMMY,
                },
                bool_expr(true),
            ],
            outputs: Vec::new(),
        },
        ast::Statement::If {
            cond_blocks: vec![ast::StatementBlock {
                cond: eq_expr(
                    func_call(
                        "mod",
                        vec![ast::Expression::ComponentReference(cref("i")), int_expr(2)],
                    ),
                    int_expr(1),
                ),
                stmts: vec![assignment(
                    "result",
                    eq_expr(ast::Expression::ComponentReference(cref("i")), int_expr(1)),
                )],
            }],
            else_block: Some(msl_is_power_of_two_even_branch()),
        },
    ]
}

fn msl_is_power_of_two_even_branch() -> Vec<ast::Statement> {
    vec![
        assignment(
            "target",
            func_call(
                "div",
                vec![ast::Expression::ComponentReference(cref("i")), int_expr(2)],
            ),
        ),
        assignment("powOf2", int_expr(1)),
        ast::Statement::While(ast::StatementBlock {
            cond: lt_expr(
                ast::Expression::ComponentReference(cref("powOf2")),
                ast::Expression::ComponentReference(cref("target")),
            ),
            stmts: vec![assignment(
                "powOf2",
                mul_expr(
                    ast::Expression::ComponentReference(cref("powOf2")),
                    int_expr(2),
                ),
            )],
        }),
        assignment(
            "result",
            eq_expr(
                ast::Expression::ComponentReference(cref("target")),
                ast::Expression::ComponentReference(cref("powOf2")),
            ),
        ),
    ]
}

#[test]
fn integer_div_operator_requires_exact_quotient() {
    assert_eq!(
        eval_integer_binary(&rumoca_core::OpBinary::Div, 8, 2),
        Some(4)
    );
    assert_eq!(eval_integer_binary(&rumoca_core::OpBinary::Div, 7, 2), None);
}

#[test]
fn integer_div_builtin_remains_truncating() {
    let expr = ast::Expression::FunctionCall {
        comp: cref("div"),
        args: vec![int_expr(7), int_expr(2)],
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    };
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(3));
}

#[test]
fn integer_builtin_floors_negative_real_during_instantiation() {
    let negative_fraction = ast::Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Arc::new(real_expr("0.5")),
        span: rumoca_core::Span::DUMMY,
    };
    let expr = func_call("integer", vec![negative_fraction]);
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(-1));
}

#[test]
fn unqualified_unique_function_call_evaluates_without_def_id() {
    let tree = msl_math_tree();
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let expr = if_expr(
        vec![(
            ast::Expression::FunctionCall {
                comp: cref("isPowerOf2"),
                args: vec![int_expr(8)],
                is_partial_application: false,
                span: rumoca_core::Span::DUMMY,
            },
            int_expr(1),
        )],
        int_expr(0),
    );

    assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(1));
}

#[test]
fn scalar_integer_eval_rejects_array_output_functions() {
    let function_id = rumoca_core::DefId::new(1);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("arrayInteger"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    let mut output = output_int_component("y");
    output.shape_expr = vec![ast::Subscript::Expression(int_expr(2))];
    function.components.insert("y".to_string(), output);
    function
        .algorithms
        .push(vec![assignment("y[1]", int_expr(1))]);

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("arrayInteger".to_string(), function);
    tree.def_map.insert(function_id, "arrayInteger".to_string());
    tree.name_map
        .insert("arrayInteger".to_string(), function_id);

    let expr = func_call("arrayInteger", Vec::new());
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(try_eval_integer_expr(&ctx, &expr), None);
}

#[test]
fn integer_function_while_break_updates_structural_result() {
    let function_id = rumoca_core::DefId::new(1);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("countUntilThree"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("n".to_string(), input_int_component("n"));
    function
        .components
        .insert("y".to_string(), output_int_component_initialized("y", 0));
    function
        .components
        .insert("i".to_string(), local_int_component("i", 0));
    function.algorithms.push(vec![
        ast::Statement::While(ast::StatementBlock {
            cond: lt_expr(
                ast::Expression::ComponentReference(cref("i")),
                ast::Expression::ComponentReference(cref("n")),
            ),
            stmts: vec![
                assignment(
                    "i",
                    add_expr(ast::Expression::ComponentReference(cref("i")), int_expr(1)),
                ),
                ast::Statement::If {
                    cond_blocks: vec![ast::StatementBlock {
                        cond: eq_expr(ast::Expression::ComponentReference(cref("i")), int_expr(3)),
                        stmts: vec![ast::Statement::Break {
                            token: token("break"),
                        }],
                    }],
                    else_block: None,
                },
                assignment(
                    "y",
                    add_expr(ast::Expression::ComponentReference(cref("y")), int_expr(1)),
                ),
            ],
        }),
        assignment(
            "y",
            add_expr(ast::Expression::ComponentReference(cref("y")), int_expr(10)),
        ),
    ]);

    let tree = tree_with_function("countUntilThree", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("countUntilThree", vec![int_expr(5)])),
        Some(12)
    );
}

#[test]
fn integer_function_return_inside_while_exits_function() {
    let function_id = rumoca_core::DefId::new(1);
    let mut function = ast::ClassDef {
        def_id: Some(function_id),
        name: token("returnFromLoop"),
        class_type: rumoca_core::ClassType::Function,
        pure: true,
        ..ast::ClassDef::default()
    };
    function
        .components
        .insert("y".to_string(), output_int_component("y"));
    function.algorithms.push(vec![
        ast::Statement::While(ast::StatementBlock {
            cond: bool_expr(true),
            stmts: vec![
                assignment("y", int_expr(7)),
                ast::Statement::Return {
                    token: token("return"),
                },
            ],
        }),
        assignment("y", int_expr(99)),
    ]);

    let tree = tree_with_function("returnFromLoop", function);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(
        try_eval_integer_expr(&ctx, &func_call("returnFromLoop", Vec::new())),
        Some(7)
    );
}

#[test]
fn evaluate_component_condition_with_resolved_enum_ref() {
    let mut components = IndexMap::default();
    // MLS §4.4.4: the declaration binding carries the value. (`start` is an
    // initial guess, MLS §4.9, and is deliberately not read as one.)
    let model_structure = ast::Component {
        name: "modelStructure".to_string(),
        binding: Some(ast::Expression::ComponentReference(cref(
            "Types.ModelStructure.a_vb",
        ))),
        has_explicit_binding: true,
        ..ast::Component::empty_with_span(test_span())
    };
    components.insert("modelStructure".to_string(), model_structure);

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    // MLS §4.8.5.1: `Types.ModelStructure.a_vb` is an enumeration literal
    // only because `ModelStructure` is an enumeration declaring `a_vb`. The
    // evaluator establishes that from the class tree, not from the spelling.
    let tree = tree_with_enumeration("Types", "ModelStructure", &["a_v", "a_vb", "av_b"]);
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = evaluate_component_condition(&ctx, &condition);

    assert_eq!(value, Some(true));
}

/// SPEC_0032 §3: a name that merely looks like an enumeration literal is
/// not one. Without a declaring enumeration the comparison is unknown.
#[test]
fn evaluate_component_condition_with_undeclared_enum_literal_is_unknown() {
    let mut components = IndexMap::default();
    components.insert(
        "modelStructure".to_string(),
        ast::Component {
            name: "modelStructure".to_string(),
            binding: Some(ast::Expression::ComponentReference(cref(
                "Types.ModelStructure.a_vb",
            ))),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), None);
}

#[test]
fn evaluate_component_condition_uses_declaration_binding() {
    let mut components = IndexMap::default();
    components.insert(
        "use_numberPort".to_string(),
        ast::Component {
            name: "use_numberPort".to_string(),
            type_name: ast::Name::from_string("Boolean"),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(bool_expr(true)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let condition = ast::Expression::ComponentReference(cref("use_numberPort"));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), Some(true));
}

#[test]
fn evaluate_component_condition_unknown_modifier_blocks_declaration_default() {
    let mut components = IndexMap::default();
    components.insert(
        "condition".to_string(),
        ast::Component {
            name: "condition".to_string(),
            type_name: ast::Name::from_string("Boolean"),
            binding: Some(bool_expr(true)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let mut mod_env = ast::ModificationEnvironment::new();
    mod_env.add(
        ast::QualifiedName::from_ident("condition"),
        ast::ModificationValue::simple(ast::Expression::ComponentReference(cref("start"))),
    );

    let condition = ast::Expression::ComponentReference(cref("condition"));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &mod_env,
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(evaluate_component_condition(&ctx, &condition), None);
}

#[test]
fn evaluate_component_condition_with_unresolved_enum_ref_is_unknown() {
    let mut components = IndexMap::default();
    let model_structure = ast::Component {
        name: "modelStructure".to_string(),
        ..ast::Component::empty_with_span(test_span())
    };
    components.insert("modelStructure".to_string(), model_structure);

    let condition = eq_expr(
        ast::Expression::ComponentReference(cref("modelStructure")),
        ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = evaluate_component_condition(&ctx, &condition);

    assert_eq!(value, None);
}

#[test]
fn evaluate_array_dimensions_supports_structural_if_shape_refs() {
    // MLS §4.4.4: structural parameters carry their value in the
    // declaration binding. `start` is an initial guess (MLS §4.9) that the
    // parser seeds with the type default, so it is never read as a value.
    let mut components = IndexMap::default();
    components.insert(
        "useLumpedPressure".to_string(),
        ast::Component {
            name: "useLumpedPressure".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(bool_expr(false)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFMLumped".to_string(),
        ast::Component {
            name: "nFMLumped".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFMDistributed".to_string(),
        ast::Component {
            name: "nFMDistributed".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nFM".to_string(),
        ast::Component {
            name: "nFM".to_string(),
            binding: Some(if_expr(
                vec![(
                    ast::Expression::ComponentReference(cref("useLumpedPressure")),
                    ast::Expression::ComponentReference(cref("nFMLumped")),
                )],
                ast::Expression::ComponentReference(cref("nFMDistributed")),
            )),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[1],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("nFM")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, Some(vec![2]));
}

#[test]
fn evaluate_array_dimensions_rejects_runtime_if_shape_condition() {
    let mut components = IndexMap::default();
    components.insert(
        "runtimeSwitch".to_string(),
        ast::Component {
            name: "runtimeSwitch".to_string(),
            start: bool_expr(false),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nA".to_string(),
        ast::Component {
            name: "nA".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(2),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "nB".to_string(),
        ast::Component {
            name: "nB".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(1),
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "n".to_string(),
        ast::Component {
            name: "n".to_string(),
            start: if_expr(
                vec![(
                    ast::Expression::ComponentReference(cref("runtimeSwitch")),
                    ast::Expression::ComponentReference(cref("nA")),
                )],
                ast::Expression::ComponentReference(cref("nB")),
            ),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[1],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("n")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    // Non-compile-time condition should keep dimension-expression evaluation
    // disabled and preserve the precomputed shape.
    assert_eq!(dims, Some(vec![1]));
}

#[test]
fn try_eval_integer_expr_prefers_binding_over_start_for_component_refs() {
    let mut components = IndexMap::default();
    components.insert(
        "n".to_string(),
        ast::Component {
            name: "n".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            // Unresolvable start should not override explicit binding.
            start: ast::Expression::ComponentReference(cref("missing.scope.value")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    let value = try_eval_integer_expr(
        &ctx,
        &add_expr(ast::Expression::ComponentReference(cref("n")), int_expr(1)),
    );

    assert_eq!(value, Some(2));
}

#[test]
fn evaluate_array_dimensions_prefers_binding_over_start_for_shape_refs() {
    let mut components = IndexMap::default();
    components.insert(
        "m".to_string(),
        ast::Component {
            name: "m".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            // Keep start unresolved and provide the structural value via binding.
            start: ast::Expression::ComponentReference(cref("missing.scope.value")),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let dims = evaluate_array_dimensions(
        &[0],
        &[ast::Subscript::Expression(add_expr(
            ast::Expression::ComponentReference(cref("m")),
            int_expr(1),
        ))],
        &ast::ModificationEnvironment::new(),
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, Some(vec![2]));
}

#[test]
fn evaluate_array_dimensions_reads_record_field_from_class_modification() {
    let mut components = IndexMap::default();
    components.insert(
        "stackData".to_string(),
        ast::Component {
            name: "stackData".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut mod_env = ast::ModificationEnvironment::new();
    let stack_data_mod = ast::Expression::ClassModification {
        target: cref("StackData"),
        modifications: vec![
            ast::Expression::NamedArgument {
                name: token("Ns"),
                value: Arc::new(int_expr(3)),

                span: rumoca_core::Span::DUMMY,
            },
            ast::Expression::NamedArgument {
                name: token("Np"),
                value: Arc::new(int_expr(2)),

                span: rumoca_core::Span::DUMMY,
            },
        ],

        each_flags: vec![false, false],
        final_flags: vec![false, false],
        redeclare_flags: vec![false, false],
        span: rumoca_core::Span::DUMMY,
    };
    mod_env.add(
        ast::QualifiedName::from_ident("stackData"),
        ast::ModificationValue::simple(stack_data_mod),
    );

    let dims = evaluate_array_dimensions(
        &[1, 1],
        &[
            ast::Subscript::Expression(ast::Expression::ComponentReference(cref("stackData.Ns"))),
            ast::Subscript::Expression(ast::Expression::ComponentReference(cref("stackData.Np"))),
        ],
        &mod_env,
        &components,
        &ast::ClassTree::new(),
        no_op_resolve_class_components,
    );

    assert_eq!(dims, Some(vec![3, 2]));
}

#[test]
fn try_eval_integer_expr_resolves_enclosing_scope_component_ref() {
    let mut components = IndexMap::default();
    components.insert(
        "pipe2.nFM".to_string(),
        ast::Component {
            name: "pipe2.nFM".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            start: int_expr(1),
            binding: Some(int_expr(1)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    let value = try_eval_integer_expr(
        &ctx,
        &add_expr(
            ast::Expression::ComponentReference(cref("pipe2.flowModel.nFM")),
            int_expr(1),
        ),
    );

    assert_eq!(value, Some(2));
}

#[test]
fn try_eval_integer_expr_evaluates_if_expressions() {
    let expr = if_expr(vec![(bool_expr(true), int_expr(2))], int_expr(1));
    let ctx = InstantiateEvalCtx {
        tree: &ast::ClassTree::new(),
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &IndexMap::default(),
        resolve_class_components: no_op_resolve_class_components,
    };
    let value = try_eval_integer_expr(&ctx, &expr);

    assert_eq!(value, Some(2));
}

#[test]
fn try_eval_integer_expr_evaluates_parameterized_if_expressions() {
    let mut components = IndexMap::default();
    components.insert(
        "ParDesired".to_string(),
        ast::Component {
            name: "ParDesired".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    components.insert(
        "mSystems".to_string(),
        ast::Component {
            name: "mSystems".to_string(),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            binding: Some(int_expr(2)),
            has_explicit_binding: true,
            ..ast::Component::empty_with_span(test_span())
        },
    );
    let expr = if_expr(
        vec![(
            func_call(
                "Modelica.Math.isPowerOf2",
                vec![ast::Expression::ComponentReference(cref("ParDesired"))],
            ),
            ast::Expression::ComponentReference(cref("ParDesired")),
        )],
        ast::Expression::ComponentReference(cref("mSystems")),
    );
    let tree = msl_math_tree();
    let ctx = InstantiateEvalCtx {
        tree: &tree,
        mod_env: &ast::ModificationEnvironment::new(),
        effective_components: &components,
        resolve_class_components: no_op_resolve_class_components,
    };

    assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(2));
}
