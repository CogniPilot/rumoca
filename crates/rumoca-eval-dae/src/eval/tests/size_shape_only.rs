//! Shape-only evaluation of `size()` (MLS §10.3.1).
//!
//! Media packages inline `Medium.substanceNames` and
//! `Medium.extraPropertiesNames` as String arrays, and every numeric shape
//! derived from them reaches the DAE evaluator as `size(<String array>, d)`.

use super::*;

/// MLS §10.3.1: `size(A, d)` reads only `A`'s dimensions. A String array
/// literal therefore has a well-defined size even though its elements carry no
/// numeric value.
#[test]
fn size_of_string_array_literal_evaluates_to_its_dimension() {
    let substance_names = rumoca_core::Expression::Array {
        elements: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("N2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("O2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let env = VarEnv::<f64>::new();

    assert_eq!(
        eval_expr::<f64>(
            &builtin(
                rumoca_core::BuiltinFunction::Size,
                vec![substance_names, int_lit(1)],
            ),
            &env,
        ),
        Ok(2.0)
    );
}

/// `Medium.extraPropertiesNames = fill("", 0)` flattens into every
/// `Modelica.Fluid` `C_start`/`mC_scaled` shape as `size(fill("", 0), 1)`. The
/// dimension comes from `fill`'s size argument, so the String seed never needs
/// a numeric value.
#[test]
fn size_of_string_seeded_fill_reads_its_size_argument() {
    let extra_properties_names = builtin(
        rumoca_core::BuiltinFunction::Fill,
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(String::new()),
                span: rumoca_core::Span::DUMMY,
            },
            int_lit(0),
        ],
    );
    let env = VarEnv::<f64>::new();

    assert_eq!(
        eval_expr::<f64>(
            &builtin(
                rumoca_core::BuiltinFunction::Size,
                vec![extra_properties_names, int_lit(1)],
            ),
            &env,
        ),
        Ok(0.0)
    );
}

/// `Medium.X_default` flattens to `fill(1/size(substanceNames, 1),
/// size(substanceNames, 1))` with `substanceNames` inlined as a String array
/// literal. Passing it to a medium function (e.g. `density_pTX`) must not
/// reject the call because the shape argument holds Strings.
#[test]
fn string_sized_fill_is_accepted_as_a_user_function_argument() {
    let substance_names = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("SimpleLiquidWater".to_string()),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let size = builtin(
        rumoca_core::BuiltinFunction::Size,
        vec![substance_names, int_lit(1)],
    );
    let x_default = builtin(
        rumoca_core::BuiltinFunction::Fill,
        vec![
            binop(rumoca_core::OpBinary::Div, int_lit(1), size.clone()),
            size,
        ],
    );

    let mut function = Function::new("Medium.density_pTX", rumoca_core::Span::DUMMY);
    function.add_input(FunctionParam::new(
        "p",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.add_input(
        FunctionParam::new("X", "Real", rumoca_core::Span::source_free_serde_default())
            .with_dims(vec![1]),
    );
    function.add_output(FunctionParam::new(
        "d",
        "Real",
        rumoca_core::Span::source_free_serde_default(),
    ));
    function.body.push(Statement::Assignment {
        comp: comp_ref("d"),
        value: index_expr(var("X"), 1),
        span: rumoca_core::Span::DUMMY,
    });

    let mut env = VarEnv::<f64>::new();
    env.functions = Arc::new(IndexMap::from([(
        "Medium.density_pTX".to_string(),
        function,
    )]));

    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Medium.density_pTX", vec![lit(1e5), x_default]),
            &env,
        ),
        Ok(1.0)
    );
}

#[test]
fn test_build_env_discrete_start_forward_ref_re_evaluates_and_preserves_pre_seed() {
    clear_pre_values();

    let mut dae = rumoca_ir_dae::Dae::default();

    // Insert dependent start first to exercise forward-reference handling.
    let mut a = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("a"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    a.start = Some(dae_var("b"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), a);

    let mut b = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("b"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    b.start = Some(dae_bool_lit(true));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), b);

    let env = build_env(&dae, &[], &[], 0.0).expect("test env should build");
    assert_eq!(env_value(&env, "b"), 1.0);
    assert_eq!(env_value(&env, "a"), 1.0);

    // Pre-seeded values must take precedence over start expressions.
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("a", 0.0);
    pre_env.set("b", 0.0);
    seed_pre_values_from_env(&pre_env);

    let env_from_pre = build_env_with_runtime(&dae, &[], &[], 1.0, pre_env.runtime.clone())
        .expect("test env should build");
    assert_eq!(env_value(&env_from_pre, "a"), 0.0);
    assert_eq!(env_value(&env_from_pre, "b"), 0.0);

    clear_pre_values();
}
