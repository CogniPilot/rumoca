use super::*;

#[test]
fn test_full_path_name_runtime_special_is_not_numeric_placeholder() {
    let env = VarEnv::<f64>::new();
    let result = eval_expr::<f64>(
        &fn_call(
            "Modelica.Utilities.Files.fullPathName",
            vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("a.txt".to_string()),
                span: rumoca_core::Span::DUMMY,
            }],
        ),
        &env,
    );
    assert_eq!(
        result,
        Err(EvalError::UnsupportedExpression {
            kind: "string/file intrinsic"
        })
    );
}

#[test]
fn start_expr_treats_load_resource_as_nonnumeric() {
    let env = VarEnv::<f64>::new();
    let expr = fn_call(
        "Modelica.Utilities.Files.loadResource",
        vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("modelica://Pkg/Resources/a.idf".to_string()),
            span: rumoca_core::Span::DUMMY,
        }],
    );

    assert!(start_expr_is_nonnumeric(&expr, &env));
}

#[test]
fn start_expr_treats_subscripted_string_parameter_as_nonnumeric() {
    let mut env = VarEnv::<f64>::new();
    env.nonnumeric_names =
        std::sync::Arc::new(std::collections::HashSet::from(["zoneName".to_string()]));
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("floor.zoneName"),
        subscripts: vec![
            rumoca_core::Subscript::generated_expr(
                Box::new(var("floorIndex")),
                rumoca_core::Span::DUMMY,
            ),
            rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert!(start_expr_is_nonnumeric(&expr, &env));
}
