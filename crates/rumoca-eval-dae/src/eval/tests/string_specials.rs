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
