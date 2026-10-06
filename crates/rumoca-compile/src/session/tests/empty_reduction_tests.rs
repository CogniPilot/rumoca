use super::*;
use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae::{ExpressionOperation, PureBuiltin, ScalarType};

fn compile(source: &str, name: &str) -> anyhow::Result<CompilationResult> {
    let mut session = Session::default();
    session.add_document("empty_reduction.mo", source)?;
    session.compile_model(name)
}

fn checked_reductions(result: &CompilationResult) -> Vec<(ScalarType, Vec<u32>, f64)> {
    result.dae.inspect(|view| {
        let mut evaluator = NumericEvaluator::new(view);
        let mut reductions = Vec::new();
        for ordinal in 0..view.expression_count() {
            let id = view.expression_id(ordinal).unwrap();
            let expression = view.expression(id).unwrap();
            let ExpressionOperation::Builtin {
                builtin: PureBuiltin::Sum | PureBuiltin::Product,
                arguments,
            } = expression.operation()
            else {
                continue;
            };
            let argument = view.expression(arguments.get(0).unwrap()).unwrap();
            assert_eq!(
                argument.value_type().scalar_type(),
                expression.value_type().scalar_type(),
            );
            if let ExpressionOperation::Comprehension { domain, .. } = argument.operation() {
                assert_eq!(view.domain(domain).unwrap().scalar_count(), 0);
            }
            reductions.push((
                expression.value_type().scalar_type(),
                argument.value_type().dimensions().to_vec(),
                evaluator.expression(id).expect("checked native reduction")[0],
            ));
        }
        reductions
    })
}

#[test]
fn empty_reduction_has_checked_real_and_integer_identities() {
    let result = compile(
        "model EmptyIdentity output Real sr; output Real pr;
         output Integer si; output Integer pi; equation
         sr=sum(k+0.5 for k in 1:0); pr=product(k+0.5 for k in 1:0);
         si=sum(k for k in 1:0); pi=product(k for k in 1:0);
         end EmptyIdentity;",
        "EmptyIdentity",
    )
    .expect("empty domains derive the body type without a witness element");
    assert_eq!(
        checked_reductions(&result),
        [
            (ScalarType::Real, vec![0], 0.0),
            (ScalarType::Real, vec![0], 1.0),
            (ScalarType::Integer, vec![0], 0.0),
            (ScalarType::Integer, vec![0], 1.0),
        ]
    );
}

#[test]
fn empty_reduction_retains_nested_shape_and_shadowed_binders() {
    let result = compile(
        "model NestedEmpty output Real s; output Real p; equation
         s=sum({i+j+0.5 for j in 1:2} for i in 1:0);
         p=product({i+0.5 for i in 1:0} for i in 1:2);
         end NestedEmpty;",
        "NestedEmpty",
    )
    .expect("nested empty constructors retain both axes and lexical scope");
    assert_eq!(
        checked_reductions(&result),
        [
            (ScalarType::Real, vec![0, 2], 0.0),
            (ScalarType::Real, vec![2, 0], 1.0),
        ]
    );
}

#[test]
fn empty_reduction_does_not_read_body_coordinates() {
    let result = compile(
        "model EmptyCoordinates input Real a[6]; input Integer b[6];
         output Real s; output Integer p; equation
         s=sum(a[k]*a[k] for k in 1:0); p=product(b[k] for k in 1:0);
         end EmptyCoordinates;",
        "EmptyCoordinates",
    )
    .expect("the body is checked while its zero-iteration evaluation stays empty");
    assert_eq!(
        checked_reductions(&result),
        [
            (ScalarType::Real, vec![0], 0.0),
            (ScalarType::Integer, vec![0], 1.0)
        ]
    );
}

#[test]
fn empty_reduction_does_not_hide_incompatible_body_shapes() {
    let error = compile(
        "model InvalidEmpty input Real a[2]; input Real b[3]; output Real s;
         equation s=sum(a*b for k in 1:0); end InvalidEmpty;",
        "InvalidEmpty",
    )
    .expect_err("MLS §10.7 requires body dimension checks even with no elements");
    assert!(!error.to_string().contains("empty array"), "{error:#}");
}

#[test]
fn empty_reduction_does_not_accept_nonnumeric_body_type() {
    let error = compile(
        "model InvalidEmptyType output Boolean s;
         equation s=sum(true for k in 1:0); end InvalidEmptyType;",
        "InvalidEmptyType",
    )
    .expect_err("the checked empty element type must still be numeric");
    assert!(!error.to_string().contains("empty array"), "{error:#}");
}

#[test]
fn empty_reduction_does_not_hide_nested_runtime_extent() {
    let error = compile(
        "model DynamicEmpty input Integer n; input Real a[6]; output Real s;
         equation s=sum(sum(a[j] for j in 1:n) for k in 1:0); end DynamicEmpty;",
        "DynamicEmpty",
    )
    .expect_err("an empty enclosing domain cannot certify a runtime inner extent");
    assert!(!error.to_string().contains("empty array"), "{error:#}");
}

#[test]
fn empty_reduction_retains_rectangular_and_descending_domains() {
    let result = compile(
        "model EmptyDomains output Integer s; output Real p; equation
         s=sum(i+j for i in 1:0, j in 1:3);
         p=product(k+0.5 for k in 2:-1:3); end EmptyDomains;",
        "EmptyDomains",
    )
    .expect("empty rectangular and descending domains need no synthetic element");
    assert_eq!(
        checked_reductions(&result),
        [
            (ScalarType::Integer, vec![0, 3], 0.0),
            (ScalarType::Real, vec![0], 1.0)
        ]
    );
}

#[test]
fn empty_reduction_preserves_nonempty_numeric_controls() {
    let result = compile(
        "model NonemptyIdentity output Real s; output Integer p; equation
         s=sum(k+0.5 for k in 1:3); p=product(k for k in 1:3);
         end NonemptyIdentity;",
        "NonemptyIdentity",
    )
    .expect("nonempty constructors retain their original expansion");
    assert_eq!(
        checked_reductions(&result),
        [
            (ScalarType::Real, vec![3], 7.5),
            (ScalarType::Integer, vec![3], 6.0)
        ]
    );
}

#[test]
fn empty_reduction_compiles_generic_cholesky_first_row() {
    let result = compile(include_str!("fixtures/spd6_typed_empty.mo"), "SPD6Solve")
        .expect("the generic six-by-six Cholesky includes its first empty sum");
    assert!(result.is_balanced());
}
