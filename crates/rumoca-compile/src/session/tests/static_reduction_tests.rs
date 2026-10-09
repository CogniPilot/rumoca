use super::*;

fn compile(source: &str, name: &str) -> anyhow::Result<CompilationResult> {
    let mut session = Session::default();
    session.add_document("static_reduction.mo", source)?;
    session.compile_model(name)
}

fn reduction_lengths(model: &flat::Model) -> Vec<usize> {
    use rumoca_core::{BuiltinFunction, Expression, ExpressionVisitor};
    struct Lengths(Vec<usize>);
    impl ExpressionVisitor for Lengths {
        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if *function == BuiltinFunction::Sum
                && let [Expression::Array { elements, .. }] = args
            {
                self.0.push(elements.len());
            }
            self.walk_builtin_call(function, args);
        }
    }
    let mut lengths = Lengths(Vec::new());
    for equation in &model.equations {
        lengths.visit_expression(&equation.residual);
    }
    lengths.0
}

#[test]
fn static_reduction_specializes_each_proven_outer_loop_index() {
    let result = compile(
        "model PrefixSum input Real a[6]; output Real p[6]; equation
         p[1]=a[1]; for i in 2:6 loop p[i]=sum(a[k] for k in 1:i-1); end for;
         end PrefixSum;",
        "PrefixSum",
    )
    .expect("each static outer iteration has an exact reduction domain");
    assert!(result.is_balanced());
    assert_eq!(result.flat.equations.len(), 6);
    assert_eq!(reduction_lengths(&result.flat), [1, 2, 3, 4, 5]);
    let family = &result.flat.structured_equations[0];
    assert_eq!(family.domain.scalar_count().unwrap(), 5);
    assert!(family.interiors_materialized());
    assert!(family.template.is_none());
}

#[test]
fn static_reduction_preserves_empty_real_comprehension_type() {
    let result = compile(
        "model EmptyPrefix input Real a[6]; output Real p[6]; equation
         for i in 1:6 loop p[i]=sum(a[k]*a[k] for k in 1:i-1); end for;
         end EmptyPrefix;",
        "EmptyPrefix",
    )
    .expect("the first empty reduction retains its checked Real element type");
    assert!(result.is_balanced());
}

#[test]
fn static_reduction_keeps_constant_domain_templates_compact() {
    let result = compile(
        "model RowSum input Real a[6,6]; output Real p[6]; equation
         for i in 1:6 loop p[i]=sum(a[i,k] for k in 1:6); end for;
         end RowSum;",
        "RowSum",
    )
    .expect("fixed reduction domains retain symbolic family ownership");
    assert!(result.is_balanced());
    assert!(result.flat.structured_equations[0].template.is_some());
}

#[test]
fn static_reduction_respects_comprehension_binder_shadowing() {
    let result = compile(
        "model ShadowSum input Real a[6]; output Real p[6]; equation
         for i in 1:6 loop p[i]=sum(a[i] for i in 1:6); end for;
         end ShadowSum;",
        "ShadowSum",
    )
    .expect("the inner i shadows the enclosing for-equation i");
    assert!(result.is_balanced());
    assert!(result.flat.structured_equations[0].template.is_some());
}

#[test]
fn static_reduction_materializes_dependent_state_derivative_rows() {
    let result = compile(
        "model PrefixDerivative input Real a[6]; Real p[6]; equation
         der(p[1])=a[1];
         for i in 2:6 loop der(p[i])=sum(a[k] for k in 1:i-1); end for;
         end PrefixDerivative;",
        "PrefixDerivative",
    )
    .expect("dependent reductions require complete state-derivative rows");
    assert!(result.is_balanced());
    assert_eq!(reduction_lengths(&result.flat), [1, 2, 3, 4, 5]);
    let family = &result.flat.structured_equations[0];
    assert!(family.interiors_materialized());
    assert!(family.template.is_none());
}

#[test]
fn static_reduction_does_not_accept_runtime_extents() {
    let error = compile(
        "model DynamicSum input Integer n; input Real a[6]; output Real p[6];
         equation for i in 1:6 loop p[i]=sum(a[k] for k in 1:n); end for;
         end DynamicSum;",
        "DynamicSum",
    )
    .expect_err("a runtime input is not a translation-time extent");
    assert!(error.to_string().contains("Flatten"), "{error:#}");
}

#[test]
fn static_reduction_compiles_dependent_cholesky_and_sixteen_rhs() {
    let result = compile(include_str!("fixtures/spd6_dependent.mo"), "SPD6Solve")
        .expect("static Cholesky and forward/back substitutions must specialize");
    assert!(result.is_balanced());
}

#[test]
fn static_reduction_registers_initial_template_domains() {
    let result = compile(
        "model InitRowSum input Real a[6,6]; Real p[6]; initial equation
         for i in 1:6 loop p[i]=sum(a[i,k] for k in 1:6); end for;
         equation der(p)=fill(0.0,6); end InitRowSum;",
        "InitRowSum",
    )
    .expect("initialization template comprehensions also need checked plans");
    assert!(result.is_balanced());
    assert!(
        result.flat.initial_structured_equations[0]
            .template
            .is_some()
    );
}

#[test]
fn static_reduction_registers_nested_covariance_template_domains() {
    let result = compile(
        include_str!("fixtures/es15_nested_reductions.mo"),
        "ES15CovariancePrediction",
    )
    .expect("both fixed nested domains in Q must have exact certificates");
    assert!(result.is_balanced());
    assert!(result.flat.structured_equations.iter().any(|family| {
        family.template.is_some() && family.domain.scalar_count().unwrap() == 225
    }));
}
