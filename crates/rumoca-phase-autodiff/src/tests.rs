//! Unit checks of the surface, the engine, and the refusal boundary.

mod admission;
mod bindings;
mod calls;
mod refusals;
mod shapes;

use crate::{Refusal, Rule, expand_source, may_expand};

const SCALED: &str = r"
function fscale
  input Real x[3];
  input Real k;
  output Real y[2];
protected
  Real t;
algorithm
  t := k*x[1];
  y[1] := t*x[2] + sin(x[3]);
  y[2] := exp(x[1])*x[2];
end fscale;

model Probe
  parameter Real x[3] = {0.3, -1.2, 0.7};
  parameter Real k = 2.5;
  Real J[2, 3] = jacobian(fscale(x, k), x);
end Probe;
";

fn refuse(source: &str) -> Refusal {
    match expand_source(source, "Refused.mo") {
        Err(crate::ExpansionError::Refused(refusal)) => refusal,
        Err(other) => panic!("expected a refusal, got {other}"),
        Ok(text) => panic!("expected a refusal, expansion produced:\n{text}"),
    }
}

fn expand(source: &str) -> String {
    expand_source(source, "Probe.mo").expect("expansion runs")
}

#[test]
fn a_source_without_the_surface_is_returned_unchanged() {
    let source = "model M\n  Real x;\nequation\n  der(x) = -x;\nend M;\n";
    assert!(!may_expand(source));
    assert_eq!(
        expand_source(source, "M.mo").expect("expansion runs"),
        source
    );
}

#[test]
fn derivative_call_arguments_are_not_a_surface_traversal_barrier() {
    let expanded = expand(
        r#"
function f
  input Real x;
  output Real y;
algorithm
  y := x*x;
end f;
model Probe
  Real x;
equation
  der(jacobian(f(x), x)) = 0.0;
end Probe;
"#,
    );

    assert!(
        expanded.contains("der(f_jacobian_x(x))"),
        "the nested surface call inside a typed derivative argument must be rewritten:\n{expanded}"
    );
}

#[test]
fn expansion_mints_the_wrapper_and_rewrites_the_call() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("function fscale_jacobian_x"),
        "missing wrapper:\n{expanded}"
    );
    assert!(
        expanded.contains("function fscale_ad_tangent"),
        "missing tangent:\n{expanded}"
    );
    assert!(
        expanded.contains("Real J[2, 3] = fscale_jacobian_x(x, k);"),
        "call not rewritten:\n{expanded}"
    );
    // The only `jacobian(` left is inside the provenance description string
    // the generated functions carry back to the minting site.
    assert!(
        !expanded.contains("= jacobian("),
        "surface call survived:\n{expanded}"
    );
}

#[test]
fn an_unshadowed_predefined_real_remains_differentiable() {
    let expanded = expand(SCALED);
    assert!(expanded.contains("function fscale_ad_tangent"));
}

#[test]
fn a_root_within_clause_keeps_predefined_real_available() {
    let expanded = expand(
        r#"
within;
function f
  input Real x;
  output Real y;
algorithm
  y := x;
end f;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(f(x), x);
end Probe;
"#,
    );
    assert!(expanded.contains("function f_ad_tangent"));
}

#[test]
fn an_unavailable_within_package_cannot_certify_predefined_real() {
    let refusal = refuse(
        r#"
within Outer;
function f
  input Real x;
  output Real y;
algorithm
  y := x;
end f;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(f(x), x);
end Probe;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("cannot prove"));
    assert!(refusal.detail.contains("within"));
}

#[test]
fn a_local_type_named_real_cannot_mint_a_real_tangent() {
    let refusal = refuse(
        r#"
function f
  type Real = Integer;
  input Real x;
  output Real y;
algorithm
  y := x;
end f;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(f(x), x);
end Probe;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("cannot prove"));
    assert!(refusal.detail.contains("local class or type"));
}

#[test]
fn an_import_named_real_cannot_mint_a_real_tangent() {
    let refusal = refuse(
        r#"
package Types
  type Real = Integer;
end Types;
function f
  import Types.Real;
  input Real x;
  output Real y;
algorithm
  y := x;
end f;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(f(x), x);
end Probe;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("cannot prove"));
    assert!(refusal.detail.contains("import"));
}

#[test]
fn an_inherited_type_named_real_cannot_mint_a_real_tangent() {
    let refusal = refuse(
        r#"
package Types
  type Real = Integer;
end Types;
package Host
  extends Types;
  function f
    input Real x;
    output Real y;
  algorithm
    y := x;
  end f;
end Host;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(Host.f(x), x);
end Probe;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("cannot prove"));
    assert!(refusal.detail.contains("extends clause"));
}

#[test]
fn unresolved_enclosing_extends_cannot_fall_through_to_predefined_real() {
    let refusal = refuse(
        r#"
package Host
  extends MissingTypes;
  function f
    input Real x;
    output Real y;
  algorithm
    y := x;
  end f;
end Host;
model Probe
  parameter Real x = 1.0;
  Real J = jacobian(Host.f(x), x);
end Probe;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("cannot prove"));
    assert!(refusal.detail.contains("extends clause"));
}

#[test]
fn recovery_nodes_refuse_expansion_while_empty_sections_remain_legal() {
    let mut definition =
        rumoca_phase_parse::parse_to_ast(SCALED, "Recovery.mo").expect("fixture parses");
    {
        let probe = definition
            .classes
            .get_mut("Probe")
            .expect("fixture owns Probe");
        probe.algorithms.push(Vec::new());
        probe.equations.clear();
    }

    crate::plan(&definition, "Recovery.mo").expect("empty section vectors are valid syntax");

    definition
        .classes
        .get_mut("Probe")
        .expect("fixture owns Probe")
        .algorithms[0]
        .push(rumoca_ir_ast::Statement::Empty);
    let statement_refusal = crate::plan(&definition, "Recovery.mo")
        .expect_err("a recovery statement must not mint an expansion");
    assert_eq!(statement_refusal.rule, Rule::StatementForm);
    assert!(statement_refusal.detail.contains("parser recovery"));
    assert!(
        statement_refusal.span().is_some(),
        "the nearest class owner supplies recovery provenance"
    );

    {
        let probe = definition
            .classes
            .get_mut("Probe")
            .expect("fixture owns Probe");
        probe.algorithms[0].clear();
        probe.equations.push(rumoca_ir_ast::Equation::Empty);
    }
    let equation_refusal = crate::plan(&definition, "Recovery.mo")
        .expect_err("a recovery equation must not mint an expansion");
    assert_eq!(equation_refusal.rule, Rule::StatementForm);
    assert!(equation_refusal.detail.contains("parser recovery"));
    assert!(
        equation_refusal.span().is_some(),
        "the nearest class owner supplies recovery provenance"
    );
}

#[test]
fn every_expression_recovery_shape_refuses_before_site_collection() {
    use rumoca_core::{OpBinary, OpUnary, Span, Token};
    use rumoca_ir_ast::{
        ComponentRefPart, ComponentReference, Expression, Subscript, TerminalType,
    };
    use std::sync::Arc;

    let literal = || Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from("1"),
            ..Token::default()
        },
        span: Span::DUMMY,
    };
    let forms = vec![
        Expression::Empty { span: Span::DUMMY },
        Expression::Terminal {
            terminal_type: TerminalType::Empty,
            token: Token::default(),
            span: Span::DUMMY,
        },
        Expression::Unary {
            op: OpUnary::Empty,
            rhs: Arc::new(literal()),
            span: Span::DUMMY,
        },
        Expression::Binary {
            op: OpBinary::Empty,
            lhs: Arc::new(literal()),
            rhs: Arc::new(literal()),
            span: Span::DUMMY,
        },
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token::default(),
                subs: Some(vec![Subscript::Empty]),
                def_id: None,
            }],
            span: Span::DUMMY,
            qualified_display_name: None,
        }),
        Expression::ArrayIndex {
            base: Arc::new(literal()),
            subscripts: vec![Subscript::Expression(Expression::Empty {
                span: Span::DUMMY,
            })],
            span: Span::DUMMY,
        },
    ];

    for recovery in forms {
        let mut definition =
            rumoca_phase_parse::parse_to_ast(SCALED, "Recovery.mo").expect("fixture parses");
        definition
            .classes
            .get_mut("Probe")
            .expect("fixture owns Probe")
            .components
            .get_mut("x")
            .expect("fixture owns x")
            .binding = Some(recovery);
        let refusal = crate::plan(&definition, "Recovery.mo")
            .expect_err("required expression recovery must refuse expansion");
        assert_eq!(refusal.rule, Rule::ExpressionForm);
        assert!(refusal.detail.contains("parser-recovery"));
    }
}

#[test]
fn recovery_in_statement_control_and_assertion_paths_refuses() {
    use rumoca_core::{Span, Token};
    use rumoca_ir_ast::{Expression, ForIndex, Statement, StatementBlock, TerminalType};
    use std::sync::Arc;

    let recovery = || Expression::Empty { span: Span::DUMMY };
    let message = || Expression::Terminal {
        terminal_type: TerminalType::String,
        token: Token {
            text: Arc::from("recovery"),
            ..Token::default()
        },
        span: Span::DUMMY,
    };
    let statements = vec![
        Statement::For {
            indices: vec![ForIndex {
                ident: Token::default(),
                range: recovery(),
            }],
            equations: Vec::new(),
        },
        Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: recovery(),
                stmts: Vec::new(),
            }],
            else_block: None,
        },
        Statement::Assert {
            condition: recovery(),
            message: message(),
            level: None,
        },
    ];

    for statement in statements {
        let mut definition =
            rumoca_phase_parse::parse_to_ast(SCALED, "Recovery.mo").expect("fixture parses");
        definition
            .classes
            .get_mut("fscale")
            .expect("fixture owns fscale")
            .algorithms = vec![vec![statement]];
        let refusal = crate::plan(&definition, "Recovery.mo")
            .expect_err("recovery in a statement-owned expression must refuse");
        assert_eq!(refusal.rule, Rule::ExpressionForm);
        assert!(refusal.detail.contains("parser-recovery"));
    }
}

#[test]
fn the_expansion_carries_its_minting_site() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("\"jacobian(fscale(x, k), x) at Probe.mo:"),
        "wrapper lost its provenance:\n{expanded}"
    );
}

#[test]
fn a_name_that_merely_ends_in_the_surface_is_not_a_call_site() {
    for source in [
        "  J := LieGroups.SO3.Quat.right_jacobian(tangent);",
        "  J := Lib.jacobian(a, b);",
        "  // jacobian of the measurement model",
        "  Real jacobianRow[3];",
    ] {
        assert!(
            !may_expand(source),
            "the scan must not wake the expander for: {source}"
        );
    }
    assert!(may_expand("  Real J[1, 1] = jacobian (f(x), x);"));
}

#[test]
fn a_declared_jacobian_wins_over_the_construct() {
    let source = r"
function jacobian
  input Real a[2, 2];
  input Real b[2];
  output Real y[2];
algorithm
  y := a*b;
end jacobian;

model Uses
  parameter Real a[2, 2] = {{1.0, 0.0}, {0.0, 1.0}};
  parameter Real b[2] = {2.0, 3.0};
  Real y[2] = jacobian(a, b);
end Uses;
";
    assert_eq!(
        expand_source(source, "Uses.mo").expect("a declared name is left alone"),
        source
    );
}

#[test]
fn declared_surface_opt_out_does_not_hide_recovery_nodes() {
    let source = r"
function jacobian
  input Real x;
  output Real y;
algorithm
  y := x;
end jacobian;

model Uses
  Real y = jacobian(1.0);
end Uses;
";
    let mut definition =
        rumoca_phase_parse::parse_to_ast(source, "Recovery.mo").expect("fixture parses");
    definition
        .classes
        .get_mut("Uses")
        .expect("fixture owns Uses")
        .algorithms
        .push(vec![rumoca_ir_ast::Statement::Empty]);

    let refusal = crate::plan(&definition, "Recovery.mo")
        .expect_err("declared-name opt-out must still reject recovery syntax");
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.detail.contains("parser recovery"));

    {
        let uses = definition
            .classes
            .get_mut("Uses")
            .expect("fixture owns Uses");
        uses.algorithms.clear();
        uses.components
            .get_mut("y")
            .expect("fixture owns y")
            .binding = Some(rumoca_ir_ast::Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        });
    }
    let refusal = crate::plan(&definition, "Recovery.mo")
        .expect_err("declared-name opt-out must still reject expression recovery");
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.detail.contains("parser-recovery"));
}

#[test]
fn the_generated_text_reparses() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    rumoca_phase_parse::parse_to_ast(&expanded, "Expanded.mo").expect("expansion reparses");
}

#[test]
fn a_product_takes_the_product_rule() {
    let expanded = expand_source(SCALED, "Probe.mo").expect("expansion runs");
    assert!(
        expanded.contains("(k_ad) * (x[1]) + (k) * (x_ad[1])"),
        "product rule not emitted:\n{expanded}"
    );
}

#[test]
fn a_while_loop_refuses() {
    let refusal = refuse(
        r"
function walk
  input Real x;
  output Real y;
protected
  Real step;
algorithm
  y := x;
  step := 0;
  while step < 3 loop
    y := y*x;
    step := step + 1;
  end while;
end walk;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(walk(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::StatementForm);
    assert!(refusal.to_string().contains("JAC-R4"), "{refusal}");
}

#[test]
fn a_non_differentiable_builtin_refuses() {
    let refusal = refuse(
        r"
function stepper
  input Real x;
  output Real y;
algorithm
  y := sign(x)*x;
end stepper;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(stepper(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::ExpressionForm);
    assert!(refusal.to_string().contains("sign"), "{refusal}");
}

#[test]
fn an_external_function_refuses() {
    let refusal = refuse(
        r#"
function outside
  input Real x;
  output Real y;
  external "C" y = outside(x);
end outside;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(outside(x), x);
end Refused;
"#,
    );
    assert_eq!(refusal.rule, Rule::CalleeTangent);
    assert!(refusal.to_string().contains("external"), "{refusal}");
}

#[test]
fn an_unknown_function_refuses() {
    let refusal = refuse(
        r"
model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(missing(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
}

#[test]
fn an_argument_that_is_not_a_reference_refuses() {
    let refusal = refuse(
        r"
function twice
  input Real x;
  output Real y;
algorithm
  y := 2*x;
end twice;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(twice(2*x), 2*x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
}

#[test]
fn a_repeated_argument_refuses() {
    let refusal = refuse(
        r"
function pair
  input Real a;
  input Real b;
  output Real y;
algorithm
  y := a*b;
end pair;

model Refused
  parameter Real x = 1.5;
  Real J[1, 1] = jacobian(pair(x, x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
}

#[test]
fn a_symbolic_input_dimension_refuses() {
    let refusal = refuse(
        r"
function total
  input Integer n;
  input Real x[n];
  output Real y;
algorithm
  y := sum(x);
end total;

model Refused
  parameter Integer n = 3;
  parameter Real x[3] = {1.0, 2.0, 3.0};
  Real J[1, 3] = jacobian(total(n, x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::Signature);
}

#[test]
fn a_record_typed_port_refuses() {
    let refusal = refuse(
        r"
record Pose
  Real p[3];
end Pose;

function height
  input Pose pose;
  output Real y;
algorithm
  y := pose.p[3];
end height;

model Refused
  Pose pose;
  Real J[1, 1] = jacobian(height(pose), pose);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::DifferentiableType);
}

#[test]
fn context_only_value_syntax_refuses_jacobian_synthesis() {
    for make_invalid in [
        |valid: &rumoca_ir_ast::Expression| rumoca_ir_ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs: std::sync::Arc::new(valid.clone()),
            rhs: std::sync::Arc::new(valid.clone()),
            span: valid.span(),
        },
        |valid: &rumoca_ir_ast::Expression| rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::End,
            token: rumoca_core::Token::default(),
            span: valid.span(),
        },
    ] {
        let mut definition =
            rumoca_phase_parse::parse_to_ast(SCALED, "ContextOnly.mo").expect("fixture parses");
        let function = definition
            .classes
            .get_mut("fscale")
            .expect("fixture owns fscale");
        let rumoca_ir_ast::Statement::Assignment { value, .. } = &mut function.algorithms[0][0]
        else {
            panic!("fixture starts with an assignment");
        };
        let valid = value.clone();
        let invalid = make_invalid(&valid);
        *value = rumoca_ir_ast::Expression::If {
            branches: vec![(invalid, valid.clone())],
            else_branch: std::sync::Arc::new(valid.clone()),
            span: valid.span(),
        };
        let refusal = crate::plan(&definition, "ContextOnly.mo")
            .expect_err("context-only value syntax must fail before differentiation");
        assert_eq!(refusal.rule, Rule::ExpressionForm);
        assert!(refusal.to_string().contains("JAC-R5"), "{refusal}");
        assert_eq!(refusal.span(), Some(valid.span()));
    }
}

#[test]
fn end_remains_legal_inside_a_differentiated_array_subscript() {
    let source = r"
function last
  input Real x[2];
  output Real y;
algorithm
  y := x[end];
end last;

model Probe
  parameter Real x[2] = {1.0, 2.0};
  Real J[1, 2] = jacobian(last(x), x);
end Probe;
";
    let expanded = expand(source);
    assert!(expanded.contains("x_ad[end]"), "{expanded}");
}

#[test]
fn surface_calls_in_conditions_and_array_indices_are_not_skipped() {
    let source = r"
function identity
  input Real x;
  output Real y;
algorithm
  y := x;
end identity;

model Probe
  parameter Real x = 1.0;
  Real table[1] = {2.0};
  Real y = table[size(jacobian(identity(x), x), 1)];
  Boolean b;
algorithm
  if size(jacobian(identity(x), x), 1) > 0 then
    b := true;
  end if;
end Probe;
";
    let expanded = expand(source);
    assert_eq!(
        expanded.matches("identity_jacobian_x(x)").count(),
        2,
        "both nested surface sites must be rewritten:\n{expanded}"
    );
}

#[test]
fn lexical_imports_shadow_the_surface_only_in_their_reaching_scope() {
    let source = r"
package P
  function ordinary
    input Real x;
    output Real y;
  algorithm
    y := x;
  end ordinary;
end P;

model UsesImport
  import jacobian = P.ordinary;
  Real y = jacobian(1.0);
end UsesImport;
";
    assert_eq!(
        expand_source(source, "Imported.mo").expect("an imported name is not stolen"),
        source
    );
}

#[test]
fn unresolved_extends_lookup_refuses_before_stealing_an_inherited_surface_name() {
    let refusal = refuse(
        r"
model Base
  function jacobian
    input Real x;
    output Real y;
  algorithm
    y := x;
  end jacobian;
end Base;

model UsesInherited
  extends Base;
  Real y = jacobian(1.0);
end UsesInherited;
",
    );
    assert_eq!(refusal.rule, Rule::CalleeLookup);
    assert!(refusal.detail.contains("extends clause"), "{refusal}");
}

#[test]
fn an_extending_scope_without_a_surface_call_remains_unchanged() {
    let source = r"
model Base
end Base;

model NoSurface
  extends Base;
  Real y = 1.0;
end NoSurface;
";
    assert_eq!(
        expand_source(source, "Inherited.mo").expect("no ambiguous surface lookup occurs"),
        source
    );
}

#[test]
fn an_exact_local_binding_wins_even_when_the_same_scope_extends() {
    let source = r"
model Base
end Base;

model UsesLocal
  extends Base;
  function jacobian
    input Real x;
    output Real y;
  algorithm
    y := x;
  end jacobian;
  Real y = jacobian(1.0);
end UsesLocal;
";
    assert_eq!(
        expand_source(source, "Local.mo").expect("the exact local binding resolves the spelling"),
        source
    );
}

#[test]
fn malformed_shape_and_binding_aggregate_refuse_before_synthesis() {
    let mut definition =
        rumoca_phase_parse::parse_to_ast(SCALED, "Malformed.mo").expect("fixture parses");
    let x = definition
        .classes
        .get_mut("fscale")
        .expect("fixture owns fscale")
        .components
        .get_mut("x")
        .expect("fixture owns x");
    x.shape_expr = vec![rumoca_ir_ast::Subscript::Empty];
    let refusal = crate::plan(&definition, "Malformed.mo")
        .expect_err("a recovery declaration shape cannot become a deferred dimension");
    assert_eq!(refusal.rule, Rule::ExpressionForm);

    let mut definition =
        rumoca_phase_parse::parse_to_ast(SCALED, "Malformed.mo").expect("fixture parses");
    let x = definition
        .classes
        .get_mut("fscale")
        .expect("fixture owns fscale")
        .components
        .get_mut("x")
        .expect("fixture owns x");
    x.shape_expr = vec![rumoca_ir_ast::Subscript::Expression(
        rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::End,
            token: rumoca_core::Token::default(),
            span: x.location.span(),
        },
    )];
    let refusal = crate::plan(&definition, "Malformed.mo")
        .expect_err("a declaration shape has no indexed array bound for `end` to denote");
    assert_eq!(refusal.rule, Rule::ExpressionForm);

    let mut definition =
        rumoca_phase_parse::parse_to_ast(SCALED, "Malformed.mo").expect("fixture parses");
    let k = definition
        .classes
        .get_mut("Probe")
        .expect("fixture owns Probe")
        .components
        .get_mut("k")
        .expect("fixture owns k");
    assert!(k.binding.is_some());
    k.has_explicit_binding = false;
    let refusal = crate::plan(&definition, "Malformed.mo")
        .expect_err("binding marker and payload must be one exact aggregate");
    assert_eq!(refusal.rule, Rule::ExpressionForm);
}

#[test]
fn nested_surface_calls_refuse_instead_of_leaving_an_overlapped_rewrite() {
    let refusal = refuse(
        r"
function identity
  input Real x;
  output Real y;
algorithm
  y := x;
end identity;

model Refused
  parameter Real x = 1.0;
  Real J[1, 1] = jacobian(identity(jacobian(identity(x), x)), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
    assert!(refusal.detail.contains("nested rewrites"), "{refusal}");
}

#[test]
fn a_standalone_surface_call_refuses_instead_of_being_skipped() {
    let refusal = refuse(
        r"
function identity
  input Real x;
  output Real y;
algorithm
  y := x;
end identity;

model Refused
  parameter Real x = 1.0;
algorithm
  jacobian(identity(x), x);
end Refused;
",
    );
    assert_eq!(refusal.rule, Rule::CallForm);
    assert!(refusal.detail.contains("call statement"), "{refusal}");
}
