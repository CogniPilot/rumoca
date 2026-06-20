use super::*;
use rumoca_phase_parse::parse_to_ast;

fn resolve_test_source(source: &str) -> Result<ResolvedTree, Diagnostics> {
    resolve_with_options(parsed_tree_from_source(source), ResolveOptions::default())
}

fn parsed_tree_from_source(source: &str) -> ParsedTree {
    let ast = parse_to_ast(source, "test.mo").expect("parse should succeed");
    let mut tree = ClassTree::from_parsed(ast);
    tree.source_map.add("test.mo", source);
    ParsedTree::new(tree)
}

fn resolve_parsed_tree_source(source: &str) -> Result<ResolvedTree, Diagnostics> {
    resolve(parsed_tree_from_source(source))
}

fn resolve_tree_source(source: &str) -> ResolvedTree {
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");
    match result {
        Ok(tree) => tree,
        Err(_) => unreachable!("resolution result was checked above"),
    }
}

fn find_comp_ref_def_id(expr: &rumoca_ir_ast::Expression) -> Option<DefId> {
    match expr {
        ast::Expression::ComponentReference(cr) => cr.def_id,
        ast::Expression::Binary { lhs, rhs, .. } => {
            find_comp_ref_def_id(lhs).or_else(|| find_comp_ref_def_id(rhs))
        }
        ast::Expression::Unary { rhs, .. } => find_comp_ref_def_id(rhs),
        ast::Expression::Range {
            start, step, end, ..
        } => find_comp_ref_def_id(start)
            .or_else(|| step.as_ref().and_then(|s| find_comp_ref_def_id(s)))
            .or_else(|| find_comp_ref_def_id(end)),
        ast::Expression::FunctionCall { comp, args, .. } => comp
            .def_id
            .or_else(|| args.iter().find_map(find_comp_ref_def_id)),
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => target
            .def_id
            .or_else(|| modifications.iter().find_map(find_comp_ref_def_id)),
        ast::Expression::NamedArgument { value, .. } => find_comp_ref_def_id(value),
        ast::Expression::Modification { target, value, .. } => {
            target.def_id.or_else(|| find_comp_ref_def_id(value))
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            elements.iter().find_map(find_comp_ref_def_id)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .find_map(|(cond, value)| {
                find_comp_ref_def_id(cond).or_else(|| find_comp_ref_def_id(value))
            })
            .or_else(|| find_comp_ref_def_id(else_branch)),
        ast::Expression::Parenthesized { inner, .. } => find_comp_ref_def_id(inner),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => find_comp_ref_def_id(expr)
            .or_else(|| {
                indices
                    .iter()
                    .find_map(|idx| find_comp_ref_def_id(&idx.range))
            })
            .or_else(|| filter.as_ref().and_then(|f| find_comp_ref_def_id(f))),
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => find_comp_ref_def_id(base).or_else(|| {
            subscripts.iter().find_map(|sub| match sub {
                rumoca_ir_ast::Subscript::Expression(expr) => find_comp_ref_def_id(expr),
                rumoca_ir_ast::Subscript::Range { .. } => None,
                rumoca_ir_ast::Subscript::Empty => None,
            })
        }),
        ast::Expression::FieldAccess { base, .. } => find_comp_ref_def_id(base),
        ast::Expression::Empty { .. } | ast::Expression::Terminal { .. } => None,
    }
}

#[test]
fn test_empty_resolution() {
    let tree = ClassTree::new();
    let parsed = ParsedTree::new(tree);
    let result = resolve(parsed);
    assert!(result.is_ok());
}

#[test]
fn test_component_reference_resolution() {
    let source = r#"
model Test
Real x;
Real y;
equation
y = x + 1;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");

    // Components should have DefIds
    assert!(model.components.get("x").unwrap().def_id.is_some());
    assert!(model.components.get("y").unwrap().def_id.is_some());

    // Model should have a scope
    assert!(model.scope_id.is_some());
}

#[test]
fn test_simple_inherited_type_name_resolves_before_global_short_name_fallback() {
    let source = r#"
package Other
  model Temperature
  end Temperature;
end Other;

package Base
  type Temperature = Real;
end Base;

package Derived
  extends Base;

  record State
Temperature T;
  end State;
end Derived;
"#;
    let tree = resolve_test_source(source).expect("resolution should succeed");
    let state = tree
        .definitions
        .classes
        .get("Derived")
        .and_then(|derived| derived.classes.get("State"))
        .expect("Derived.State should exist");
    let temp = state
        .components
        .get("T")
        .expect("State.T should exist")
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id));

    assert_eq!(
        temp.map(String::as_str),
        Some("Base.Temperature"),
        "record field type must resolve through the enclosing package's inherited members, \
         not by global short-name fallback"
    );
}

#[test]
fn test_partial_member_under_replaceable_package_is_not_rejected_in_resolve() {
    let source = r#"
package PartialMedium
  replaceable partial model BaseProperties
Real p;
  end BaseProperties;
end PartialMedium;

model UsesReplaceableMedium
  replaceable package Medium = PartialMedium;
  Medium.BaseProperties medium;
end UsesReplaceableMedium;
"#;
    resolve_test_source(source).expect("resolve must defer replaceable package member partiality");
}

#[test]
fn test_short_package_alias_member_lookup_resolves_inherited_member() {
    let source = r#"
package PhaseSystems
  package ThreePhase_dq0
function j
  input Real x;
  output Real y;
algorithm
  y := x;
end j;
  end ThreePhase_dq0;
end PhaseSystems;

package AC3ph
  package Ports
model PortBase
  package PS = PhaseSystems.ThreePhase_dq0;
  function j = PS.j;
  Real y;
equation
  y = j(1.0);
end PortBase;
  end Ports;
end AC3ph;
"#;

    resolve_test_source(source)
        .expect("short package alias member access like `PS.j` should resolve");
}

#[test]
fn test_cardinality_allows_indexed_connector_array_element() {
    let source = r#"
connector Port
  Real p;
end Port;

model UsesIndexedCardinality
  Port ports[2];
equation
  if cardinality(ports[1]) == 0 then
ports[1].p = 0;
  end if;
end UsesIndexedCardinality;
"#;
    resolve_test_source(source).expect("indexed connector array element is scalar");
}

#[test]
fn test_cardinality_rejects_unindexed_connector_array() {
    let source = r#"
connector Port
  Real p;
end Port;

model UsesArrayCardinality
  Port ports[2];
equation
  if cardinality(ports) == 0 then
ports[1].p = 0;
  end if;
end UsesArrayCardinality;
"#;
    let diags = resolve_test_source(source).expect_err("connector array target must fail");
    assert!(
        diags
            .iter()
            .any(|d| d.code.as_deref() == Some("ER057")
                && d.message.contains("connector array 'ports'")),
        "expected cardinality connector-array diagnostic, got: {diags:?}"
    );
}

#[test]
fn test_loop_index_named_like_class_does_not_trigger_class_used_as_value() {
    let source = r#"
package P
  model j
  end j;
end P;

model UsesLoopIndexJ
  Integer y;
equation
  for j in 1:2 loop
y = j;
  end for;
end UsesLoopIndexJ;
"#;
    resolve_test_source(source)
        .expect("loop index `j` must resolve as a value, not as global class `j`");
}

#[test]
fn test_evaluate_on_non_parameter_component_is_error_by_default() {
    let source = r#"
model EvaluateScopeWarning
  Real x annotation(Evaluate=true);
equation
  x = 1;
end EvaluateScopeWarning;
"#;
    let diagnostics = resolve_test_source(source)
        .expect_err("Evaluate annotation scope should fail by default in strict mode");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ER070")),
        "expected ER070 for invalid Evaluate annotation, got: {diagnostics:?}"
    );
}

#[test]
fn test_evaluate_on_function_local_component_is_allowed() {
    let source = r#"
function F
  input Real x[:];
  output Real y;
protected
  Integer m=size(x, 1) annotation(Evaluate=true);
algorithm
  y := m;
end F;
"#;
    resolve_test_source(source)
        .expect("Evaluate annotation on function local component should be accepted");
}

#[test]
fn test_when_single_assign_allows_distinct_indexed_targets() {
    let source = r#"
model IndexedWhenTargets
  Boolean open1;
  Boolean open2;
  Real t0[2];
equation
  when edge(open1) then
t0[1] = time;
  end when;
  when edge(open2) then
t0[2] = time;
  end when;
end IndexedWhenTargets;
"#;
    resolve_test_source(source)
        .expect("distinct indexed targets in separate when-equations should be allowed");
}

#[test]
fn test_when_single_assign_rejects_same_target_across_when_equations() {
    let source = r#"
model DuplicateWhenTarget
  Boolean open1;
  Boolean open2;
  Real t0;
equation
  when edge(open1) then
t0 = time;
  end when;
  when edge(open2) then
t0 = time;
  end when;
end DuplicateWhenTarget;
"#;
    let result = resolve_test_source(source);
    assert!(result.is_err(), "duplicate when target should fail");
    let diagnostics = result.expect_err("expected diagnostics");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ER053")),
        "expected ER053 for duplicate when target, got: {diagnostics:?}"
    );
}

#[test]
fn test_unresolved_component_reference_is_error() {
    let source = r#"
model Test
Real y;
equation
y = x + 1;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.message.contains("unresolved component reference") && d.code.as_deref() == Some("ER002")
    }));
}

#[test]
fn test_def_id_zero_is_reserved_for_root_not_builtin() {
    let resolver = Resolver::new();
    let real_id = resolver
        .scope_tree
        .lookup(ScopeId::GLOBAL, &ComponentPath::from_flat_path("Real"))
        .expect("Real builtin should be registered globally");

    assert_ne!(
        real_id,
        DefId::new(0),
        "SPEC_0001 reserves DefId(0) for root/global scope"
    );
    assert!(
        resolver.is_builtin(real_id),
        "Real should remain classified as a builtin"
    );
    assert!(
        !resolver.is_builtin(DefId::new(0)),
        "root/global DefId must not be classified as a builtin"
    );
}

#[test]
fn test_nested_non_encapsulated_class_sees_enclosing_name() {
    let source = r#"
package P
constant Real c = 1;
model M
    Real x = c;
end M;
end P;
"#;

    resolve_test_source(source).expect("ordinary nested lookup should resolve enclosing c");
}

#[test]
fn test_encapsulated_class_cannot_see_enclosing_name() {
    let source = r#"
package P
constant Real c = 1;
encapsulated model M
    Real x = c;
end M;
end P;
"#;

    let diagnostics = resolve_test_source(source).expect_err("encapsulated M must not resolve P.c");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message.contains("unresolved component reference: 'c'")
                && diag.code.as_deref() == Some("ER002")
        }),
        "expected unresolved component diagnostic for c, got: {diagnostics:?}"
    );
    assert!(
        !diagnostics
            .iter()
            .any(|diag| { diag.message.contains("unresolved type reference: 'Real'") }),
        "predefined Real type should remain visible from encapsulated scope"
    );
}

#[test]
fn test_encapsulated_class_resolves_predefined_type() {
    let source = r#"
package P
encapsulated model M
    Real x = 1;
end M;
end P;
"#;

    resolve_test_source(source).expect("encapsulated scope should resolve predefined Real");
}

#[test]
fn test_state_machine_operator_reports_explicit_unsupported_diagnostic() {
    let source = r#"
model UnsupportedStateMachine
Real a;
equation
transition(a, a, true);
end UnsupportedStateMachine;
"#;

    let diagnostics =
        resolve_test_source(source).expect_err("state-machine operators are unsupported");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.code.as_deref() == Some("ER073")
                && diag
                    .message
                    .contains("transition() requires Modelica state-machine")
        }),
        "expected explicit state-machine unsupported diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn test_qualified_initial_state_function_is_not_state_machine_operator() {
    let source = r#"
package P
function initialState
    output Real y;
algorithm
    y := 1;
end initialState;

model M
    Real x = P.initialState();
end M;
end P;
"#;

    let resolved = resolve_test_source(source).expect("qualified function should resolve");
    let model = resolved
        .inner()
        .definitions
        .classes
        .get("P")
        .and_then(|package| package.classes.get("M"))
        .expect("P.M should exist");
    let binding = model
        .components
        .get("x")
        .and_then(|component| component.binding.as_ref())
        .expect("x should have a binding");
    let ast::Expression::FunctionCall { comp, .. } = binding else {
        panic!("x binding should remain a function call");
    };
    assert_eq!(comp.to_string(), "P.initialState");
}

#[test]
fn test_unresolved_import_is_emitted_before_unresolved_type_reference() {
    let source = r#"
model Ball
import Modelica.Blocks.Continuous.PID;
PID pid();
end Ball;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let messages: Vec<_> = diags.iter().map(|d| d.message.as_str()).collect();

    let import_pos = messages
        .iter()
        .position(|msg| msg.contains("unresolved import") && msg.contains("PID"));
    let type_pos = messages
        .iter()
        .position(|msg| msg.contains("unresolved type reference") && msg.contains("PID"));

    assert!(
        import_pos.is_some(),
        "expected unresolved import diagnostic, got: {messages:?}"
    );
    assert!(
        type_pos.is_some(),
        "expected unresolved type reference diagnostic, got: {messages:?}"
    );
    assert!(
        import_pos.expect("import diagnostic index")
            < type_pos.expect("unresolved type diagnostic index"),
        "expected import diagnostic before unresolved type reference, got: {messages:?}"
    );
}

#[test]
fn test_unresolved_diagnostics_include_source_labels() {
    let source = r#"
model Ball
import Modelica.Blocks.Continuous.PID;
PID pid();
equation
der(x) = x;
end Ball;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let import = diags
        .iter()
        .find(|d| d.message.contains("unresolved import"))
        .expect("missing unresolved import diagnostic");
    let unresolved_type = diags
        .iter()
        .find(|d| d.message.contains("unresolved type reference"))
        .expect("missing unresolved type reference diagnostic");

    assert!(
        !import.labels.is_empty(),
        "unresolved import should include a source label"
    );
    assert!(
        !unresolved_type.labels.is_empty(),
        "unresolved type reference should include a source label"
    );
}

#[test]
fn test_unresolved_selective_import_member_is_error() {
    let source = r#"
package P
  model A
  end A;
end P;

model M
  import P.{A, B};
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    let import = diags
        .iter()
        .find(|d| d.message.contains("unresolved import member") && d.message.contains("B"))
        .expect("missing unresolved selective import member diagnostic");

    assert_eq!(import.code.as_deref(), Some("ER002"));
    assert!(
        !import.labels.is_empty(),
        "unresolved selective import member should include source label"
    );
}

#[test]
fn test_import_from_non_package_is_rejected() {
    let source = r#"
model Outer
  model Inner
  end Inner;
end Outer;

model Test
  import Outer.Inner;
  Inner x;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("invalid import target")
            && d.message.contains("Outer.Inner")
    }));
}

#[test]
fn test_single_segment_class_import_is_allowed() {
    let source = r#"
operator record Complex
  encapsulated operator function '0'
import Complex;
output Complex result;
  algorithm
result := Complex(0);
  end '0';
end Complex;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "single-segment class import must be allowed for operator records"
    );
}

#[test]
fn test_import_cannot_traverse_non_package_member() {
    let source = r#"
package P
  model A
constant Real x = 1;
  end A;
end P;

model Test
  import P.A.x;
  Real y;
equation
  y = x;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("invalid import target")
            && d.message.contains("P.A.x")
    }));
}

#[test]
fn test_non_replaceable_partial_type_path_is_unresolved() {
    let source = r#"
model M
  package P
  end P;
  P.Missing x;
equation
  x = 0;
end M;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_err(),
        "resolution should fail for non-replaceable partial type path"
    );

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER002")
            && d.message.contains("unresolved type reference")
            && d.message.contains("P.Missing")
    }));
}

#[test]
fn test_partial_model_can_declare_replaceable_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

partial block PartialLogical
  replaceable PartialBooleanMISO combinator constrainedby PartialBooleanMISO;
end PartialLogical;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "partial classes may contain replaceable components constrained by partial classes"
    );
}

#[test]
fn test_concrete_model_can_declare_replaceable_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

block Concrete
  replaceable PartialBooleanMISO combinator constrainedby PartialBooleanMISO;
end Concrete;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(
        result.is_ok(),
        "replaceable partial-typed components must remain legal until instantiation"
    );
}

#[test]
fn test_concrete_model_cannot_instantiate_partial_component() {
    let source = r#"
partial block PartialBooleanMISO
  input Boolean u;
  output Boolean y;
end PartialBooleanMISO;

block Concrete
  PartialBooleanMISO combinator;
end Concrete;
"#;
    let result = resolve_test_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(diags.iter().any(|d| {
        d.code.as_deref() == Some("ER005")
            && d.message
                .contains("component 'combinator' instantiates partial block")
    }));
}

#[test]
fn test_unresolved_function_call_is_error() {
    let source = r#"
model Test
Real y;
equation
y = unknownFunc(1.0);
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_err(), "resolution should fail");

    let diags = result.expect_err("expected resolve diagnostics");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("unresolved function call")
                && d.code.as_deref() == Some("ER002"))
    );
}

#[test]
fn test_unresolved_function_call_can_be_lenient() {
    let source = r#"
model Test
Real y;
equation
y = unknownFunc(1.0);
end Test;
"#;
    let parsed = parsed_tree_from_source(source);
    let options = ResolveOptions {
        unresolved_component_refs_are_errors: false,
        unresolved_function_calls_are_errors: false,
        ..ResolveOptions::default()
    };
    let result = resolve_with_options(parsed, options);
    assert!(
        result.is_ok(),
        "lenient mode should not fail unresolved function calls"
    );
}

#[test]
fn test_function_call_preserves_source_scope_and_resolves_target_def_id() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function f
  input Real u;
  output Real y;
algorithm
  y := u;
end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
input Real u;
output Real y;
  algorithm
y := u + 1;
  end f;
end TableBased;

model UsesMediumAlias
  package Medium = TableBased;
  Real y;
equation
  y = Medium.f(1.0);
end UsesMediumAlias;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesMediumAlias")
        .expect("UsesMediumAlias should exist");
    let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
        panic!("expected simple equation");
    };
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
        panic!("expected function call on rhs");
    };
    let def_id = comp.def_id.expect("function call should have def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(
        resolved, "TableBased.f",
        "function call should resolve to canonical qualified function"
    );
    assert_eq!(
        comp.to_string(),
        "Medium.f",
        "function call path should preserve source component-reference scope"
    );
}

#[test]
fn test_redeclare_package_modifier_resolves_rhs_in_modifier_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model B
  replaceable package Medium = Interfaces.PartialMedium;
end B;

model C
  package Medium = TableBased;
  B b(redeclare package Medium = Medium);
end C;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree.definitions.classes.get("C").expect("C should exist");
    let component = model.components.get("b").expect("b should exist");
    let modification = component
        .modifications
        .get("Medium")
        .expect("Medium redeclare should be preserved");
    let rumoca_ir_ast::Expression::ClassModification { target, .. } = modification else {
        panic!("expected redeclare package value to be a class modification");
    };
    let def_id = target
        .def_id
        .expect("redeclare package RHS should resolve to enclosing Medium");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved Medium def_id should exist in def_map");

    assert_eq!(resolved, "C.Medium");
    assert_eq!(target.to_string(), "Medium");
}

#[test]
fn test_replaceable_medium_member_calls_resolve_through_forwarded_redeclare() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function setState_pTX
  input Real p;
  input Real T;
  output Real state;
algorithm
  state := p + T;
end setState_pTX;
replaceable function density
  input Real state;
  output Real d;
algorithm
  d := state;
end density;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model Boundary
  replaceable package Medium = Interfaces.PartialMedium;
  Real d;
equation
  d = Medium.density(Medium.setState_pTX(1.0, 2.0));
end Boundary;

model Network
  replaceable package Medium = TableBased constrainedby Interfaces.PartialMedium;
  Boundary source(redeclare package Medium = Medium);
end Network;
"#;
    let _ = resolve_tree_source(source);
}

#[test]
fn test_extends_redeclared_replaceable_medium_member_calls_resolve() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function setState_pTX
  input Real p;
  input Real T;
  output Real state;
algorithm
  state := p + T;
end setState_pTX;
replaceable function density
  input Real state;
  output Real d;
algorithm
  d := state;
end density;
  end PartialMedium;
  partial package PartialTwoPhaseMedium
extends PartialMedium;
replaceable function saturationPressure
  input Real T;
  output Real p;
algorithm
  p := T;
end saturationPressure;
  end PartialTwoPhaseMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialTwoPhaseMedium;
end TableBased;

partial model Base
  replaceable package Medium = Interfaces.PartialMedium;
end Base;

model Derived
  extends Base(
    redeclare replaceable package Medium = TableBased
      constrainedby Interfaces.PartialTwoPhaseMedium);
  Real p = Medium.saturationPressure(1.0);
end Derived;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    let component = model.components.get("p").expect("p should exist");
    let binding = component.binding.as_ref().expect("p should have a binding");
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = binding else {
        panic!("expected Medium.saturationPressure call");
    };
    let def_id = comp
        .def_id
        .expect("deferred medium call should be anchored to replaceable package root");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved Medium def_id should exist in def_map");
    assert_eq!(resolved, "Base.Medium");
}

#[test]
fn test_inherited_medium_alias_function_call_preserves_source_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function density_pTX
  input Real p;
  input Real T;
  output Real d;
algorithm
  d := p + T;
end density_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
end TableBased;

model Base
  package Medium = TableBased;
end Base;

model Derived
  extends Base;
  Real d;
equation
  d = Medium.density_pTX(1.0, 2.0);
end Derived;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    let rumoca_ir_ast::Equation::Simple { rhs, .. } = &model.equations[0] else {
        panic!("expected simple equation");
    };
    let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = rhs else {
        panic!("expected function call on rhs");
    };
    let def_id = comp
        .def_id
        .expect("inherited Medium call should have def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(
        resolved, "Interfaces.PartialMedium.density_pTX",
        "inherited alias function should resolve to concrete target"
    );
    assert_eq!(
        comp.to_string(),
        "Medium.density_pTX",
        "function call path should preserve source component-reference scope"
    );
}

#[test]
fn test_component_binding_function_call_preserves_source_scope() {
    let source = r#"
package Interfaces
  partial package PartialMedium
replaceable function f
  input Real u;
  output Real y;
algorithm
  y := u;
end f;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium;
  redeclare function f
input Real u;
output Real y;
  algorithm
y := u + 2;
  end f;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Real state = Medium.f(1.0);
end UsesTableBasedState;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesTableBasedState")
        .expect("UsesTableBasedState should exist");
    let state = model
        .components
        .get("state")
        .expect("state component should exist");

    let binding = state
        .binding
        .as_ref()
        .expect("state component should preserve explicit binding");
    let target = extract_call_target(binding).expect("binding should contain function call");
    let def_id = target
        .def_id
        .expect("binding function call should have def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(resolved, "TableBased.f");
    assert_eq!(target.to_string(), "Medium.f");
}

fn extract_call_target(
    expr: &rumoca_ir_ast::Expression,
) -> Option<&rumoca_ir_ast::ComponentReference> {
    match expr {
        rumoca_ir_ast::Expression::FunctionCall { comp, .. } => Some(comp),
        rumoca_ir_ast::Expression::ClassModification { target, .. } => Some(target),
        _ => None,
    }
}

#[test]
fn test_binding_call_with_redeclared_record_alias_preserves_source_scope() {
    let source = r#"
package Common
  record BaseProps_Tpoly
Real T;
Real p;
  end BaseProps_Tpoly;
end Common;

package Interfaces
  partial package PartialMedium
replaceable record ThermodynamicState
  Real x;
end ThermodynamicState;

replaceable function setState_pTX
  input Real p;
  input Real T;
  output ThermodynamicState state;
  external "C";
end setState_pTX;
  end PartialMedium;
end Interfaces;

package TableBased
  extends Interfaces.PartialMedium(
redeclare record ThermodynamicState = Common.BaseProps_Tpoly
  );

  redeclare function setState_pTX
input Real p;
input Real T;
output ThermodynamicState state;
external "C";
  end setState_pTX;
end TableBased;

model UsesTableBasedState
  package Medium = TableBased;
  Medium.ThermodynamicState state = Medium.setState_pTX(1, 2);
end UsesTableBasedState;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("UsesTableBasedState")
        .expect("UsesTableBasedState should exist");
    let state = model
        .components
        .get("state")
        .expect("state component should exist");
    let binding = state
        .binding
        .as_ref()
        .expect("state component should preserve explicit binding");
    let target = extract_call_target(binding).expect("binding should contain function call");
    let def_id = target
        .def_id
        .expect("binding function call should have def_id");
    let resolved = tree
        .def_map
        .get(&def_id)
        .expect("resolved function def_id should exist in def_map");
    assert_eq!(resolved, "TableBased.setState_pTX");
    assert_eq!(target.to_string(), "Medium.setState_pTX");
}

#[test]
fn test_for_loop_scope() {
    let source = r#"
model Test
Real x[3];
equation
for i in 1:3 loop
    x[i] = i;
end for;
end Test;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");
}

#[test]
fn test_for_equation_range_resolves() {
    let source = r#"
model Test
parameter Integer n = 3;
Real x[n];
equation
for i in 1:n loop
    x[i] = i;
end for;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let rumoca_ir_ast::Equation::For { indices, .. } = &model.equations[0] else {
        panic!("expected for-equation");
    };
    let range_expr = &indices[0].range;
    assert!(
        find_comp_ref_def_id(range_expr).is_some(),
        "range expression should resolve component references"
    );
}

#[test]
fn test_for_statement_range_resolves() {
    let source = r#"
model Test
parameter Integer n = 3;
Integer x;
algorithm
for i in 1:n loop
    x := i;
end for;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let stmt = model.algorithms[0].first().expect("for statement");
    let rumoca_ir_ast::Statement::For { indices, .. } = stmt else {
        panic!("expected for-statement");
    };
    let range_expr = &indices[0].range;
    assert!(
        find_comp_ref_def_id(range_expr).is_some(),
        "range expression should resolve component references"
    );
}

#[test]
fn test_while_condition_resolves() {
    let source = r#"
model Test
Integer n = 3;
algorithm
while n > 0 loop
    n := n - 1;
end while;
end Test;
"#;
    let tree = resolve_tree_source(source).into_inner();
    let model = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test should exist");
    let stmt = model.algorithms[0].first().expect("while statement");
    let rumoca_ir_ast::Statement::While(block) = stmt else {
        panic!("expected while-statement");
    };
    assert!(
        find_comp_ref_def_id(&block.cond).is_some(),
        "while condition should resolve component references"
    );
}

#[test]
fn test_nested_class_resolution() {
    let source = r#"
package TestPkg
model Inner
    Real x;
end Inner;
end TestPkg;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let pkg = tree
        .definitions
        .classes
        .get("TestPkg")
        .expect("TestPkg should exist");
    assert!(pkg.def_id.is_some());

    let inner = pkg.classes.get("Inner").expect("Inner should exist");
    assert!(inner.def_id.is_some());
}

// =========================================================================
// Extends resolution tests (MLS §7)
// =========================================================================

#[test]
fn test_simple_extends_resolution() {
    // Test that a simple extends clause resolves correctly
    let source = r#"
model Base
Real x;
end Base;

model Derived
extends Base;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();

    // Verify base class exists and has a DefId
    let base = tree
        .definitions
        .classes
        .get("Base")
        .expect("Base should exist");
    assert!(base.def_id.is_some(), "Base should have DefId");

    // Verify derived class exists and extends has base_def_id set
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 1, "Derived should have one extends");

    let extend = &derived.extends[0];
    assert!(
        extend.base_def_id.is_some(),
        "Extends should have base_def_id set"
    );
    assert_eq!(
        extend.base_def_id, base.def_id,
        "base_def_id should match Base's DefId"
    );
}

#[test]
fn test_qualified_extends_resolution() {
    // Test that qualified extends (Package.Model) resolves correctly
    let source = r#"
package MyPkg
model Base
    Real x;
end Base;
end MyPkg;

model Derived
extends MyPkg.Base;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();

    // Get the base class's DefId
    let pkg = tree
        .definitions
        .classes
        .get("MyPkg")
        .expect("MyPkg should exist");
    let base = pkg.classes.get("Base").expect("Base should exist in MyPkg");
    assert!(base.def_id.is_some(), "Base should have DefId");

    // Verify derived class extends has correct base_def_id
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 1);

    let extend = &derived.extends[0];
    assert!(
        extend.base_def_id.is_some(),
        "Extends should have base_def_id set"
    );
    assert_eq!(
        extend.base_def_id, base.def_id,
        "base_def_id should match MyPkg.Base's DefId"
    );
}

#[test]
fn test_base_class_not_found() {
    // Test that extending a non-existent class produces an error
    let source = r#"
model Derived
extends NonExistent;
Real y;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with base class not found error
    assert!(result.is_err(), "resolution should fail");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "base class not found"
    let has_base_not_found = diagnostics
        .iter()
        .any(|d| d.message.contains("base class not found"));
    assert!(has_base_not_found, "should have base class not found error");
}

#[test]
fn test_circular_inheritance_direct() {
    // Test that direct self-reference (A extends A) is detected.
    // This produces "base class not found" because when we exclude the
    // current class from lookup (to support redeclare extends pattern),
    // we can't find any other class with that name.
    let source = r#"
model A
extends A;
Real x;
end A;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with "base class not found" error
    assert!(result.is_err(), "resolution should fail");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message indicates base not found
    let has_base_not_found = diagnostics
        .iter()
        .any(|d| d.message.contains("base class not found"));
    assert!(has_base_not_found, "should have base class not found error");
}

#[test]
fn test_multiple_extends() {
    // Test that multiple extends clauses all resolve correctly
    let source = r#"
model Base1
Real x;
end Base1;

model Base2
Real y;
end Base2;

model Derived
extends Base1;
extends Base2;
Real z;
end Derived;
"#;
    let result = resolve_parsed_tree_source(source);
    assert!(result.is_ok(), "resolution should succeed");

    let tree = result.unwrap().into_inner();
    let derived = tree
        .definitions
        .classes
        .get("Derived")
        .expect("Derived should exist");
    assert_eq!(derived.extends.len(), 2, "Derived should have two extends");

    // Both extends should have base_def_id set
    for extend in &derived.extends {
        assert!(
            extend.base_def_id.is_some(),
            "All extends should have base_def_id set"
        );
    }
}

#[test]
fn test_circular_inheritance_indirect() {
    // Test that indirect circular inheritance (A extends B, B extends A) is detected
    let source = r#"
model A
extends B;
Real x;
end A;

model B
extends A;
Real y;
end B;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with circular inheritance error
    assert!(result.is_err(), "resolution should fail for indirect cycle");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "circular"
    let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
    assert!(
        has_circular,
        "should have circular inheritance error for indirect cycle"
    );
}

#[test]
fn test_circular_inheritance_chain() {
    // Test that longer cycles (A extends B, B extends C, C extends A) are detected
    let source = r#"
model A
extends B;
end A;

model B
extends C;
end B;

model C
extends A;
end C;
"#;
    let result = resolve_parsed_tree_source(source);

    // Resolution should fail with circular inheritance error
    assert!(result.is_err(), "resolution should fail for chain cycle");
    let diagnostics = result.unwrap_err();
    assert!(diagnostics.has_errors(), "should have error diagnostics");

    // Check that the error message contains "circular"
    let has_circular = diagnostics.iter().any(|d| d.message.contains("circular"));
    assert!(
        has_circular,
        "should have circular inheritance error for chain cycle"
    );
}
