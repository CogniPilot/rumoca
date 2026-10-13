//! Solve lowering records, from each visible scalar's defining equation, the
//! phasor an angle or power-factor scalar is a function of (SPEC_0040
//! SOLVE-C65), and model wire replay refuses a phasor that names no two
//! distinct visible Real components.

use std::collections::{BTreeMap, HashMap};

use rumoca::Compiler;
use rumoca_ir_solve::{SolveModel, SolvePhasor, SolvePhasorFunction};

fn lowered(source: &str, model: &str) -> SolveModel {
    let dae = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

fn phasors(model: &SolveModel) -> BTreeMap<String, SolvePhasor> {
    model
        .visible_metadata()
        .iter()
        .filter_map(|meta| Some((meta.name.clone(), meta.phasor.clone()?)))
        .collect()
}

fn phasor(function: SolvePhasorFunction, re: &str, im: &str) -> SolvePhasor {
    SolvePhasor {
        function,
        re: re.to_string(),
        im: im.to_string(),
    }
}

/// The MSL forms: `Modelica.Math.atan3` (a guarded body, not inlined),
/// `ComplexMath.arg` over it with its default `phi0 = 0`, the
/// `ComplexToPolar` conditional on a parameter, and `cos(arg(...))` power
/// factors, over scalar and record (`Complex`) arguments; plus the forms
/// that must not be recorded.
const PHASORS: &str = r#"
model Phasors
  function atan3
    input Real u1;
    input Real u2;
    input Real y0 = 0;
    output Real y;
  protected
    Real w;
  algorithm
    w := atan2(u1, u2);
    if y0 == 0 then
      y := w;
    else
      y := w + 2*3.141592653589793*integer((3.141592653589793 + y0 - w)/(2*3.141592653589793));
    end if;
  end atan3;
  function arg
    input Real re;
    input Real im;
    input Real phi0 = 0;
    output Real phi;
  algorithm
    phi := atan3(im, re, phi0);
    annotation(Inline = true);
  end arg;
  record Cplx
    Real re;
    Real im;
  end Cplx;
  function argOf
    input Cplx c;
    input Real phi0 = 0;
    output Real phi;
  algorithm
    phi := atan3(c.im, c.re, phi0);
    annotation(Inline = true);
  end argOf;
  parameter Boolean conjugate = false;
  parameter Real reference = 1;
  Real re(start = 1), im(start = 0);
  Real P, Q;
  Real angle, viaAtan3, viaArg, polar, pf;
  Cplx z;
  Real viaRecord, pfRecord;
  Real shifted, referenced, sum;
equation
  der(re) = -im;
  der(im) = re;
  P = 2*re;
  Q = 3*im;
  angle = atan2(im, re);
  viaAtan3 = atan3(im, re, 0);
  viaArg = arg(re, im);
  polar = if conjugate then atan2(-im, re) else atan2(im, re);
  pf = cos(arg(P, Q));
  z = Cplx(re, im);
  viaRecord = argOf(z);
  pfRecord = cos(argOf(Cplx(P, Q)));
  shifted = atan2(im, re) + 1;
  referenced = atan3(im, re, reference);
  sum = atan2(im + P, re);
end Phasors;
"#;

#[test]
fn angles_and_power_factors_record_the_phasor_their_equation_reads() {
    let angle = phasor(SolvePhasorFunction::Angle, "re", "im");
    let power_factor = phasor(SolvePhasorFunction::CosineOfAngle, "P", "Q");
    assert_eq!(
        phasors(&lowered(PHASORS, "Phasors")),
        BTreeMap::from([
            ("angle".to_string(), angle.clone()),
            ("viaAtan3".to_string(), angle.clone()),
            ("viaArg".to_string(), angle.clone()),
            ("polar".to_string(), angle),
            (
                "viaRecord".to_string(),
                phasor(SolvePhasorFunction::Angle, "z.re", "z.im")
            ),
            ("pf".to_string(), power_factor.clone()),
            ("pfRecord".to_string(), power_factor),
        ])
    );
}

fn replay(wire: &serde_json::Value) -> Result<SolveModel, String> {
    let bytes = serde_json::to_vec(wire).unwrap();
    rumoca_eval_solve::model_wire::deserialize_solve_model(
        &mut serde_json::Deserializer::from_slice(&bytes),
    )
    .map_err(|error| error.to_string())
}

#[test]
fn model_wire_replay_refuses_a_phasor_without_two_distinct_visible_components() {
    let model = lowered(PHASORS, "Phasors");
    let wire =
        serde_json::to_value(rumoca_eval_solve::model_wire::solve_model_wire(&model).unwrap())
            .unwrap();
    assert_eq!(
        phasors(&replay(&wire).expect("the recorded phasors replay")).len(),
        7
    );
    let index = wire["visible_projection"]
        .as_array()
        .unwrap()
        .iter()
        .position(|meta| meta["binding"] == "angle")
        .unwrap();
    for (component, value) in [
        ("re", "absent"),
        ("re", "im"),
        ("im", "angle"),
        ("re", "conjugate"),
    ] {
        let mut refused = wire.clone();
        refused["visible_projection"][index]["phasors"][0][1][component] = value.into();
        let error = replay(&refused).expect_err("an invalid phasor is refused");
        assert!(error.contains("phasor"), "{error}");
    }
}

#[test]
fn checked_scoped_results_preserve_original_angle_proofs_without_near_match_admission() {
    let source = r#"
model ScopedAngles
  function scoped
    input Real im;
    input Real re;
    input Boolean pick;
    output Real y;
  algorithm
    if pick then y := atan2(im, re); else y := atan2(im, re); end if;
  end scoped;
  function nearMatch
    input Real im;
    input Real re;
    input Boolean pick;
    output Real y;
  algorithm
    if pick then y := atan2(im, re); else y := atan2(im, re) + 1; end if;
  end nearMatch;
  parameter Boolean pick = false;
  Real re(start=1), im(start=0);
  Real expressionAngle, scopedAngle, rejected;
equation
  der(re)=-im;
  der(im)=re;
  expressionAngle=if pick then atan2(im,re) else atan2(im,re);
  scopedAngle=scoped(im,re,pick);
  rejected=nearMatch(im,re,pick);
end ScopedAngles;
"#;
    let compiled = Compiler::new()
        .model("ScopedAngles")
        .compile_str(source, "ScopedAngles.mo")
        .unwrap();
    let conditional_outputs = compiled.dae.inspect(|view| {
        (0..view.expression_count())
            .filter(|&index| {
                view.expression_id(index)
                    .and_then(|id| view.expression(id))
                    .is_some_and(|node| {
                        matches!(
                            node.operation(),
                            rumoca_ir_dae::ExpressionOperation::Conditional(_)
                                | rumoca_ir_dae::ExpressionOperation::FunctionConditionalOutput { .. }
                        )
                    })
            })
            .count()
    });
    assert!(
        conditional_outputs >= 3,
        "the equation conditional and both function conditionals must stay checked conditionals"
    );
    let model = rumoca_phase_solve::lower_solve_model(&compiled.dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone();
    assert_eq!(
        phasors(&model),
        BTreeMap::from([
            (
                "expressionAngle".into(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
            (
                "scopedAngle".into(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
        ])
    );
}

#[test]
fn original_msl_atan3_selected_result_keeps_its_parent_components() {
    // Modelica 4.1.0 Math.atan3 algorithm body, verbatim; declarations retain pi2.
    let source = r#"
model OriginalAtan3
  function atan3
    input Real u1;
    input Real u2;
    input Real y0=0;
    output Real y;
  protected
    constant Real pi=3.141592653589793;
    constant Real pi2=2*pi;
    Real w;
  algorithm
  w := Math.atan2(u1, u2);
  if y0 == 0 then
    // For the default (y0 = 0), exactly the same result as with atan2(..) is returned
    y := w;
  else
    /* -pi < y - y0 <= pi
       -pi < w + 2*pi*N - y0 <= pi
       (-pi+y0-w)/(2*pi) < N <= (pi+y0-w)/(2*pi)
       -> N := integer( (pi+y0-w)/(2*pi) )
    */
    y := w + pi2*integer((pi+y0-w)/pi2);
  end if;
  end atan3;
  package Math
    function atan2
      input Real u1;
      input Real u2;
      output Real y;
    algorithm
      y := .atan2(u1, u2);
      annotation(Inline=true);
    end atan2;
  end Math;
  Real re(start=1), im(start=0);
  Real selected, shifted;
equation
  der(re)=-im;
  der(im)=re;
  selected=atan3(im,re,0);
  shifted=atan3(im,re,1);
end OriginalAtan3;
"#;
    assert_eq!(
        phasors(&lowered(source, "OriginalAtan3")),
        BTreeMap::from([(
            "selected".into(),
            phasor(SolvePhasorFunction::Angle, "re", "im")
        ),])
    );
}

#[test]
fn exact_integer_guards_and_distinct_original_call_bindings_select_only_their_own_arm() {
    let source = r#"
model ExactAngleContexts
  function selectAngle
    input Real im;
    input Real re;
    input Integer selector;
    output Real y;
  algorithm
    if selector == 9007199254740992 then
      y := atan2(im,re) + 1;
    else
      y := atan2(im,re);
    end if;
  end selectAngle;
  function opposite
    input Real im;
    input Real re;
    input Integer selector;
    output Real y;
  algorithm
    if selector == 9007199254740992 then
      y := atan2(im,re);
    else
      y := atan2(im,re) + 1;
    end if;
  end opposite;
  Real re(start=1), im(start=0);
  parameter Boolean pick=false;
  Real selected, refused, oppositeSelected, oppositeRefused, differentContexts;
equation
  der(re)=-im;
  der(im)=re;
  selected=selectAngle(im,re,9007199254740993);
  refused=selectAngle(im,re,9007199254740992);
  oppositeSelected=opposite(im,re,9007199254740992);
  oppositeRefused=opposite(im,re,9007199254740993);
  differentContexts=if pick then opposite(im,re,9007199254740992) else opposite(im,re,9007199254740993);
end ExactAngleContexts;
"#;
    assert_eq!(
        phasors(&lowered(source, "ExactAngleContexts")),
        BTreeMap::from([
            (
                "selected".to_owned(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
            (
                "oppositeSelected".to_owned(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
        ])
    );
}

const REAL_BINDING_CONTEXTS: &str = r#"
model RealAngleContexts
  function byParameter
    input Real n;
    input Real im;
    input Real re;
    output Real p;
  algorithm
    if n == 9007199254740993 then p := 0; else p := atan2(im,re); end if;
  end byParameter;
  function byValue
    input Integer n;
    input Real im;
    input Real re;
    output Real p;
  protected
    Real q;
  algorithm
    q := n;
    if q == 9007199254740993 then p := 0; else p := atan2(im,re); end if;
  end byValue;
  Real re(start=1), im(start=0);
  Real direct, throughParameter, throughValue;
equation
  der(re)=-im;
  der(im)=re;
  direct=atan2(im,re);
  throughParameter=byParameter(9007199254740992,im,re);
  throughValue=byValue(9007199254740992,im,re);
end RealAngleContexts;
"#;

#[test]
fn real_function_bindings_cannot_certify_an_integer_guard_before_their_declared_conversion() {
    use rumoca_ir_dae::{CoordinateView, DaeLiteral, ExpressionOperation, ScalarType};

    let source = REAL_BINDING_CONTEXTS;
    let compiled = Compiler::new()
        .model("RealAngleContexts")
        .compile_str(source, "RealAngleContexts.mo")
        .unwrap();
    let (integer_call_arguments, real_parameters, real_integer_definitions) =
        compiled.dae.inspect(|view| {
            let mut counts = (0usize, 0usize, 0usize);
            for index in 0..view.expression_count() {
                let node = view.expression(view.expression_id(index).unwrap()).unwrap();
                match node.operation() {
                    ExpressionOperation::Call { arguments, .. }
                        if arguments
                            .get(0)
                            .and_then(|id| view.expression(id))
                            .is_some_and(|argument| {
                                matches!(
                                    argument.operation(),
                                    ExpressionOperation::Literal(DaeLiteral::Integer(
                                        9007199254740992
                                    ))
                                )
                            }) =>
                    {
                        counts.0 += 1;
                    }
                    ExpressionOperation::Coordinate(CoordinateView::FunctionParameter(
                        parameter,
                    )) if parameter.ordinal() == 0
                        && node.value_type().scalar_type() == ScalarType::Real =>
                    {
                        counts.1 += 1;
                    }
                    ExpressionOperation::FunctionValue { definition, .. }
                        if node.value_type().scalar_type() == ScalarType::Real
                            && view.expression(definition.rhs()).is_some_and(|rhs| {
                                rhs.value_type().scalar_type() == ScalarType::Integer
                            }) =>
                    {
                        counts.2 += 1;
                    }
                    _ => {}
                }
            }
            counts
        });
    assert!(
        integer_call_arguments >= 2,
        "original Integer call operands stay issued"
    );
    assert!(
        real_parameters >= 1,
        "constructor-issued Real parameter is exercised"
    );
    assert!(
        real_integer_definitions >= 1,
        "constructor-issued Real local forwards an Integer RHS"
    );
    let model = rumoca_phase_solve::lower_solve_model(&compiled.dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone();
    assert_eq!(
        phasors(&model),
        BTreeMap::from([(
            "direct".into(),
            phasor(SolvePhasorFunction::Angle, "re", "im")
        )]),
        "both Real guards compare rounded numeric operands; neither guarded result is an angle proof"
    );
}

#[test]
fn real_function_bindings_select_their_actual_rounded_guard_with_canonical_conversion() {
    let source = r#"
model RoundedAngleContexts
  function byParameter
    input Real n;
    input Real im;
    input Real re;
    output Real p;
  algorithm
    if n == 9007199254740992 then p := atan2(im,re); else p := 0; end if;
  end byParameter;
  function byValue
    input Integer n;
    input Real im;
    input Real re;
    output Real p;
  protected
    Real q;
  algorithm
    q := n;
    if q == 9007199254740992 then p := atan2(im,re); else p := 0; end if;
  end byValue;
  Real re(start=1), im(start=0);
  Real throughParameter, throughValue;
equation
  der(re)=-im;
  der(im)=re;
  throughParameter=byParameter(9007199254740993,im,re);
  throughValue=byValue(9007199254740993,im,re);
end RoundedAngleContexts;
"#;
    assert_eq!(
        phasors(&lowered(source, "RoundedAngleContexts")),
        BTreeMap::from([
            (
                "throughParameter".into(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
            (
                "throughValue".into(),
                phasor(SolvePhasorFunction::Angle, "re", "im")
            ),
        ]),
        "declared Real conversion selects the rounded guard for both parameter and local bindings"
    );
}

#[test]
fn canonical_source_evaluator_confirms_real_guard_rounding_before_phasor_proof() {
    let source = REAL_BINDING_CONTEXTS
        .replace(
            "byParameter(9007199254740992,im,re)",
            "byParameter(9007199254740992,1.0,0.0)",
        )
        .replace(
            "byValue(9007199254740992,im,re)",
            "byValue(9007199254740992,1.0,0.0)",
        );
    assert_eq!(
        source.split("  Real re(start=1)").next(),
        REAL_BINDING_CONTEXTS.split("  Real re(start=1)").next(),
        "original function bodies and declarations are unchanged; only wrapper call coordinates are literal"
    );
    let compiled = Compiler::new()
        .model("RealAngleContexts")
        .compile_str(&source, "RealAngleContexts.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        let calls = (0..view.expression_count())
            .map(|index| view.expression_id(index).unwrap())
            .filter(|id| {
                let expression = view.expression(*id).unwrap();
                expression.function_scope().is_none()
                    && matches!(
                        expression.operation(),
                        rumoca_ir_dae::ExpressionOperation::Call { .. }
                    )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2, "both original function calls execute");
        let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(view);
        for call in calls {
            let value = evaluator
                .expression(call)
                .expect("checked literal call inputs");
            assert_eq!(
                value,
                vec![0.0],
                "the declared Real comparison selects the zero arm"
            );
        }
    });
}
