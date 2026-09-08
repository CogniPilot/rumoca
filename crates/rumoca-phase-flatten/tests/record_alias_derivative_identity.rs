//! Regression coverage for reads redirected through a record alias.
//!
//! `Modelica.Magnetic.FundamentalWave.Interfaces.TwoPortExtended` declares
//! `SI.ComplexMagneticFlux Phi = port_p.Phi`, a record bound to another record
//! by a plain path, and `Components.EddyCurrent` differentiates its fields:
//!
//! ```modelica
//! (pi/2)*V_m.re = G*der(Phi.re);
//! ```
//!
//! Flattening redirects such a read to the alias target. The redirected read
//! must carry the target variable's exact identity: DAE construction proves a
//! `der` operand by its allocated occurrence, so a spelling-only redirect left
//! every FundamentalWave machine example refused with
//! `ED019 unsupported Flat semantic owner 'derivative target identity'`.

use std::collections::BTreeSet;

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

const SOURCE_NAME: &str = "<record_alias_derivative_identity>";
const SOURCE: &str = r#"
package P
  record R
    Real re;
    Real im;
  end R;

  model Carrier
    R Phi;
  end Carrier;

  model Loss
    Carrier port;
    R Phi = port.Phi;
    parameter Real G = 2.0;
    Real v_re;
    Real v_im;
  equation
    v_re = G*der(Phi.re);
    v_im = G*der(Phi.im);
  end Loss;

  model Top
    Loss loss;
  equation
    loss.v_re = 1.0;
    loss.v_im = 0.5;
  end Top;
end P;
"#;

const ALIAS_TARGETS: [&str; 2] = ["loss.port.Phi.re", "loss.port.Phi.im"];

fn typed_flat_model(model_name: &str) -> (flat::Model, rumoca_core::SourceMap) {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(
        resolved.inner(),
        model_name,
    ) {
        rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
        rumoca_phase_instantiate::InstantiationOutcome::NeedsInner { missing_inners, .. } => {
            panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
        }
        rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
            panic!("fixture instantiation failed: {error}")
        }
    };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, model_name)
        .expect("instanced model typechecks");
    let source_map = resolved.inner().source_map.clone();
    let flat =
        rumoca_phase_flatten::flatten_typed(typed, rumoca_phase_flatten::FlattenOptions::default())
            .expect("typed model flattens");
    (flat, source_map)
}

fn variable<'model>(model: &'model flat::Model, name: &str) -> &'model flat::Variable {
    model
        .variables
        .get(&rumoca_core::VarName::new(name))
        .unwrap_or_else(|| panic!("flat model owns `{name}`"))
}

/// Every `der(...)` operand reference in the model's continuous equations.
struct DerivativeOperands(Vec<rumoca_core::Reference>);

impl ExpressionVisitor for DerivativeOperands {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Der
            && let Some(rumoca_core::Expression::VarRef { name, .. }) = args.first()
        {
            self.0.push(name.clone());
        }
        self.walk_builtin_call(function, args);
    }
}

fn derivative_operands(model: &flat::Model) -> Vec<rumoca_core::Reference> {
    let mut operands = DerivativeOperands(Vec::new());
    for equation in &model.equations {
        operands.visit_expression(&equation.residual);
    }
    operands.0
}

#[test]
fn a_derivative_operand_redirected_through_a_record_alias_keeps_the_target_identity() {
    let (flat, _) = typed_flat_model("P.Top");

    let operands = derivative_operands(&flat);
    let spelled: BTreeSet<&str> = operands.iter().map(|operand| operand.as_str()).collect();
    assert_eq!(spelled, ALIAS_TARGETS.into_iter().collect::<BTreeSet<_>>());

    for operand in &operands {
        let target = variable(&flat, operand.as_str());
        assert_eq!(
            operand.instance_id(),
            Some(target.instance_id),
            "`{}` must read the occurrence Flat allocated to the alias target",
            operand.as_str()
        );
        assert_eq!(
            operand.component_ref(),
            target.component_ref.as_ref(),
            "`{}` must carry the alias target's structured component reference",
            operand.as_str()
        );
    }
}

#[test]
fn the_redirected_derivative_selects_the_alias_target_occurrence_as_the_state() {
    let (flat, source_map) = typed_flat_model("P.Top");
    let expected: BTreeSet<u32> = ALIAS_TARGETS
        .into_iter()
        .map(|name| variable(&flat, name).instance_id.index())
        .collect();

    let product = rumoca_phase_dae::construct(&flat, source_map)
        .expect("the redirected derivative operands name exact state occurrences");
    let states = product.dae().inspect(|view| {
        view.variables()
            .filter(|(_, variable)| {
                matches!(
                    variable.identity(),
                    rumoca_ir_dae::VariableIdentity::State(_)
                )
            })
            .map(|(_, variable)| variable.source_occurrence().index())
            .collect::<BTreeSet<_>>()
    });
    assert_eq!(states, expected);
}
