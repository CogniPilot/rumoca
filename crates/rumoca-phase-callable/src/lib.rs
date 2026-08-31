//! Sole semantic classification pass from checked DAE callables to the
//! target-neutral callable proof plan.

mod emission;
mod error;

pub use error::CallablePhaseError;

use rumoca_ir_dae::Dae;
use rumoca_plan_callable::CallablePlan;

/// Consume one checked DAE and construct its complete callable proof plan.
///
/// Any unsupported occurrence rejects the whole construction. No partial plan
/// is observable.
pub fn build_callable_plan(dae: Dae) -> Result<CallablePlan, CallablePhaseError> {
    CallablePlan::construct(dae, emission::emit)
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, VarName};
    use rumoca_ir_dae::{
        DaeConstruction, DaeConstructionError, DaeLiteral, DaeProvenance, FunctionReservation,
        FunctionSignature, ScalarType, ValueType,
    };

    use super::*;

    /// `function f output Real y; algorithm y := 1.0; end f;` as checked DAE.
    fn define_constant_function<'function, 'dae>(
        dae: &mut DaeConstruction<'dae>,
        reservation: FunctionReservation<'function, 'dae>,
        declaration: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, declaration)
        })?;
        let value = dae.expressions(|expressions| {
            expressions.at(declaration).literal(DaeLiteral::Real(1.0))
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, declaration))?;
        dae.functions(|functions| functions.assign(&mut body, output, value, declaration))?;
        dae.functions(|functions| functions.define(body, declaration))?;
        Ok(())
    }

    fn one_constant_function(source_map: SourceMap, declaration: DaeProvenance) -> Dae {
        Dae::construct(source_map, |dae| {
            let real =
                dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), declaration))?;
            dae.function(
                FunctionSignature::new(VarName::new("f"), [], [real], declaration),
                |dae, reservation| define_constant_function(dae, reservation, declaration),
            )?;
            Ok(())
        })
        .expect("fixture DAE constructs")
    }

    #[test]
    fn nonempty_callable_graph_fails_closed_at_the_exact_declaration_span() {
        let mut source_map = SourceMap::new();
        let source = source_map.add(
            "callable-phase-test.mo",
            "function f output Real y; algorithm y := 1.0; end f;",
        );
        let declaration_span = Span::from_offsets(source, 0, 10);
        let declaration =
            DaeProvenance::source(declaration_span).expect("test declaration is source-backed");
        let dae = one_constant_function(source_map, declaration);

        let error = build_callable_plan(dae)
            .expect_err("an unfinished semantic emitter must refuse the whole plan");
        assert_eq!(error.code(), "EL001");
        assert_eq!(error.source_span(), declaration_span);
    }

    #[test]
    fn empty_callable_graph_constructs_without_fabricated_obligations() {
        let dae = Dae::construct(SourceMap::new(), |_dae| Ok(())).expect("empty DAE constructs");
        let plan = build_callable_plan(dae).expect("empty callable graph is complete");
        assert_eq!(plan.counters().source_functions(), 0);
        assert_eq!(plan.counters().operations(), 0);
    }
}
