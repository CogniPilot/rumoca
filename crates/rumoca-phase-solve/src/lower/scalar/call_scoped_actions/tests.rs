use super::*;

#[test]
fn unclocked_guarded_call_assertion_uses_active_only_program() {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("activated_call_assertion.mo", "false");
    let span = Span::from_offsets(source, 0, 5);
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let (trigger, guard) = model.conditions(|conditions| {
            Ok((
                conditions.reserve(provenance)?,
                conditions.reserve(provenance)?,
            ))
        })?;
        model.conditions(|conditions| {
            conditions.define(trigger, dae::ConditionInput::Always, provenance)?;
            conditions.define(guard, dae::ConditionInput::Always, provenance)
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let trigger = view.condition_id(0).unwrap();
        let guard = view.condition_id(1).unwrap();
        let condition = view.expression_id(0).unwrap();
        let mut compiler = ScalarCompiler::new(view, &layout, None);
        compiler.activation_path.push(ActivationGuard {
            condition: ActivationCondition::GuardedAssignment {
                clock: None,
                periodic: false,
                trigger,
                guard,
                trigger_memory: 0,
            },
            register: None,
            expected: true,
        });

        // This direct compiler-unit setup pins the production branch guard:
        // an unclocked GuardedAssignment, not merely a sample() source spelling.
        let operations = compiler.assertion_action_program(condition, span).unwrap();
        let program = operations
            .iter()
            .find_map(|operation| match operation {
                solve::LinearOp::FunctionConditional { program, .. } => Some(program.as_ref()),
                _ => None,
            })
            .expect("activated assertion action emits a conditional program");

        assert_eq!(program.capture_count, 1);
        assert_eq!(program.result_count, 1);
        assert_eq!(program.arms.len(), 1);
        assert_eq!(
            program.fallback.as_slice(),
            &[
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        );
        assert_eq!(
            program.arms[0].condition.as_slice(),
            &[
                solve::LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        );
        assert!(matches!(
            program.arms[0].result.as_slice(),
            [
                solve::LinearOp::Const { dst: condition_register, value },
                solve::LinearOp::Unary { dst: failed_register, op: solve::UnaryOp::Not, arg },
                solve::LinearOp::StoreOutput { src },
            ] if *value == 0.0 && arg == condition_register && src == failed_register
        ));
    });
}
