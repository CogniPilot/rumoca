//! Bounded diagnostic attribution, compiled only with debug assertions.
use super::*;

#[derive(Clone, Copy, Debug)]
pub(in crate::projection) enum Event {
    EligibleCached,
    EligibleDerived,
    IneligibleCached,
    Builtin,
    Clock,
    Record,
    Comprehension,
    String,
    Coordinate,
    External,
    RootCall,
    OtherUnsupported,
    FrameFallback,
    ContextFallback,
    Hit,
    Checking,
    KeyCapacity,
    PayloadCapacity,
    RecordingCapacity,
    Saturated,
    PendingEffect,
    ParameterEffect,
    Invalidated,
    Published,
    Discarded,
}

const COUNT: usize = 25;

pub(in crate::projection) fn event(
    root: u32,
    event: Event,
    expression: Option<u32>,
    state: (usize, usize, usize, usize),
) {
    static COUNTS: [AtomicUsize; COUNT] = [const { AtomicUsize::new(0) }; COUNT];
    static TOTAL: AtomicUsize = AtomicUsize::new(0);
    if !enabled() {
        return;
    }
    let ordinal = COUNTS[event as usize].fetch_add(1, Ordering::Relaxed);
    let total = TOTAL.fetch_add(1, Ordering::Relaxed);
    if ordinal < 4 || total.is_multiple_of(100_000) {
        let counts = COUNTS.each_ref().map(|count| count.load(Ordering::Relaxed));
        let (keys, bytes, recordings, eligibility) = state;
        eprintln!(
            "SCALAR_GUARD_MEMO root={root} event={event:?} expression={expression:?} total={total} keys={keys} charged_bytes={bytes} recordings={recordings} eligibility_entries={eligibility} counts={counts:?}"
        );
    }
}

pub(in crate::projection) fn unsupported(node: dae::ExpressionView<'_>) -> Event {
    if node.value_type().is_record() {
        return Event::Record;
    }
    match node.operation() {
        dae::ExpressionOperation::Builtin { .. } => Event::Builtin,
        dae::ExpressionOperation::ClockTransfer { .. } => Event::Clock,
        dae::ExpressionOperation::Record(_) | dae::ExpressionOperation::Field { .. } => {
            Event::Record
        }
        dae::ExpressionOperation::Comprehension { .. } => Event::Comprehension,
        dae::ExpressionOperation::StringConversion { .. } => Event::String,
        dae::ExpressionOperation::Coordinate(_) => Event::Coordinate,
        dae::ExpressionOperation::Call { .. } => Event::External,
        _ => Event::OtherUnsupported,
    }
}

pub(in crate::projection) fn refusal<'dae>(
    root: u32,
    expression: u32,
    node: dae::ExpressionView<'dae>,
    view: dae::DaeView<'dae>,
) {
    static EXAMPLES: AtomicUsize = AtomicUsize::new(0);
    if !enabled() || EXAMPLES.fetch_add(1, Ordering::Relaxed) >= 32 {
        return;
    }
    if let dae::ExpressionOperation::Builtin { builtin, arguments } = node.operation() {
        eprintln!(
            "SCALAR_GUARD_REFUSAL root={root} expression={expression} builtin={builtin:?} result_scalar={:?} result_rank={} result_dimensions={:?} span={:?}",
            node.value_type().scalar_type(),
            node.value_type().dimensions().len(),
            node.value_type()
                .dimensions()
                .iter()
                .take(8)
                .collect::<Vec<_>>(),
            node.provenance().span(),
        );
        for (ordinal, argument) in arguments.iter().take(4).enumerate() {
            let operand = view
                .expression(argument)
                .expect("checked builtin operand resolves");
            eprintln!(
                "SCALAR_GUARD_OPERAND root={root} expression={expression} ordinal={ordinal} operand={} scalar={:?} rank={} dimensions={:?} operation={} span={:?}",
                argument.index(),
                operand.value_type().scalar_type(),
                operand.value_type().dimensions().len(),
                operand
                    .value_type()
                    .dimensions()
                    .iter()
                    .take(8)
                    .collect::<Vec<_>>(),
                operand_kind(operand),
                operand.provenance().span(),
            );
        }
    }
}

fn operand_kind(node: dae::ExpressionView<'_>) -> String {
    match node.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
            format!("Integer({value})")
        }
        dae::ExpressionOperation::Literal(_) => "literal".into(),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            format!(
                "formal(function={},ordinal={})",
                parameter.function().index(),
                parameter.ordinal()
            )
        }
        dae::ExpressionOperation::Coordinate(_) => "coordinate".into(),
        dae::ExpressionOperation::FunctionFoldParameter { .. } => "fold_parameter".into(),
        dae::ExpressionOperation::FunctionFoldOutput { .. } => "fold_output".into(),
        dae::ExpressionOperation::FunctionValue { .. } => "function_value".into(),
        dae::ExpressionOperation::Index { .. } => "index".into(),
        dae::ExpressionOperation::Call { .. } => "call".into(),
        _ => "other".into(),
    }
}
