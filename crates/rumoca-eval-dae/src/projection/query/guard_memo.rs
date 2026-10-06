//! A successful guard owns ordered effects, never an empty dependency shortcut.
mod eligibility;
mod enqueue;
mod integration;
#[cfg(test)]
mod tests;

use super::super::*;
use domain_context::DomainContextId;

const KEY_LIMIT: usize = 65_536;
const PAYLOAD_LIMIT: usize = 16 * 1024 * 1024;
const RECORDING_LIMIT: usize = 128;

fn supported_builtin<'dae>(view: dae::DaeView<'dae>, node: dae::ExpressionView<'dae>) -> bool {
    match node.operation() {
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Size | dae::PureBuiltin::Floor,
            ..
        } => true,
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Abs,
            arguments,
        } => {
            let operand = view
                .expression(arguments.get(0).expect("checked Abs has one operand"))
                .expect("checked Abs operand resolves");
            scalar_real(node) && scalar_real(operand)
        }
        _ => false,
    }
}

fn scalar_real(node: dae::ExpressionView<'_>) -> bool {
    node.value_type().scalar_type() == dae::ScalarType::Real
        && node.value_type().dimensions().is_empty()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Key {
    activation: Activation,
    expression: u32,
    context: DomainContextId,
}

#[derive(Clone, Debug)]
enum Effect<'dae> {
    Pending(fold_graph::FoldNode<'dae>),
    Parameter {
        function: dae::FunctionId<'dae>,
        dependency: FunctionParameterDependency,
        span: Span,
    },
}

impl Effect<'_> {
    fn charge(&self) -> usize {
        let extra = match self {
            Self::Pending(node) => node
                .parent
                .iter()
                .map(|(_, point)| {
                    std::mem::size_of::<(u32, Vec<i64>)>()
                        + point.len() * std::mem::size_of::<i64>()
                })
                .sum(),
            Self::Parameter { .. } => 0,
        };
        std::mem::size_of::<Self>() + extra
    }
}

#[derive(Clone)]
struct Checked<'dae> {
    effects: Arc<[Effect<'dae>]>,
    cacheable: bool,
}

struct Recording<'dae> {
    key: Key,
    effects: Vec<Effect<'dae>>,
    bytes: usize,
    complete: bool,
}

enum Start<'dae> {
    None,
    Hit(Checked<'dae>),
    Checking,
}

/// One exact root actual tuple and its sole summary capture own every entry.
/// Both retained and in-flight effect storage are bounded; failures publish nothing.
pub(in crate::projection) struct GuardMemo<'dae> {
    root: dae::FunctionId<'dae>,
    actuals: Vec<dae::ExprId<'dae>>,
    eligible: HashMap<u32, bool>,
    traversal: dae::ExpressionTraversal<'dae>,
    checked: HashMap<Key, Checked<'dae>>,
    recording: Vec<Recording<'dae>>,
    bytes: usize,
    key_limit: usize,
    payload_limit: usize,
    admission_saturated: bool,
    #[cfg(test)]
    pub(in crate::projection) hits: u64,
}

impl<'dae> GuardMemo<'dae> {
    fn new(root: dae::FunctionId<'dae>, actuals: Vec<dae::ExprId<'dae>>) -> Self {
        Self {
            root,
            actuals,
            eligible: HashMap::default(),
            traversal: dae::ExpressionTraversal::new(),
            checked: HashMap::default(),
            recording: Vec::new(),
            bytes: 0,
            key_limit: KEY_LIMIT,
            payload_limit: PAYLOAD_LIMIT,
            admission_saturated: false,
            #[cfg(test)]
            hits: 0,
        }
    }

    fn independent_walk(&self) -> bool {
        self.recording.iter().any(|recording| recording.complete)
    }

    fn begin(&mut self, key: Key) -> Start<'dae> {
        if let Some(checked) = self.checked.get(&key) {
            #[cfg(test)]
            {
                self.hits += 1;
            }
            return Start::Hit(checked.clone());
        }
        if self.admission_saturated
            || self.checked.len() >= self.key_limit
            || self.recording.len() == RECORDING_LIMIT
        {
            return Start::None;
        }
        self.recording.push(Recording {
            key,
            effects: Vec::new(),
            bytes: 0,
            complete: true,
        });
        Start::Checking
    }

    fn record(&mut self, effect: Effect<'dae>) {
        let charge = effect.charge();
        for recording in &mut self.recording {
            if !recording.complete {
                continue;
            }
            if charge > self.payload_limit.saturating_sub(self.bytes) {
                self.admission_saturated = true;
                self.bytes -= recording.bytes;
                recording.bytes = 0;
                recording.effects = Vec::new();
                recording.complete = false;
                continue;
            }
            recording.effects.push(effect.clone());
            recording.bytes += charge;
            self.bytes += charge;
        }
    }

    fn invalidate(&mut self) {
        for recording in &mut self.recording {
            self.bytes -= recording.bytes;
            recording.bytes = 0;
            recording.effects = Vec::new();
            recording.complete = false;
        }
    }

    fn finish(&mut self, success: bool, cacheable: bool) {
        let recording = self.recording.pop().expect("guard recording was opened");
        if success && recording.complete && self.checked.len() < self.key_limit {
            self.checked.insert(
                recording.key,
                Checked {
                    effects: recording.effects.into(),
                    cacheable,
                },
            );
        } else {
            self.bytes -= recording.bytes;
        }
    }
}
