//! Source-issued local factor channels, projected into the original FMI inventory.
use std::collections::BTreeMap;

use super::{FmiMetadata, FmiStorageColumn, FmiStorageRun, FmiVariable};
use crate::refresh::articulated_bindings::{self, ArticulatedApplication, SourceCell};
use crate::{SolveModel, SolveVariableValueKind, TensorInputKind};

/// One ordinary Float64 element or source literal. No public constructor can
/// issue source correspondence; this exposes numerical data, never Solve slots.
#[derive(Debug)]
pub struct FmiFactorChannel(Channel);

#[derive(Debug)]
enum Channel {
    Literal(f64),
    Variable {
        reference: u32,
        element: usize,
        negated: bool,
    },
}

impl FmiFactorChannel {
    pub fn literal(&self) -> Option<f64> {
        match self.0 {
            Channel::Literal(value) => Some(value),
            _ => None,
        }
    }
    /// FMI value reference, zero-based element within its declared aggregate,
    /// and the exact source sign. It is not an internal storage coordinate.
    pub fn variable(&self) -> Option<(u32, usize, bool)> {
        match self.0 {
            Channel::Variable {
                reference,
                element,
                negated,
            } => Some((reference, element, negated)),
            _ => None,
        }
    }
}

/// Complete channels of one checked zero-increment C112 application.
/// This proves neither topology, basis, damping, domain nor state coverage;
/// no runtime profile or modelDescription annotation is issued by this type.
#[derive(Debug)]
pub struct FmiArticulatedReduction {
    inputs: Box<[Box<[FmiFactorChannel]>]>,
    outputs: Box<[FmiFactorChannel]>,
}

impl FmiArticulatedReduction {
    /// Source order I(36), S(6), armature, positive zero increment, effort,
    /// bias(6), convective acceleration(6); row-major fixed spatial tensors.
    pub fn inputs(&self) -> &[Box<[FmiFactorChannel]>] {
        &self.inputs
    }
    /// Source order U(6), D, u, Ia(36), pa(6), all normal FMI variables.
    pub fn outputs(&self) -> &[FmiFactorChannel] {
        &self.outputs
    }
}

pub(super) fn derive(model: &SolveModel, metadata: &FmiMetadata) -> Vec<FmiArticulatedReduction> {
    let applications = articulated_bindings::derive(&model.problem, &model.pure_calls);
    if applications.is_empty() {
        return Vec::new();
    }
    let inventory = Inventory::new(metadata);
    applications
        .into_iter()
        .filter_map(|app| project(app, &inventory))
        .collect()
}

fn project(
    app: ArticulatedApplication,
    inventory: &Inventory<'_>,
) -> Option<FmiArticulatedReduction> {
    let inputs = app
        .inputs
        .into_iter()
        .map(|group| {
            group
                .into_iter()
                .map(|value| inventory.project(value))
                .collect::<Option<Box<[_]>>>()
        })
        .collect::<Option<Box<[_]>>>()?;
    let outputs = app
        .outputs
        .into_iter()
        .map(|index| {
            inventory.project(SourceCell::Storage {
                input: TensorInputKind::Y,
                index,
                negated: false,
            })
        })
        .collect::<Option<Box<[_]>>>()?;
    Some(FmiArticulatedReduction { inputs, outputs })
}

struct Inventory<'a> {
    y: BTreeMap<usize, (&'a FmiVariable, FmiStorageRun)>,
    p: BTreeMap<usize, (&'a FmiVariable, FmiStorageRun)>,
}

impl<'a> Inventory<'a> {
    fn new(metadata: &'a FmiMetadata) -> Self {
        let mut result = Self {
            y: BTreeMap::new(),
            p: BTreeMap::new(),
        };
        for (variable, storage) in metadata
            .variables()
            .iter()
            .filter_map(|variable| variable.storage().map(|storage| (variable, storage)))
        {
            let column = match storage.column() {
                FmiStorageColumn::Y => &mut result.y,
                FmiStorageColumn::P => &mut result.p,
            };
            column.insert(storage.base(), (variable, storage));
        }
        result
    }
    fn project(&self, source: SourceCell) -> Option<FmiFactorChannel> {
        let (input, index, negated) = match source {
            SourceCell::Literal(value) => return Some(FmiFactorChannel(Channel::Literal(value))),
            SourceCell::Storage {
                input,
                index,
                negated,
            } => (input, index, negated),
        };
        let column = match input {
            TensorInputKind::Y => &self.y,
            TensorInputKind::P => &self.p,
        };
        let (_, &(variable, storage)) = column.range(..=index).next_back()?;
        let element = index.checked_sub(storage.base())?;
        if element >= storage.scalar_count()
            || variable.value_kind() != SolveVariableValueKind::Real
        {
            return None;
        }
        Some(FmiFactorChannel(Channel::Variable {
            reference: variable.value_reference_fmi3(),
            element,
            negated,
        }))
    }
}
