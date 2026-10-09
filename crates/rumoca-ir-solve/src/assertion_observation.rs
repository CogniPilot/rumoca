//! Borrowed observation authority for one exact scalar-program invocation.

use crate::{
    LinearOp, ScalarProgramRegisterFlow, SolveAssertionLevel, SolveEventAction,
    SolveEventActionKind, SolveEventMessagePart, SolvePureCallOwner, SolvePureCallSite,
    SolvePureCallTable, SolveStringConversionFormat, SolveStringConversionSource,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Construction recipe for only the assertion observations of an issued
/// value owner. It carries no ordinary result projection.
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct SolveAssertionObservationSite {
    value_site: SolvePureCallSite,
    predicates: Arc<[usize]>,
}

impl SolveAssertionObservationSite {
    pub fn new(value_site: SolvePureCallSite) -> Option<Self> {
        let predicates: Vec<_> = value_site
            .outputs()
            .iter()
            .enumerate()
            .filter_map(|(index, output)| output.assertion_level().map(|_| index))
            .collect();
        if predicates.is_empty() {
            return None;
        }
        Some(Self {
            value_site,
            predicates: predicates.into(),
        })
    }

    #[must_use]
    pub const fn value_site(&self) -> &SolvePureCallSite {
        &self.value_site
    }
    #[must_use]
    pub fn predicate_outputs(&self) -> &[usize] {
        &self.predicates
    }
    #[must_use]
    pub fn output_scalar_count(&self) -> usize {
        self.predicates.len()
    }

    /// Reconstruct the recipe from the exact model table before admitting it.
    #[must_use]
    pub fn matches_table(&self, table: &SolvePureCallTable) -> bool {
        table.matches_site(&self.value_site)
            && table
                .owner(self.value_site.owner())
                .is_some_and(|owner| owner.assertion_flow().is_some())
            && Self::new(self.value_site.clone()).as_ref() == Some(self)
    }
}

/// A selector reconstructed from the source-issued message row. Captured
/// selectors read initialized observations; they never execute the row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssertionCaptureSelector {
    output: usize,
}

impl AssertionCaptureSelector {
    #[must_use]
    pub const fn output(self) -> usize {
        self.output
    }
}

pub struct CheckedAssertionConversion {
    pub value: AssertionCaptureSelector,
    pub source: SolveStringConversionSource,
    pub minimum_length: Option<AssertionCaptureSelector>,
    pub left_justified: Option<AssertionCaptureSelector>,
    pub significant_digits: Option<AssertionCaptureSelector>,
}

pub enum CheckedAssertionMessagePart<'model> {
    Text(&'model str),
    Conversion(CheckedAssertionConversion),
}

pub struct CheckedAssertionAction<'model> {
    action: &'model SolveEventAction,
    action_index: usize,
    predicate_output: usize,
    message: Box<[CheckedAssertionMessagePart<'model>]>,
}

impl CheckedAssertionAction<'_> {
    #[must_use]
    pub const fn action(&self) -> &SolveEventAction {
        self.action
    }

    #[must_use]
    pub const fn action_index(&self) -> usize {
        self.action_index
    }

    #[must_use]
    pub const fn predicate_output(&self) -> usize {
        self.predicate_output
    }

    #[must_use]
    pub fn message(&self) -> &[CheckedAssertionMessagePart<'_>] {
        &self.message
    }
}

/// Authority borrows all three immutable issuing inventories together.
/// Neither a detached recipe nor a failed-call diagnostic can construct it.
pub struct CheckedAssertionInvocation<'model> {
    table: &'model SolvePureCallTable,
    program: &'model [LinearOp],
    operation: usize,
    owner: &'model SolvePureCallOwner,
    actions: &'model [SolveEventAction],
    projections: Box<[CheckedAssertionAction<'model>]>,
}

impl<'model> CheckedAssertionInvocation<'model> {
    pub fn new(
        table: &'model SolvePureCallTable,
        program: &'model [LinearOp],
        operation: usize,
        actions: &'model [SolveEventAction],
    ) -> Option<Self> {
        ScalarProgramRegisterFlow::derive(program).ok()?;
        let site = match program.get(operation)? {
            LinearOp::PureCall { site, .. } => site,
            LinearOp::PureCallObservation { site, .. } if site.matches_table(table) => {
                site.value_site()
            }
            _ => return None,
        };
        if !table.matches_site(site) {
            return None;
        }
        let owner = table.owner(site.owner())?;
        owner.assertion_flow()?;
        let mut projections = Vec::new();
        for (predicate_output, output) in site.outputs().iter().enumerate() {
            let Some(level) = output.assertion_level() else {
                continue;
            };
            projections.extend(checked_actions(actions, site, predicate_output, level)?);
        }
        Some(Self {
            table,
            program,
            operation,
            owner,
            actions,
            projections: projections.into_boxed_slice(),
        })
    }

    #[must_use]
    pub const fn table(&self) -> &'model SolvePureCallTable {
        self.table
    }
    #[must_use]
    pub const fn program(&self) -> &'model [LinearOp] {
        self.program
    }
    #[must_use]
    pub const fn operation(&self) -> usize {
        self.operation
    }
    #[must_use]
    pub const fn owner(&self) -> &'model SolvePureCallOwner {
        self.owner
    }
    #[must_use]
    pub const fn actions(&self) -> &'model [SolveEventAction] {
        self.actions
    }
    #[must_use]
    pub fn projections(&self) -> &[CheckedAssertionAction<'model>] {
        &self.projections
    }
}

fn checked_actions<'model>(
    actions: &'model [SolveEventAction],
    site: &SolvePureCallSite,
    predicate_output: usize,
    level: SolveAssertionLevel,
) -> Option<Vec<CheckedAssertionAction<'model>>> {
    let projections = actions
        .iter()
        .enumerate()
        .filter(|(_, action)| {
            action.assertion_projection.as_ref().is_some_and(|recipe| {
                recipe.site() == site && recipe.predicate_output() == predicate_output
            })
        })
        .map(|(index, action)| check_action(action, index, site, predicate_output, level))
        .collect::<Option<Vec<_>>>()?;
    (!projections.is_empty()).then_some(projections)
}

fn check_action<'model>(
    action: &'model SolveEventAction,
    action_index: usize,
    site: &SolvePureCallSite,
    predicate_output: usize,
    level: SolveAssertionLevel,
) -> Option<CheckedAssertionAction<'model>> {
    let kind = match level {
        SolveAssertionLevel::Error => SolveEventActionKind::Assert,
        SolveAssertionLevel::Warning => SolveEventActionKind::Warning,
    };
    if action.kind != kind {
        return None;
    }
    let message = action
        .message
        .parts
        .iter()
        .map(|part| match part {
            SolveEventMessagePart::Text(text) => Some(CheckedAssertionMessagePart::Text(text)),
            SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            } => {
                let SolveStringConversionFormat::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } = format;
                Some(CheckedAssertionMessagePart::Conversion(
                    CheckedAssertionConversion {
                        value: select_capture(value, site, predicate_output, *source)?,
                        source: *source,
                        minimum_length: select_option(
                            minimum_length.as_deref(),
                            site,
                            predicate_output,
                            SolveStringConversionSource::Integer,
                        )?,
                        left_justified: select_option(
                            left_justified.as_deref(),
                            site,
                            predicate_output,
                            SolveStringConversionSource::Boolean,
                        )?,
                        significant_digits: select_option(
                            significant_digits.as_deref(),
                            site,
                            predicate_output,
                            SolveStringConversionSource::Integer,
                        )?,
                    },
                ))
            }
        })
        .collect::<Option<Vec<_>>>()?;
    Some(CheckedAssertionAction {
        action,
        action_index,
        predicate_output,
        message: message.into_boxed_slice(),
    })
}

fn select_option(
    row: Option<&[LinearOp]>,
    site: &SolvePureCallSite,
    predicate: usize,
    source: SolveStringConversionSource,
) -> Option<Option<AssertionCaptureSelector>> {
    match row {
        Some(row) => Some(Some(select_capture(row, site, predicate, source)?)),
        None => Some(None),
    }
}

fn select_capture(
    row: &[LinearOp],
    site: &SolvePureCallSite,
    predicate: usize,
    source: SolveStringConversionSource,
) -> Option<AssertionCaptureSelector> {
    ScalarProgramRegisterFlow::derive(row).ok()?;
    let LinearOp::StoreOutput { src } = row.last()? else {
        return None;
    };
    let producer = row[..row.len() - 1].iter().rev().find(|op| {
        op.dst_register().is_some_and(|start| {
            *src >= start
                && usize::try_from(*src - start)
                    .ok()
                    .is_some_and(|offset| offset < op.dst_register_count())
        })
    })?;
    match producer {
        LinearOp::PureCall {
            site: message_site,
            dst_start,
            ..
        } if message_site == site => select_output(site, *dst_start, *src, predicate, source),
        _ => None,
    }
}

fn select_output(
    site: &SolvePureCallSite,
    mut offset: u32,
    source_register: u32,
    predicate: usize,
    source: SolveStringConversionSource,
) -> Option<AssertionCaptureSelector> {
    for (output, leaf) in site.outputs().iter().enumerate() {
        if offset == source_register
            && leaf.value_type().scalar_count() == 1
            && leaf.message_predicate_output(output) == Some(predicate)
        {
            let matches = matches!(
                (leaf.value_type().element_type(), source),
                (
                    crate::SolveScalarType::Real { .. },
                    SolveStringConversionSource::Real
                ) | (
                    crate::SolveScalarType::Integer(_),
                    SolveStringConversionSource::Integer
                ) | (
                    crate::SolveScalarType::Boolean,
                    SolveStringConversionSource::Boolean
                )
            );
            return matches.then_some(AssertionCaptureSelector { output });
        }
        offset = offset.checked_add(leaf.value_type().scalar_count())?;
    }
    None
}
