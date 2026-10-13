//! Original declaration-backed observations and explicitly authored expressions.

use crate::{LinearOp, ScalarProgramBlock, ScalarSlot, SolveProblem, SolveVariableMeta};
use rumoca_core::{Span, VarName};
use serde::{Deserialize, Serialize};

mod declaration;
mod facts;
mod points;
mod scalar;

/// Unchecked current-wire construction inputs, never executable authority.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct VisibleDeclarationInput {
    pub declaration: usize,
    pub binding: VarName,
    pub attributes: VisibleSourceAttributes,
    pub state_coordinates: Vec<(usize, crate::SolveStateCoordinate)>,
    pub phasors: Vec<(usize, crate::SolvePhasor)>,
}

/// Attributes authored once on the selected original declaration.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct VisibleSourceAttributes {
    pub source_span: Span,
    pub variability: Option<String>,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", deny_unknown_fields)]
pub enum VisibleProjectionInput {
    Declaration(Box<VisibleDeclarationInput>),
    /// Actual authored expression payload; never synthesized from a tensor run.
    Expression {
        names: Vec<String>,
        rows: ScalarProgramBlock,
        metadata: Option<Vec<SolveVariableMeta>>,
    },
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct VisibleProjection {
    members: Vec<VisibleProjectionInput>,
    #[serde(skip)]
    scalar_count: usize,
}

/// A demanded observation view issued from one model's two correlated owners.
pub struct VisibleProjectionView<'model> {
    problem: &'model SolveProblem,
    projection: &'model VisibleProjection,
}

impl crate::SolveModel {
    pub fn visible_projection(&self) -> VisibleProjectionView<'_> {
        VisibleProjectionView {
            problem: &self.problem,
            projection: &self.visible_projection,
        }
    }
}

impl VisibleProjectionView<'_> {
    pub fn name(&self, point: usize) -> Option<String> {
        self.projection.name(self.problem, point)
    }
    pub fn index(&self, name: &str) -> Option<usize> {
        self.projection.index(self.problem, name)
    }
    pub fn slot(&self, point: usize) -> Option<ScalarSlot> {
        self.projection.slot(self.problem, point)
    }
    pub fn metadata(&self, point: usize) -> Option<SolveVariableMeta> {
        self.projection.metadata(self.problem, point)
    }
    pub fn scalar_rows(&self) -> Result<ScalarProgramBlock, VisibleProjectionError> {
        self.projection.scalar_rows(self.problem)
    }

    pub fn scalar_count(&self) -> usize {
        self.projection.scalar_count()
    }
    pub fn inputs(&self) -> &[VisibleProjectionInput] {
        self.projection.inputs()
    }
}

#[derive(Clone, Debug)]
pub struct VisibleProjectionError {
    pub detail: String,
    pub span: Option<Span>,
}

impl std::fmt::Display for VisibleProjectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "visible projection is invalid: {}", self.detail)
    }
}
impl std::error::Error for VisibleProjectionError {}

fn error(detail: impl Into<String>, span: Option<Span>) -> VisibleProjectionError {
    VisibleProjectionError {
        detail: detail.into(),
        span,
    }
}

impl VisibleProjection {
    pub fn construct(
        problem: &SolveProblem,
        members: Vec<VisibleProjectionInput>,
    ) -> Result<Self, VisibleProjectionError> {
        let count = Self::check_inputs(problem, &members)?;
        let owner = Self {
            members,
            scalar_count: count,
        };
        owner.validate_source_facts(problem)?;
        Ok(owner)
    }

    pub fn check_inputs(
        problem: &SolveProblem,
        members: &[VisibleProjectionInput],
    ) -> Result<usize, VisibleProjectionError> {
        let mut count = 0usize;
        let mut previous = None;
        for member in members {
            let extent = check_member(problem, member, &mut previous)?;
            count = count
                .checked_add(extent)
                .ok_or_else(|| error("visible extent overflow", None))?;
        }
        Ok(count)
    }

    pub fn validate_against(&self, problem: &SolveProblem) -> Result<(), VisibleProjectionError> {
        if Self::check_inputs(problem, &self.members)? != self.scalar_count {
            return Err(error("visible owner has foreign logical extent", None));
        }
        self.validate_source_facts(problem)
    }

    pub fn inputs(&self) -> &[VisibleProjectionInput] {
        &self.members
    }
    pub fn scalar_count(&self) -> usize {
        self.scalar_count
    }
}

fn check_member(
    problem: &SolveProblem,
    member: &VisibleProjectionInput,
    previous: &mut Option<usize>,
) -> Result<usize, VisibleProjectionError> {
    match member {
        VisibleProjectionInput::Declaration(source) => {
            let logical = declaration::check_declaration(problem, source)?;
            if previous.is_some_and(|index| index >= source.declaration) {
                return Err(error(
                    "declaration selections are duplicated or reordered",
                    Some(source.attributes.source_span),
                ));
            }
            *previous = Some(source.declaration);
            Ok(logical)
        }
        VisibleProjectionInput::Expression {
            names,
            rows,
            metadata,
        } => {
            check_expression(names, rows, metadata.as_deref())?;
            Ok(names.len())
        }
    }
}

fn check_expression(
    names: &[String],
    rows: &ScalarProgramBlock,
    metadata: Option<&[SolveVariableMeta]>,
) -> Result<(), VisibleProjectionError> {
    if rows.row_count() != names.len()
        || rows.output_count() != names.len()
        || !rows.uses_local_contiguous_output_indices()
        || metadata.is_some_and(|metadata| metadata.len() != names.len())
    {
        return Err(error(
            "authored expression outputs or metadata differ from names",
            None,
        ));
    }
    if metadata.is_some_and(|metadata| {
        names
            .iter()
            .zip(metadata)
            .any(|(name, meta)| *name != meta.name)
    }) {
        return Err(error(
            "authored metadata name differs from expression name",
            None,
        ));
    }
    Ok(())
}

fn validate_fact_ordinals(
    ordinals: impl Iterator<Item = usize>,
    count: usize,
    span: Option<Span>,
) -> Result<(), VisibleProjectionError> {
    let mut previous = None;
    for ordinal in ordinals {
        if ordinal >= count || previous.is_some_and(|index| index >= ordinal) {
            return Err(error(
                "source fact ordinal is outside or reordered in its declaration",
                span,
            ));
        }
        previous = Some(ordinal);
    }
    Ok(())
}

fn storage_point(base: ScalarSlot, ordinal: usize) -> Option<ScalarSlot> {
    match base {
        ScalarSlot::Y { index, .. } => Some(crate::scalar_slot_y(index.checked_add(ordinal)?)),
        ScalarSlot::P { index, .. } => Some(crate::scalar_slot_p(index.checked_add(ordinal)?)),
        _ => None,
    }
}

fn role_name(role: crate::SolveVariableStorageRole) -> &'static str {
    use crate::SolveVariableStorageRole::*;
    match role {
        State => "state",
        Algebraic => "algebraic",
        Output => "output",
        ExternalInput => "input",
        DiscreteReal => "discrete-real",
        DiscreteValue => "discrete-valued",
        Parameter => "parameter",
        Constant => "constant",
    }
}
