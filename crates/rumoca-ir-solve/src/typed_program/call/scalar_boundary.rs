//! How a pure-call owner's typed values cross its flat binary64 scalar
//! boundary.
//!
//! A caller outside a typed body hands every input and receives every output
//! as consecutive binary64 scalars. Whether a value passes straight through
//! or converts through an owned local, which checks an input conversion
//! makes, and where each output lands in the flat result are decided here,
//! once, from the owner's checked interface; a printer spells the crossings
//! it is handed.

use serde::Serialize;

use super::SolvePureCallOutput;
use crate::typed_program::{SolveDomainCheck, SolveElementToken, SolveValueType};
use crate::{SolveRealFormat, SolveScalarType};

/// How one typed value crosses the scalar boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "crossing", rename_all = "snake_case")]
pub enum SolveScalarCrossing {
    /// A binary64 real value: the caller's scalars are the value itself.
    Direct,
    /// Any other value converts elementwise through an owned local of
    /// `extent` elements. On the way in an integer must be integral and
    /// within `domain` and a Boolean must be 0 or 1; on the way out every
    /// element widens to binary64 and `domain` is absent.
    Converted {
        element: SolveElementToken,
        extent: u32,
        domain: Option<SolveDomainCheck>,
    },
}

/// One input: the caller's scalars for `argument`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveScalarInput {
    argument: usize,
    crossing: SolveScalarCrossing,
}

/// One output: `extent` scalars of output `output` at `offset` in the flat
/// result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveScalarOutput {
    output: usize,
    offset: usize,
    extent: u32,
    crossing: SolveScalarCrossing,
}

/// The issued scalar boundary of one owner interface.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveScalarBoundary {
    inputs: Box<[SolveScalarInput]>,
    outputs: Box<[SolveScalarOutput]>,
    output_scalar_count: usize,
}

/// Why an owner interface has no binary64 scalar boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveScalarBoundaryError {
    /// Input `argument` has an element type no boundary converts.
    UnsupportedInput { argument: usize },
    /// Output `output` has an element type no boundary converts.
    UnsupportedOutput { output: usize },
    /// The flat result length overflows.
    ResultOverflow,
}

impl std::fmt::Display for SolveScalarBoundaryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedInput { argument } => {
                write!(f, "input {argument} has no binary64 scalar crossing")
            }
            Self::UnsupportedOutput { output } => {
                write!(f, "output {output} has no binary64 scalar crossing")
            }
            Self::ResultOverflow => f.write_str("scalar result length overflows"),
        }
    }
}

impl std::error::Error for SolveScalarBoundaryError {}

impl SolveScalarBoundary {
    pub(super) fn of(
        inputs: &[SolveValueType],
        outputs: &[SolvePureCallOutput],
    ) -> Result<Self, SolveScalarBoundaryError> {
        let inputs = inputs
            .iter()
            .enumerate()
            .map(|(argument, value_type)| {
                let crossing = crossing(value_type, true)
                    .ok_or(SolveScalarBoundaryError::UnsupportedInput { argument })?;
                Ok(SolveScalarInput { argument, crossing })
            })
            .collect::<Result<_, _>>()?;
        let mut offset = 0usize;
        let outputs = outputs
            .iter()
            .enumerate()
            .map(|(output, value)| {
                let value_type = value.value_type();
                let crossing = crossing(value_type, false)
                    .ok_or(SolveScalarBoundaryError::UnsupportedOutput { output })?;
                let issued = SolveScalarOutput {
                    output,
                    offset,
                    extent: value_type.scalar_count(),
                    crossing,
                };
                offset = offset
                    .checked_add(value_type.scalar_count() as usize)
                    .ok_or(SolveScalarBoundaryError::ResultOverflow)?;
                Ok(issued)
            })
            .collect::<Result<_, _>>()?;
        Ok(Self {
            inputs,
            outputs,
            output_scalar_count: offset,
        })
    }

    #[must_use]
    pub fn inputs(&self) -> &[SolveScalarInput] {
        &self.inputs
    }

    #[must_use]
    pub fn outputs(&self) -> &[SolveScalarOutput] {
        &self.outputs
    }

    /// The length of the flat result.
    #[must_use]
    pub const fn output_scalar_count(&self) -> usize {
        self.output_scalar_count
    }
}

impl SolveScalarInput {
    #[must_use]
    pub const fn argument(&self) -> usize {
        self.argument
    }

    #[must_use]
    pub const fn crossing(&self) -> SolveScalarCrossing {
        self.crossing
    }
}

impl SolveScalarOutput {
    #[must_use]
    pub const fn output(&self) -> usize {
        self.output
    }

    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub const fn extent(&self) -> u32 {
        self.extent
    }

    #[must_use]
    pub const fn crossing(&self) -> SolveScalarCrossing {
        self.crossing
    }
}

fn crossing(value_type: &SolveValueType, input: bool) -> Option<SolveScalarCrossing> {
    let element = match value_type.element_type() {
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        } => return Some(SolveScalarCrossing::Direct),
        SolveScalarType::Real {
            format: SolveRealFormat::Binary32,
        } => return None,
        SolveScalarType::Integer(_) | SolveScalarType::Boolean => SolveElementToken::of(value_type),
    };
    Some(SolveScalarCrossing::Converted {
        element,
        extent: value_type.scalar_count(),
        domain: if input {
            SolveDomainCheck::of(value_type)
        } else {
            None
        },
    })
}
