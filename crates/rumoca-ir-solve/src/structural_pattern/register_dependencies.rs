//! Source-issued register families. Operand views own the versions they read.
use crate::register_versions::{RegisterVersionInventory, RegisterVersionPiece};
use std::borrow::Cow;
use std::sync::Arc;

use super::{DependencyState, StructuralPatternError};
use crate::Reg;

mod projection;
use projection::Selection;

#[derive(Default)]
pub(crate) struct DependencyRegisters {
    pieces: RegisterVersionInventory<Family>,
}

type Piece = RegisterVersionPiece<Family>;

struct Family {
    kind: FamilyKind,
    /// Issued from the operation rule and captured source witnesses.
    empty: bool,
}

enum FamilyKind {
    Uniform(DependencyState),
    Load {
        lanes: usize,
        primal: Option<usize>,
        tangent: Option<usize>,
    },
    Union {
        lanes: usize,
        terms: Box<[Term]>,
    },
    Transpose {
        source: DependencyView,
        rows: usize,
        columns: usize,
        value_width: usize,
    },
    Product {
        shape: super::MatrixMultiplyShape,
        lhs: DependencyView,
        rhs: DependencyView,
    },
    Call {
        inputs: Box<[CallInput]>,
    },
    Concatenate {
        inputs: Box<[ConcatenateInput]>,
        count: usize,
    },
    Overlay {
        source: DependencyView,
        extra: DependencyState,
    },
    Patch {
        projection: super::tensor_update::FixedUpdateProjection,
        base: Option<DependencyView>,
        patch: Option<DependencyView>,
    },
}

pub(super) struct Term {
    pub source: DependencyView,
    pub stride: usize,
    pub source_lane: usize,
    /// None means every output lane; otherwise only this output lane.
    pub output_lane: Option<usize>,
}

pub(super) enum CallInput {
    Whole(DependencyState),
    Projected {
        source: DependencyView,
        projection: crate::typed_program::CheckedCallDependencyProjection,
    },
}

pub(super) struct ConcatenateInput {
    pub source: DependencyView,
    pub mapping: super::ConcatenateMapping,
    pub count: usize,
}

#[derive(Clone)]
pub(super) struct DependencyView {
    pieces: Vec<(usize, usize, Piece)>,
}

impl DependencyRegisters {
    pub(super) fn set(&mut self, register: Reg, state: DependencyState) {
        self.uniform(register, 1, state);
    }

    pub(super) fn uniform(&mut self, start: Reg, count: usize, state: DependencyState) {
        self.install(start, count, FamilyKind::Uniform(state));
    }

    pub(super) fn load(
        &mut self,
        start: Reg,
        count: usize,
        lanes: usize,
        primal: Option<usize>,
        tangent: Option<usize>,
    ) {
        if primal.is_none() && tangent.is_none() {
            self.uniform(start, count * lanes, DependencyState::Empty);
            return;
        }
        self.install(
            start,
            count * lanes,
            FamilyKind::Load {
                lanes,
                primal,
                tangent,
            },
        );
    }

    pub(super) fn transpose(
        &mut self,
        start: Reg,
        source: DependencyView,
        rows: usize,
        columns: usize,
        value_width: usize,
    ) {
        self.install(
            start,
            rows * columns * value_width,
            FamilyKind::Transpose {
                source,
                rows,
                columns,
                value_width,
            },
        );
    }

    pub(super) fn matrix_product(
        &mut self,
        shape: &super::MatrixMultiplyShape,
        lhs: DependencyView,
        rhs: DependencyView,
    ) {
        self.install(
            shape.dst_start,
            shape.rows * shape.columns * shape.lanes,
            FamilyKind::Product {
                shape: *shape,
                lhs,
                rhs,
            },
        );
    }

    pub(super) fn call_output(&mut self, start: Reg, count: usize, inputs: Vec<CallInput>) {
        self.install(
            start,
            count,
            FamilyKind::Call {
                inputs: inputs.into(),
            },
        );
    }

    pub(super) fn concatenate(&mut self, start: Reg, count: usize, inputs: Vec<ConcatenateInput>) {
        self.install(
            start,
            count,
            FamilyKind::Concatenate {
                inputs: inputs.into(),
                count,
            },
        );
    }

    pub(super) fn overlay(
        &mut self,
        start: Reg,
        count: usize,
        source: DependencyView,
        extra: DependencyState,
    ) {
        self.install(start, count, FamilyKind::Overlay { source, extra });
    }

    pub(super) fn patch(
        &mut self,
        start: Reg,
        count: usize,
        projection: super::tensor_update::FixedUpdateProjection,
        base: Option<DependencyView>,
        patch: Option<DependencyView>,
    ) {
        self.install(
            start,
            count,
            FamilyKind::Patch {
                projection,
                base,
                patch,
            },
        );
    }

    pub(super) fn mapped_union(
        &mut self,
        start: Reg,
        count: usize,
        lanes: usize,
        terms: Vec<Term>,
    ) {
        self.install(
            start,
            count * lanes,
            FamilyKind::Union {
                lanes,
                terms: terms.into(),
            },
        );
    }

    fn install(&mut self, start: Reg, count: usize, kind: FamilyKind) {
        self.pieces.install(
            start,
            count,
            Arc::new(Family {
                empty: kind.source_empty(),
                kind,
            }),
        );
    }

    pub(crate) fn state(&self, register: Reg) -> Option<Cow<'_, DependencyState>> {
        let (&start, piece) = self.pieces.range(..=register).next_back()?;
        (u64::from(register) < piece.end)
            .then(|| {
                piece
                    .family
                    .state(piece.offset + (register - start) as usize)
            })
            .flatten()
    }

    /// Checks every source cell through interval coverage, before issuing a view.
    pub(super) fn view(&self, start: Reg, count: usize) -> Option<DependencyView> {
        Some(DependencyView {
            pieces: self.pieces.view(start, count)?.pieces,
        })
    }

    pub(crate) fn range(&self, start: Reg, count: usize) -> Option<DependencyState> {
        self.view(start, count)?.strided(0, count, 1)
    }

    pub(super) fn first_missing(&self, start: Reg, count: usize) -> Reg {
        self.pieces.first_missing(start, count)
    }

    pub(super) fn strided_view(
        &self,
        start: Reg,
        count: usize,
        stride: usize,
        lanes: usize,
    ) -> Option<DependencyView> {
        Some(DependencyView {
            pieces: self
                .pieces
                .strided_view(start, count, stride, lanes)?
                .pieces,
        })
    }

    #[cfg(test)]
    pub(super) fn family_count(&self) -> usize {
        self.pieces.len()
    }
}

impl DependencyView {
    fn source_empty(&self) -> bool {
        self.pieces.iter().all(|(_, _, piece)| piece.family.empty)
    }
    pub(super) fn strided(
        &self,
        start: usize,
        count: usize,
        stride: usize,
    ) -> Option<DependencyState> {
        projection::project(self.requests(Selection {
            start,
            count,
            stride,
        })?)
    }

    fn requests(&self, selection: Selection) -> Option<Vec<projection::Query<'_>>> {
        if selection.count == 0 {
            return Some(Vec::new());
        }
        if selection.stride == 0 {
            let (low, _, piece) = self
                .pieces
                .iter()
                .find(|(low, high, _)| *low <= selection.start && selection.start < *high)?;
            return Some(vec![projection::Query {
                family: &piece.family,
                selection: Selection {
                    start: piece.offset + selection.start - low,
                    count: 1,
                    stride: 0,
                },
            }]);
        }
        let end = selection
            .start
            .checked_add((selection.count - 1).checked_mul(selection.stride)?)?
            .checked_add(1)?;
        let mut requests = Vec::new();
        let mut covered = 0usize;
        for (low, high, piece) in self
            .pieces
            .iter()
            .filter(|(low, high, _)| *low < end && *high > selection.start)
        {
            let first = low
                .saturating_sub(selection.start)
                .div_ceil(selection.stride);
            let limit = ((*high - selection.start - 1) / selection.stride + 1).min(selection.count);
            if first >= limit {
                continue;
            }
            covered = covered.checked_add(limit - first)?;
            requests.push(projection::Query {
                family: &piece.family,
                selection: Selection {
                    start: piece.offset + selection.start + first * selection.stride - low,
                    count: limit - first,
                    stride: selection.stride,
                },
            });
        }
        (covered == selection.count).then_some(requests)
    }
}

impl FamilyKind {
    fn source_empty(&self) -> bool {
        match self {
            Self::Uniform(state) => matches!(state, DependencyState::Empty),
            Self::Load {
                primal, tangent, ..
            } => primal.is_none() && tangent.is_none(),
            Self::Union { terms, .. } => terms.iter().all(|term| term.source.source_empty()),
            Self::Transpose { source, .. } => source.source_empty(),
            Self::Concatenate { inputs, .. } => {
                inputs.iter().all(|input| input.source.source_empty())
            }
            Self::Overlay { source, extra } => {
                source.source_empty() && matches!(extra, DependencyState::Empty)
            }
            Self::Patch { base, patch, .. } => {
                base.iter().chain(patch).all(DependencyView::source_empty)
            }
            Self::Product { lhs, rhs, shape } => {
                shape.inner == 0 || (lhs.source_empty() && rhs.source_empty())
            }
            Self::Call { inputs } => inputs.iter().all(|input| match input {
                CallInput::Whole(state) => matches!(state, DependencyState::Empty),
                CallInput::Projected { source, .. } => source.source_empty(),
            }),
        }
    }
}

impl Family {
    fn state(&self, index: usize) -> Option<Cow<'_, DependencyState>> {
        match &self.kind {
            FamilyKind::Uniform(state) => Some(Cow::Borrowed(state)),
            _ => Some(Cow::Owned(projection::project(vec![projection::Query {
                family: self,
                selection: Selection {
                    start: index,
                    count: 1,
                    stride: 0,
                },
            }])?)),
        }
    }
}

impl Drop for Family {
    fn drop(&mut self) {
        let mut pending = take_children(std::mem::replace(
            &mut self.kind,
            FamilyKind::Uniform(DependencyState::Empty),
        ));
        while let Some(child) = pending.pop() {
            if let Some(mut owned) = Arc::into_inner(child) {
                pending.extend(take_children(std::mem::replace(
                    &mut owned.kind,
                    FamilyKind::Uniform(DependencyState::Empty),
                )));
            }
        }
    }
}

fn take_children(kind: FamilyKind) -> Vec<Arc<Family>> {
    match kind {
        FamilyKind::Union { terms, .. } => terms
            .into_vec()
            .into_iter()
            .flat_map(|term| {
                term.source
                    .pieces
                    .into_iter()
                    .map(|(_, _, piece)| piece.family)
            })
            .collect(),
        FamilyKind::Uniform(_) | FamilyKind::Load { .. } => Vec::new(),
        FamilyKind::Concatenate { inputs, .. } => inputs
            .into_vec()
            .into_iter()
            .flat_map(|input| {
                input
                    .source
                    .pieces
                    .into_iter()
                    .map(|(_, _, piece)| piece.family)
            })
            .collect(),
        FamilyKind::Overlay { source, .. } => source
            .pieces
            .into_iter()
            .map(|(_, _, piece)| piece.family)
            .collect(),
        FamilyKind::Patch { base, patch, .. } => base
            .into_iter()
            .chain(patch)
            .flat_map(|source| source.pieces.into_iter().map(|(_, _, piece)| piece.family))
            .collect(),
        FamilyKind::Transpose { source, .. } => source
            .pieces
            .into_iter()
            .map(|(_, _, piece)| piece.family)
            .collect(),
        FamilyKind::Product { lhs, rhs, .. } => lhs
            .pieces
            .into_iter()
            .chain(rhs.pieces)
            .map(|(_, _, piece)| piece.family)
            .collect(),
        FamilyKind::Call { inputs } => inputs
            .into_vec()
            .into_iter()
            .flat_map(|input| match input {
                CallInput::Whole(_) => Vec::new(),
                CallInput::Projected { source, .. } => source
                    .pieces
                    .into_iter()
                    .map(|(_, _, piece)| piece.family)
                    .collect(),
            })
            .collect(),
    }
}

pub(super) fn checked_view(
    registers: &DependencyRegisters,
    start: Reg,
    count: usize,
    span: Option<rumoca_core::Span>,
) -> Result<DependencyView, StructuralPatternError> {
    registers
        .view(start, count)
        .ok_or(StructuralPatternError::UninitializedRegister {
            register: registers.first_missing(start, count),
            span,
        })
}
