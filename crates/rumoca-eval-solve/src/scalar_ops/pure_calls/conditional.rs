//! Scalar views of checked selected regions, with private register files.

use super::*;

impl Inliner<'_> {
    pub(super) fn selected_regions(
        &mut self,
        condition: Reg,
        captures: Vec<Val>,
        if_true: &SolveProgramRegion,
        if_false: &SolveProgramRegion,
    ) -> Option<Vec<Val>> {
        if if_true.inputs() != if_false.inputs() || if_true.outputs() != if_false.outputs() {
            return None;
        }
        let types = if_true.outputs();
        if types.iter().any(|value| {
            value.scalar_count() == 0
                || matches!(value.element_type(), solve::SolveScalarType::Integer(_))
        }) {
            return None;
        }
        self.budget
            .reserve(captures.len().checked_add(types.len())?)?;
        let widths = types
            .iter()
            .map(|value| value.scalar_count() as usize)
            .collect::<Vec<_>>();
        // Only runtime elements enter the numeric capture ABI. Exact integer
        // binders and other construction-known elements remain typed constants.
        let capture_count = captures.iter().try_fold(1usize, |count, value| {
            count.checked_add(
                value
                    .elems
                    .iter()
                    .filter(|e| matches!(e, Elem::Reg(_)))
                    .count(),
            )
        })?;
        self.budget.reserve(capture_count)?;
        let taken = self.project_region(if_true, &captures)?;
        let fallback = self.project_region(if_false, &captures)?;
        let predicate = vec![
            LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        self.budget.reserve(predicate.len())?;
        let program = solve::FunctionConditionalProgram::checked(
            capture_count,
            widths,
            [(predicate, taken)],
            fallback,
        )
        .ok()?;
        let capture_start = *self.next;
        self.capture(condition)?;
        for elem in captures.iter().flat_map(|value| value.elems.iter()) {
            if let Elem::Reg(source) = elem {
                self.capture(*source)?;
            }
        }
        let dst_start = *self.next;
        let outputs = types
            .iter()
            .map(|value| {
                self.budget.reserve(
                    (value.scalar_count() as usize).checked_add(value.dimensions().len())?,
                )?;
                let elems = (0..value.scalar_count())
                    .map(|_| self.fresh().map(Elem::Reg))
                    .collect::<Option<Vec<_>>>()?;
                Some(Val {
                    scalar: value.element_type(),
                    dims: Arc::new(value.dimensions().to_vec()),
                    elems: Arc::new(elems),
                })
            })
            .collect::<Option<Vec<_>>>()?;
        self.push(LinearOp::FunctionConditional {
            dst_start,
            capture_start,
            program: Arc::new(program),
        })?;
        Some(outputs)
    }

    fn capture(&mut self, src: Reg) -> Option<()> {
        let dst = self.fresh()?;
        self.push(LinearOp::Move { dst, src })
    }

    /// Project once through the same owner interpreter and cumulative budget;
    /// branch-local registers never escape into the enclosing straight-line ops.
    fn project_region(
        &mut self,
        region: &SolveProgramRegion,
        captures: &[Val],
    ) -> Option<Vec<LinearOp>> {
        let mut ops = Vec::new();
        let mut next = 0;
        let mut child = Inliner {
            table: self.table,
            out: &mut ops,
            next: &mut next,
            depth: self.depth,
            budget: self.budget,
            provenance: Some(region.provenance()),
        };
        let arguments = child.region_captures(captures)?;
        let outputs = child.region(region, arguments)?;
        if outputs.len() != region.outputs().len() {
            return None;
        }
        for (value, expected) in outputs.iter().zip(region.outputs()) {
            if !value.matches(expected) {
                return None;
            }
            for elem in value.elems.iter() {
                let src = child.materialize(*elem)?;
                child.push(LinearOp::StoreOutput { src })?;
            }
        }
        Some(ops)
    }

    fn region_captures(&mut self, captures: &[Val]) -> Option<Vec<Val>> {
        let mut index = 1;
        captures
            .iter()
            .map(|value| {
                self.budget.reserve(value.elems.len())?;
                let elems = value
                    .elems
                    .iter()
                    .map(|elem| match *elem {
                        Elem::Const(_) => Some(*elem),
                        Elem::Reg(_) => {
                            let dst = self.fresh()?;
                            self.push(LinearOp::LoadFunctionConditionalCapture { dst, index })?;
                            index = index.checked_add(1)?;
                            Some(Elem::Reg(dst))
                        }
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(Val {
                    scalar: value.scalar,
                    dims: Arc::clone(&value.dims),
                    elems: Arc::new(elems),
                })
            })
            .collect()
    }
}
