//! Definedness of compact function loops from one generic iteration.
//!
//! MLS 3.6 §11.2.2 runs a `for` body once per element of its range and
//! §12.4.4 leaves a function value undefined until a statement assigns it.
//! Instead of resolving the body once per domain point, the definedness proof
//! resolves one *generic* iteration: each binder stands for every value of its
//! range, narrowed on a path only by conditions that select exactly a binder
//! range (`i <= 2`, `i == 1`). An element write `y[a*i + b]` is recorded with
//! the binder values for which it certainly runs, and its image over those
//! values is the set of elements the loop defines. A read is admitted when its
//! image is already defined, was written earlier in the same iteration on
//! every path, or was written by an earlier iteration (`y[i - 1]` after
//! `y[i]`, where the binder values that have an earlier iteration are known).
//!
//! The body is resolved twice: the first round learns what every iteration
//! certainly defines by its end (reads are not judged), the second judges
//! every read against that summary. The cost depends on the body, not on the
//! extent of the domain.

use super::function_definitions::{generated_boolean_value, reads_only_immutable};
use super::*;
use rumoca_core::{AffineForm, IndexAxis, IndexBox, IndexUnion, Progression, SetAnswer, image};
use std::rc::Rc;

/// The compact loops the current resolution is inside, outermost first.
#[derive(Clone, Default)]
pub(super) struct FoldScopes {
    scopes: Vec<FoldScope>,
    /// Rounds that learn a summary and must not judge reads.
    learning: usize,
}

#[derive(Clone)]
struct FoldScope {
    binders: Vec<FoldBinder>,
    /// The binder values of every scope (outer scopes first) that reach the
    /// current path.
    region: IndexBox,
    /// Per region coordinate, the origin `L(p)` of a binder re-based on its
    /// offset within a dependent range: the binder's value is `L(p) + t`.
    origins: Vec<Option<AffineForm>>,
    /// Whether every value in `region` reaches the current path: each guard
    /// on the path selects exactly a binder range.
    exact: bool,
    /// Element writes the current iteration certainly made so far.
    writes: Vec<SymbolicWrite>,
    /// What every iteration certainly defines by its end, from the learning
    /// round; absent while learning.
    previous: Option<Rc<IterationSummary>>,
}

#[derive(Clone)]
struct FoldBinder {
    name: VarName,
    /// The binder's values, and whether iteration visits them descending.
    domain: Progression,
    descending: bool,
}

#[derive(Clone)]
struct SymbolicWrite {
    target: VarName,
    axes: Vec<IndexAxis>,
    /// Binder values (one coordinate per binder of every scope up to the
    /// writing scope) at which the write certainly runs.
    region: IndexUnion,
    /// Whether the write runs at every binder value of `region`. A write on a
    /// path that only some executions take (a runtime guard no binder range
    /// selects) is read-visible on that path alone and leaves no definition.
    certain: bool,
}

/// The certain state at the end of one iteration of a scope.
#[derive(Default)]
pub(super) struct IterationSummary {
    writes: Vec<SymbolicWrite>,
    defined: HashSet<VarName>,
}

/// What a symbolic subscript list resolves to.
pub(super) enum SymbolicIndices {
    /// No active binder is read: the ordinary concrete index proof applies.
    Concrete,
    /// Some subscript is not an affine image the proof can represent.
    Unknown,
    Affine(Vec<IndexAxis>),
}

impl FoldScopes {
    /// Whether a loop is being resolved generically.
    pub(super) fn is_active(&self) -> bool {
        !self.scopes.is_empty()
    }

    pub(super) fn is_learning(&self) -> bool {
        self.learning > 0
    }

    /// Enter a loop over `binders` (`(name, values, descending)`).
    pub(super) fn enter(&mut self, binders: Vec<(VarName, Progression, bool)>) {
        let outer = self
            .scopes
            .last()
            .map(|scope| (scope.region.0.clone(), scope.origins.clone(), scope.exact));
        let (mut region, mut origins, exact) = outer.unwrap_or((Vec::new(), Vec::new(), true));
        origins.extend(binders.iter().map(|_| None));
        region.extend(binders.iter().map(|(_, values, _)| *values));
        self.scopes.push(FoldScope {
            binders: binders
                .into_iter()
                .map(|(name, domain, descending)| FoldBinder {
                    name,
                    domain,
                    descending,
                })
                .collect(),
            region: IndexBox(region),
            origins,
            exact,
            writes: Vec::new(),
            previous: None,
        });
    }

    /// Start one round of the innermost scope.
    pub(super) fn begin_round(&mut self, previous: Option<Rc<IterationSummary>>) {
        if previous.is_none() {
            self.learning += 1;
        }
        let scope = self.scopes.last_mut().expect("a round runs inside a scope");
        scope.writes.clear();
        scope.previous = previous;
    }

    /// The certain end state of the innermost scope's iteration.
    /// Iteration-local values start every iteration undefined, so nothing an
    /// earlier iteration did to them is carried into the summary.
    pub(super) fn end_round(
        &mut self,
        mut defined: HashSet<VarName>,
        iteration_locals: &[VarName],
    ) -> Rc<IterationSummary> {
        let scope = self.scopes.last_mut().expect("a round runs inside a scope");
        if scope.previous.is_none() {
            self.learning -= 1;
        }
        defined.retain(|name| !iteration_locals.contains(name));
        Rc::new(IterationSummary {
            writes: scope
                .writes
                .iter()
                .filter(|write| write.certain && !iteration_locals.contains(&write.target))
                .cloned()
                .collect(),
            defined,
        })
    }

    /// Leave the innermost scope; return each element set its iteration wrote
    /// that no outer binder indexes, so the loop defines it. A write whose
    /// subscripts still read outer binders becomes a write of the enclosing
    /// iteration, its inner subscripts replaced by their images.
    pub(super) fn leave(&mut self) -> Vec<(VarName, IndexUnion)> {
        let scope = self.scopes.pop().expect("leave matches enter");
        let inner_start = scope.region.0.len() - scope.binders.len();
        let mut realized = Vec::new();
        let mut outer = Vec::new();
        let results = scope
            .writes
            .iter()
            .filter(|write| write.certain)
            .flat_map(|write| {
                write.region.boxes().iter().filter_map(move |part| {
                    realize_write(write, part, inner_start).map(|done| (write, done))
                })
            });
        for (write, done) in results {
            match done {
                Realized::Defined(set) => realized.push((write.target.clone(), set)),
                Realized::Outer(write) => outer.push(write),
            }
        }
        if let Some(parent) = self.scopes.last_mut() {
            parent.writes.extend(outer);
        }
        realized
    }

    /// The scope-local writes the current state has beyond `before` writes,
    /// for joining branches.
    pub(super) fn write_count(&self) -> usize {
        self.scopes.last().map_or(0, |scope| scope.writes.len())
    }

    /// Keep, after a conditional, the writes each branch made on an exact path:
    /// each runs at its own branch's binder values.
    pub(super) fn absorb_branch_writes(&mut self, before: usize, branches: &[&Self]) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };
        for branch in branches {
            if let Some(branch_scope) = branch.scopes.last() {
                scope.writes.extend(
                    branch_scope
                        .writes
                        .iter()
                        .skip(before)
                        .filter(|write| write.certain)
                        .cloned(),
                );
            }
        }
    }

    /// Take the path `remaining` reaches, narrowed by `condition` holding.
    pub(super) fn assume_from(
        &mut self,
        remaining: &Self,
        condition: &Expression,
        context: FunctionValidationContext<'_>,
    ) -> bool {
        *self = remaining.clone();
        self.assume(condition, true, context)
    }

    /// Whether the current path is selected exactly by binder ranges, so every
    /// binder value of its region reaches it.
    pub(super) fn path_is_exact(&self) -> bool {
        self.scopes.last().is_some_and(|scope| scope.exact)
    }

    /// Whether some binder value reaches the current path.
    pub(super) fn reaches(&self) -> bool {
        self.scopes
            .last()
            .is_none_or(|scope| !scope.region.is_empty())
    }

    /// Narrow the current path by `condition` having `value`. Returns `false`
    /// when no binder value reaches the path (the branch never runs).
    pub(super) fn assume(
        &mut self,
        condition: &Expression,
        value: bool,
        context: FunctionValidationContext<'_>,
    ) -> bool {
        let Some(scope) = self.scopes.last() else {
            return true;
        };
        let narrowed = match self.binder_constraint(scope, condition, value, context) {
            Some(constraint) => scope
                .region
                .intersect(&IndexBox(constraint))
                .map(|region| (region, scope.origins.clone())),
            None if value => self.dependent_membership(scope, condition, context),
            None => None,
        };
        let Some(scope) = self.scopes.last_mut() else {
            return true;
        };
        match narrowed {
            Some((region, origins)) => {
                scope.region = region;
                scope.origins = origins;
            }
            None => scope.exact = false,
        }
        !scope.region.is_empty()
    }

    /// A conjunction of settled binder ranges and dependent memberships
    /// `L(p) <= b and b <= L(p) + w` (a dependent range rectangularized over
    /// its envelope): within the branch `b` runs over `L(p) + t` for `t` in
    /// `0:w`, so its coordinate is re-based on `t` with origin `L`.
    fn dependent_membership(
        &self,
        scope: &FoldScope,
        condition: &Expression,
        context: FunctionValidationContext<'_>,
    ) -> Option<(IndexBox, Vec<Option<AffineForm>>)> {
        let condition = selection_value(condition, context);
        let mut terms = Vec::new();
        conjuncts(condition, &mut terms);
        let mut region = scope.region.clone();
        let mut origins = scope.origins.clone();
        let mut lower = vec![None; region.0.len()];
        let mut upper = vec![None; region.0.len()];
        for term in terms {
            if let Some(constraint) = self.binder_constraint(scope, term, true, context) {
                region = region.intersect(&IndexBox(constraint))?;
                continue;
            }
            let Expression::Binary { op, lhs, rhs, .. } = term else {
                return None;
            };
            let relation = Relation::of(op)?;
            let (coordinate, relation, bound) = match self.binder_coordinate(lhs) {
                Some(coordinate) => (coordinate, relation, rhs),
                None => (self.binder_coordinate(rhs)?, relation.mirrored(), lhs),
            };
            // Integer binders: `b > f` is `b >= f + 1` and `b < f` is `b <= f - 1`.
            let (side, shift) = match relation {
                Relation::GreaterEqual => (&mut lower, 0),
                Relation::Greater => (&mut lower, 1),
                Relation::LessEqual => (&mut upper, 0),
                Relation::Less => (&mut upper, -1),
                Relation::Equal | Relation::NotEqual => return None,
            };
            let mut form = self.affine(bound, context, None)?;
            form.coeffs.truncate(region.0.len());
            form.constant = form.constant.checked_add(shift)?;
            // A bound may read only binders outside the one it bounds, and a
            // second bound on the same side would be a minimum or maximum.
            if form.coeffs[coordinate..].iter().any(|scale| *scale != 0)
                || side[coordinate].is_some()
            {
                return None;
            }
            side[coordinate] = Some(form);
        }
        for coordinate in 0..region.0.len() {
            let (low, high) = match (&lower[coordinate], &upper[coordinate]) {
                (Some(low), Some(high)) => (low, high),
                (None, None) => continue,
                // A one-sided membership is not a re-based range.
                _ => return None,
            };
            if low.coeffs != high.coeffs || origins[coordinate].is_some() {
                return None;
            }
            let width = high.constant.checked_sub(low.constant)?;
            // Every re-based value must lie in the binder's current values.
            let reach = image(
                &[IndexAxis {
                    form: low.clone(),
                    offsets: vec![Progression::range(0, 1, width)?],
                }],
                &region,
            )?
            .bound();
            let values = IndexUnion::of([IndexBox(vec![region.0[coordinate]])]);
            if region.0[coordinate].step() != 1 || !values.contains_set(&reach) {
                return None;
            }
            region.0[coordinate] = Progression::range(0, 1, width)?;
            origins[coordinate] = Some(low.clone());
        }
        Some((region, origins))
    }

    /// The binder region `condition == value` selects, when it is exactly a
    /// conjunction of binder ranges.
    fn binder_constraint(
        &self,
        scope: &FoldScope,
        condition: &Expression,
        value: bool,
        context: FunctionValidationContext<'_>,
    ) -> Option<Vec<Progression>> {
        let condition = selection_value(condition, context);
        let unconstrained = scope.region.0.clone();
        match condition {
            Expression::Unary {
                op: OpUnary::Not,
                rhs,
                ..
            } => self.binder_constraint(scope, rhs, !value, context),
            Expression::Binary {
                op: OpBinary::And,
                lhs,
                rhs,
                ..
            } if value => {
                let lhs = self.binder_constraint(scope, lhs, true, context)?;
                let rhs = self.binder_constraint(scope, rhs, true, context)?;
                IndexBox(lhs).intersect(&IndexBox(rhs)).map(|both| both.0)
            }
            Expression::Binary {
                op: OpBinary::Or,
                lhs,
                rhs,
                ..
            } if !value => {
                let lhs = self.binder_constraint(scope, lhs, false, context)?;
                let rhs = self.binder_constraint(scope, rhs, false, context)?;
                IndexBox(lhs).intersect(&IndexBox(rhs)).map(|both| both.0)
            }
            Expression::Binary { op, lhs, rhs, .. } => {
                let (coordinate, relation, bound) = self.binder_relation(op, lhs, rhs, context)?;
                let relation = if value { relation } else { relation.negated() };
                let current = unconstrained[coordinate];
                let restricted = relation.restrict(current, bound)?;
                let mut constraint = unconstrained;
                constraint[coordinate] = restricted;
                Some(constraint)
            }
            _ => None,
        }
    }

    /// `binder <relation> bound` with a settled bound, from either side.
    fn binder_relation(
        &self,
        op: &OpBinary,
        lhs: &Expression,
        rhs: &Expression,
        context: FunctionValidationContext<'_>,
    ) -> Option<(usize, Relation, i64)> {
        let relation = Relation::of(op)?;
        if let Some(coordinate) = self.binder_coordinate(lhs)
            && let Some(bound) = settled(rhs, context)
        {
            return Some((coordinate, relation, bound));
        }
        let coordinate = self.binder_coordinate(rhs)?;
        Some((coordinate, relation.mirrored(), settled(lhs, context)?))
    }

    fn binder_coordinate(&self, expression: &Expression) -> Option<usize> {
        let Expression::VarRef {
            name, subscripts, ..
        } = expression
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        // A re-based binder is no longer its region coordinate.
        self.coordinate_of(name.var_name())
            .filter(|coordinate| self.origin(*coordinate).is_none())
    }

    /// The region coordinate of the innermost binder named `name`.
    fn coordinate_of(&self, name: &VarName) -> Option<usize> {
        self.binders()
            .filter(|(_, _, binder)| &binder.name == name)
            .map(|(coordinate, _, _)| coordinate)
            .last()
    }

    /// Every binder with its region coordinate and owning scope, outermost
    /// first.
    fn binders(&self) -> impl Iterator<Item = (usize, &FoldScope, &FoldBinder)> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.binders.iter().map(move |binder| (scope, binder)))
            .enumerate()
            .map(|(coordinate, (scope, binder))| (coordinate, scope, binder))
    }

    /// Whether an iteration of an enclosing loop writes elements of `name`.
    pub(super) fn writes(&self, name: &VarName) -> bool {
        self.scopes.iter().any(|scope| {
            scope.writes.iter().any(|write| &write.target == name)
                || scope.previous.as_ref().is_some_and(|previous| {
                    previous.writes.iter().any(|write| &write.target == name)
                })
        })
    }

    /// The origin of a re-based binder coordinate on the current path.
    fn origin(&self, coordinate: usize) -> Option<AffineForm> {
        self.scopes.last()?.origins.get(coordinate)?.clone()
    }

    pub(super) fn is_binder(&self, name: &VarName) -> bool {
        self.coordinate_of(name).is_some()
    }

    /// The affine axes of `subscripts` over `extents`; `concrete` names the
    /// indices of a subscript that reads no binder.
    pub(super) fn indices(
        &self,
        subscripts: &[Subscript],
        extents: &[i64],
        context: FunctionValidationContext<'_>,
        concrete: &dyn Fn(&Expression) -> Option<Vec<Progression>>,
    ) -> SymbolicIndices {
        if self.scopes.is_empty()
            || subscripts.len() > extents.len()
            || !self.reads_binder(subscripts)
        {
            return SymbolicIndices::Concrete;
        }
        let coordinates = self.coordinate_count();
        let mut axes = Vec::with_capacity(extents.len());
        for (ordinal, extent) in extents.iter().enumerate() {
            let fixed = |offsets: Vec<Progression>| IndexAxis {
                form: AffineForm::constant(0, coordinates),
                offsets,
            };
            let axis = match subscripts.get(ordinal) {
                None | Some(Subscript::Colon { .. }) => {
                    Progression::range(1, 1, *extent).map(|whole| fixed(vec![whole]))
                }
                Some(Subscript::Index { value, .. }) => {
                    Some(fixed(vec![Progression::single(*value)]))
                }
                Some(Subscript::Expr { expr, .. }) if self.reads_binder_expression(expr) => {
                    self.expression_axis(expr, context)
                }
                Some(Subscript::Expr { expr, .. }) => concrete(expr).map(fixed),
            };
            let Some(axis) = axis else {
                return SymbolicIndices::Unknown;
            };
            axes.push(axis);
        }
        SymbolicIndices::Affine(axes)
    }

    fn coordinate_count(&self) -> usize {
        self.scopes.last().map_or(0, |scope| scope.region.0.len())
    }

    fn reads_binder(&self, subscripts: &[Subscript]) -> bool {
        subscripts.iter().any(|subscript| match subscript {
            Subscript::Expr { expr, .. } => self.reads_binder_expression(expr),
            Subscript::Index { .. } | Subscript::Colon { .. } => false,
        })
    }

    fn reads_binder_expression(&self, expression: &Expression) -> bool {
        let mut references = Vec::new();
        expression.collect_var_refs(&mut references);
        references.iter().any(|reference| self.is_binder(reference))
    }

    /// An affine subscript, a range `a(p) + b1 : s : a(p) + b2` with one form
    /// in both bounds, or a comprehension `{a(p) + c * k + b for k in r}` over
    /// a settled range (MLS §10.4.1, §10.4.2.2).
    fn expression_axis(
        &self,
        expression: &Expression,
        context: FunctionValidationContext<'_>,
    ) -> Option<IndexAxis> {
        let coordinates = self.coordinate_count();
        let with_offsets = |mut form: AffineForm, offsets: Progression| {
            let constant = std::mem::take(&mut form.constant);
            offsets.affine_image(1, constant).map(|offsets| IndexAxis {
                form,
                offsets: vec![offsets],
            })
        };
        match expression {
            Expression::ArrayComprehension {
                expr,
                indices,
                filter: None,
                ..
            } => {
                let [index] = indices.as_slice() else {
                    return None;
                };
                let range = settled_range(&index.range, context)?;
                let iterator = VarName::new(&index.name);
                let mut body = self.affine(expr, context, Some(&iterator))?;
                // The iterator occupies the coordinate after every binder.
                let scale = body.coeffs.pop()?;
                with_offsets(body, range.affine_image(scale, 0)?)
            }
            Expression::Range {
                start, step, end, ..
            } => {
                let start = self.affine(start, context, None)?;
                let end = self.affine(end, context, None)?;
                let step = match step {
                    Some(step) => settled(step, context)?,
                    None => 1,
                };
                if start.coeffs != end.coeffs {
                    return None;
                }
                let offsets =
                    Progression::range(0, step, end.constant.checked_sub(start.constant)?)?;
                let mut form = start;
                form.coeffs.truncate(coordinates);
                with_offsets(form, offsets)
            }
            _ => {
                let mut form = self.affine(expression, context, None)?;
                form.coeffs.truncate(coordinates);
                with_offsets(form, Progression::single(0))
            }
        }
    }

    /// `expression` as an affine form over every binder coordinate and, after
    /// them, the one comprehension iterator `local`.
    fn affine(
        &self,
        expression: &Expression,
        context: FunctionValidationContext<'_>,
        local: Option<&VarName>,
    ) -> Option<AffineForm> {
        let width = self.coordinate_count() + 1;
        if let Some(value) = settled(expression, context) {
            return Some(AffineForm::constant(value, width));
        }
        match expression {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                if local == Some(name.var_name()) {
                    return Some(AffineForm::unit_binder(width - 1, width));
                }
                let coordinate = self.coordinate_of(name.var_name())?;
                let unit = AffineForm::unit_binder(coordinate, width);
                match self.origin(coordinate) {
                    Some(mut origin) => {
                        origin.coeffs.resize(width, 0);
                        unit.checked_add(&origin)
                    }
                    None => Some(unit),
                }
            }
            Expression::Unary { op, rhs, .. } => match op {
                OpUnary::Plus => self.affine(rhs, context, local),
                OpUnary::Minus => self.affine(rhs, context, local)?.checked_neg(),
                _ => None,
            },
            Expression::Binary { op, lhs, rhs, .. } => {
                let lhs = self.affine(lhs, context, local)?;
                let rhs = self.affine(rhs, context, local)?;
                match op {
                    OpBinary::Add | OpBinary::AddElem => lhs.checked_add(&rhs),
                    OpBinary::Sub | OpBinary::SubElem => lhs.checked_add(&rhs.checked_neg()?),
                    OpBinary::Mul | OpBinary::MulElem if lhs.is_binder_free() => {
                        rhs.checked_scale(lhs.constant)
                    }
                    OpBinary::Mul | OpBinary::MulElem if rhs.is_binder_free() => {
                        lhs.checked_scale(rhs.constant)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The elements `axes` may name at a binder value reaching the current
    /// path (exact or a superset).
    fn read_image(&self, axes: &[IndexAxis]) -> Option<IndexUnion> {
        let scope = self.scopes.last()?;
        image(axes, &scope.region).map(SetAnswer::bound)
    }

    /// Whether every element `axes` can name on the current path lies within
    /// `extents`.
    pub(super) fn image_within(&self, axes: &[IndexAxis], extents: &[i64]) -> bool {
        self.read_image(axes)
            .is_some_and(|image| image.within(extents))
    }

    /// Record an element write on the current path. A write whose image is
    /// not exactly a union of boxes (a diagonal) is not recorded.
    pub(super) fn record_write(&mut self, target: &VarName, axes: Vec<IndexAxis>) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };
        let exact = matches!(image(&axes, &scope.region), Some(SetAnswer::Exact(_)));
        if scope.region.is_empty() || !exact {
            return;
        }
        scope.writes.push(SymbolicWrite {
            target: target.clone(),
            axes,
            region: IndexUnion::of([scope.region.clone()]),
            certain: scope.exact,
        });
    }

    /// Whether every element `axes` name on the current path is defined: at
    /// each binder value, by `covered`, by an earlier write of this
    /// iteration, or by an earlier iteration.
    pub(super) fn elements_defined(
        &self,
        target: &VarName,
        axes: &[IndexAxis],
        covered: Option<&IndexUnion>,
    ) -> bool {
        let Some(scope) = self.scopes.last() else {
            return false;
        };
        if scope.region.is_empty() {
            return true;
        }
        let empty = IndexUnion::default();
        let covered = covered.unwrap_or(&empty);
        if self
            .read_image(axes)
            .is_some_and(|image| covered.contains_set(&image))
        {
            return true;
        }
        let Some(parts) = single_offset_parts(axes, false) else {
            return false;
        };
        let region = IndexUnion::of([scope.region.clone()]);
        parts.iter().all(|part| {
            let defined = self.points_defining(target, part, &region);
            region
                .difference(&defined)
                .and_then(|rest| image_union(part, &rest))
                .is_some_and(|image| covered.contains_set(&image))
        })
    }

    /// The binder values in `region` at which a single-offset read `axes` is
    /// defined by a write of this iteration or of an earlier one.
    fn points_defining(
        &self,
        target: &VarName,
        axes: &[IndexAxis],
        region: &IndexUnion,
    ) -> IndexUnion {
        let Some(scope) = self.scopes.last() else {
            return IndexUnion::default();
        };
        let mut defined = IndexUnion::default();
        for write in scope.writes.iter().filter(|write| &write.target == target) {
            if binder_shifts(axes, &write.axes, None).contains(&0) {
                defined.extend(write.region.clone());
            }
        }
        for owner in self.scopes.iter().rev().skip(1) {
            defined.extend(enclosing_writes_defining(owner, scope, target, axes));
        }
        for (coordinate, owner, binder) in self.binders() {
            let Some(previous) = &owner.previous else {
                continue;
            };
            for write in previous
                .writes
                .iter()
                .filter(|write| &write.target == target)
            {
                defined.extend(earlier_points(axes, write, (coordinate, binder), region));
            }
        }
        defined
    }

    /// Whether a whole value undefined at loop entry was defined by the end of
    /// every earlier iteration that the current path's binder values have.
    pub(super) fn defined_by_earlier_iteration_whole(&self, name: &VarName) -> bool {
        let Some(scope) = self.scopes.last() else {
            return false;
        };
        let mut start = scope.region.0.len();
        for owner in self.scopes.iter().rev() {
            start -= owner.binders.len();
            let Some(previous) = &owner.previous else {
                continue;
            };
            if previous.defined.contains(name) && self.excludes_first_point(owner, start) {
                return true;
            }
        }
        false
    }

    /// Whether no binder value on the current path is the first iteration of
    /// `owner` (whose binders start at region coordinate `start`).
    fn excludes_first_point(&self, owner: &FoldScope, start: usize) -> bool {
        let Some(scope) = self.scopes.last() else {
            return false;
        };
        owner.binders.iter().enumerate().any(|(ordinal, binder)| {
            if scope.origins[start + ordinal].is_some() {
                return false;
            }
            let first = if binder.descending {
                binder.domain.last()
            } else {
                binder.domain.first()
            };
            let reached = scope.region.0[start + ordinal];
            first.is_some_and(|first| {
                reached
                    .intersect(Progression::single(first))
                    .is_some_and(Progression::is_empty)
            })
        })
    }
}

/// `axes` as subscripts of the enclosing iteration: each axis keeps its terms
/// in the coordinates before `from` and its constant, and its terms in the
/// inner coordinates join its offsets as their exact image over `region`
/// (the box is a product, so every outer value sees the whole inner image).
/// `None` when an inner image has no exact progression description.
fn inner_images(axes: &[IndexAxis], region: &IndexBox, from: usize) -> Option<Vec<IndexAxis>> {
    axes.iter()
        .map(|axis| {
            let mut outer = axis.clone();
            outer.form.coeffs.truncate(from);
            if axis.form.coeffs[from..].iter().all(|scale| *scale == 0) {
                return Some(outer);
            }
            let mut inner = axis.clone();
            inner.form.constant = 0;
            inner.form.coeffs[..from].fill(0);
            let set = image(std::slice::from_ref(&inner), region)?.exact()?;
            outer.offsets = set.boxes().iter().map(|part| part.0[0]).collect();
            Some(outer)
        })
        .collect()
}

/// The read split into one read per offset of every binder axis, and of
/// every fixed axis too when `split_fixed` (a read whose elements may come
/// from different writes).
fn single_offset_parts(axes: &[IndexAxis], split_fixed: bool) -> Option<Vec<Vec<IndexAxis>>> {
    const PART_LIMIT: usize = 64;
    let mut parts = vec![Vec::new()];
    for axis in axes {
        let choices = if axis.form.is_binder_free() && !split_fixed {
            vec![axis.clone()]
        } else {
            offset_values(&axis.offsets, PART_LIMIT)?
                .into_iter()
                .map(|offset| IndexAxis {
                    form: axis.form.clone(),
                    offsets: vec![Progression::single(offset)],
                })
                .collect()
        };
        let mut extended = Vec::new();
        for prefix in &parts {
            for choice in &choices {
                let mut next: Vec<IndexAxis> = prefix.clone();
                next.push(choice.clone());
                extended.push(next);
            }
        }
        if extended.len() > PART_LIMIT {
            return None;
        }
        parts = extended;
    }
    Some(parts)
}

/// The offset values of an axis with at most `limit` of them.
fn offset_values(offsets: &[Progression], limit: usize) -> Option<Vec<i64>> {
    let mut values = Vec::new();
    for part in offsets {
        let (Some(first), Some(last)) = (part.first(), part.last()) else {
            continue;
        };
        let mut value = first;
        while value <= last {
            values.push(value);
            if values.len() > limit {
                return None;
            }
            value = value.checked_add(part.step())?;
        }
    }
    Some(values)
}

/// The shifts `d` along `coordinate` (or none, `d = 0`, without one) with
/// `read(p) ⊆ write(p + d)` for a single-offset read.
fn binder_shifts(read: &[IndexAxis], write: &[IndexAxis], coordinate: Option<usize>) -> Vec<i64> {
    if read.len() != write.len() {
        return Vec::new();
    }
    let mut shifts: Option<Vec<i64>> = None;
    for (read, write) in read.iter().zip(write) {
        if read.form.coeffs != write.form.coeffs {
            return Vec::new();
        }
        let scale = coordinate.map_or(0, |coordinate| read.form.coeffs[coordinate]);
        if scale == 0 {
            // The axis does not move with the shift: the write's indices must
            // contain the read's at the same binder values.
            if !offsets_contained(&write.offsets, &read.offsets) {
                return Vec::new();
            }
            continue;
        }
        let Some(read_offset) = read.offsets.first().and_then(|offsets| offsets.first()) else {
            return Vec::new();
        };
        let candidates = offset_values(&write.offsets, 64)
            .unwrap_or_default()
            .into_iter()
            .filter_map(|write_offset| {
                let difference = read_offset - write_offset;
                (difference % scale == 0).then(|| difference / scale)
            })
            .filter(|shift| *shift != 0)
            .collect::<Vec<_>>();
        if shifts.is_some() {
            return Vec::new();
        }
        shifts = Some(candidates);
    }
    shifts.unwrap_or_else(|| {
        if coordinate.is_none() {
            vec![0]
        } else {
            Vec::new()
        }
    })
}

fn offsets_contained(write: &[Progression], read: &[Progression]) -> bool {
    let written = IndexUnion::of(write.iter().map(|axis| IndexBox(vec![*axis])));
    read.iter()
        .all(|axis| written.contains_box(&IndexBox(vec![*axis])))
}

/// The elements `axes` may name over every binder value in `points`.
fn image_union(axes: &[IndexAxis], points: &IndexUnion) -> Option<IndexUnion> {
    let mut union = IndexUnion::default();
    for part in points.boxes() {
        union.extend(image(axes, part)?.bound());
    }
    Some(union)
}

fn settled_range(
    range: &Expression,
    context: FunctionValidationContext<'_>,
) -> Option<Progression> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return None;
    };
    let step = match step {
        Some(step) => settled(step, context)?,
        None => 1,
    };
    Progression::range(settled(start, context)?, step, settled(end, context)?)
}

fn settled(expression: &Expression, context: FunctionValidationContext<'_>) -> Option<i64> {
    static_shape_integer_expression(expression, context.static_integers, context.shapes)
        .ok()
        .flatten()
}

/// `binder <relation> bound`.
#[derive(Clone, Copy)]
enum Relation {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Relation {
    fn of(op: &OpBinary) -> Option<Self> {
        Some(match op {
            OpBinary::Lt => Self::Less,
            OpBinary::Le => Self::LessEqual,
            OpBinary::Gt => Self::Greater,
            OpBinary::Ge => Self::GreaterEqual,
            OpBinary::Eq => Self::Equal,
            OpBinary::Neq => Self::NotEqual,
            _ => return None,
        })
    }

    fn negated(self) -> Self {
        match self {
            Self::Less => Self::GreaterEqual,
            Self::LessEqual => Self::Greater,
            Self::Greater => Self::LessEqual,
            Self::GreaterEqual => Self::Less,
            Self::Equal => Self::NotEqual,
            Self::NotEqual => Self::Equal,
        }
    }

    fn mirrored(self) -> Self {
        match self {
            Self::Less => Self::Greater,
            Self::LessEqual => Self::GreaterEqual,
            Self::Greater => Self::Less,
            Self::GreaterEqual => Self::LessEqual,
            other => other,
        }
    }

    /// The values of `values` that satisfy the relation with `bound`, when
    /// they form one progression.
    fn restrict(self, values: Progression, bound: i64) -> Option<Progression> {
        let (Some(first), Some(last)) = (values.first(), values.last()) else {
            return Some(Progression::EMPTY);
        };
        let window = |lower: i64, upper: i64| {
            if lower > upper {
                return Some(Progression::EMPTY);
            }
            values.intersect(Progression::range(lower, 1, upper)?)
        };
        match self {
            Self::Less => window(first, bound.checked_sub(1)?),
            Self::LessEqual => window(first, bound),
            Self::Greater => window(bound.checked_add(1)?, last),
            Self::GreaterEqual => window(bound, last),
            Self::Equal => window(bound, bound),
            // Excluding one value leaves one progression only at an end.
            Self::NotEqual if bound == first => window(first + 1, last),
            Self::NotEqual if bound == last => window(first, last - 1),
            Self::NotEqual if bound < first || bound > last => Some(values),
            Self::NotEqual => None,
        }
    }
}

/// What one write box of a finished loop becomes.
enum Realized {
    /// Elements the loop defines.
    Defined(IndexUnion),
    /// A write of the enclosing iteration, still indexed by its binders.
    Outer(SymbolicWrite),
}

fn realize_write(write: &SymbolicWrite, part: &IndexBox, inner_start: usize) -> Option<Realized> {
    let axes = inner_images(&write.axes, part, inner_start)?;
    if axes.iter().all(|axis| axis.form.is_binder_free()) {
        return image(&axes, part)
            .and_then(SetAnswer::exact)
            .map(Realized::Defined);
    }
    Some(Realized::Outer(SymbolicWrite {
        target: write.target.clone(),
        axes,
        region: IndexUnion::of([IndexBox(part.0[..inner_start].to_vec())]),
        certain: true,
    }))
}

/// The binder values of `region` whose read `axes` an earlier iteration
/// (along `binder`, at `coordinate`) defined through `write`.
fn earlier_points(
    axes: &[IndexAxis],
    write: &SymbolicWrite,
    (coordinate, binder): (usize, &FoldBinder),
    region: &IndexUnion,
) -> IndexUnion {
    let mut points = IndexUnion::default();
    for shift in binder_shifts(axes, &write.axes, Some(coordinate)) {
        let earlier = if binder.descending {
            shift > 0
        } else {
            shift < 0
        };
        if !earlier || shift % binder.domain.step() != 0 {
            continue;
        }
        // `p` reads what `p + shift` wrote.
        // An overflowing shift proves no earlier write.
        if let Some(reached) = write
            .region
            .translated(coordinate, -shift)
            .and_then(|moved| moved.meet(region))
        {
            points.extend(reached);
        }
    }
    points
}

fn conjuncts<'a>(expression: &'a Expression, terms: &mut Vec<&'a Expression>) {
    match expression {
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            conjuncts(lhs, terms);
            conjuncts(rhs, terms);
        }
        other => terms.push(other),
    }
}

/// The value a generated branch-selection Boolean captured, when it reads
/// only binders and immutable values: within one iteration those cannot
/// change between the capture and its use, so the capture selects exactly the
/// binder values its definition does.
fn selection_value<'a>(
    condition: &'a Expression,
    context: FunctionValidationContext<'a>,
) -> &'a Expression {
    let mut current = condition;
    for _ in 0..16 {
        match generated_boolean_value(current, context) {
            Some(value) if reads_only_immutable(value, context) => current = value,
            _ => break,
        }
    }
    current
}

/// The binder values of `scope` at which a read `axes` of `target` is defined
/// by writes `owner`, an enclosing loop, made earlier in its current
/// iteration. Such a write ran before `scope` began, so it holds at every
/// binder value of `scope`. The read, its inner binders replaced by their
/// images over `scope`'s values, is compared in `owner`'s coordinates one
/// element at a time, since each element may come from a different write
/// (`solution[1, c]` ... `solution[6, c]` before `solution[i, c]`).
fn enclosing_writes_defining(
    owner: &FoldScope,
    scope: &FoldScope,
    target: &VarName,
    axes: &[IndexAxis],
) -> IndexUnion {
    let width = owner.region.0.len();
    let Some(parts) =
        inner_images(axes, &scope.region, width).and_then(|read| single_offset_parts(&read, true))
    else {
        return IndexUnion::default();
    };
    let writes = owner
        .writes
        .iter()
        .filter(|write| &write.target == target)
        .collect::<Vec<_>>();
    let covering = |part: &Vec<IndexAxis>| {
        let mut covering = IndexUnion::default();
        for write in writes
            .iter()
            .filter(|write| binder_shifts(part, &write.axes, None).contains(&0))
        {
            covering.extend(write.region.clone());
        }
        covering
    };
    let Some(reached) = parts
        .iter()
        .map(covering)
        .reduce(|reached, covering| reached.meet(&covering).unwrap_or_default())
    else {
        return IndexUnion::default();
    };
    let inner = &scope.region.0[width..];
    IndexUnion::of(
        reached
            .boxes()
            .iter()
            .map(|part| IndexBox(part.0.iter().chain(inner).copied().collect())),
    )
}
