//! Prove an aggregate's entry seed is dead when a fixed loop fills it in an
//! order computed by scalar integer recurrences. The emitted loop stays compact.
use super::*;
use rumoca_core::Statement;

struct Candidate {
    extent: usize,
    written: HashSet<usize>,
    read_early: bool,
}

struct Trace {
    values: EvalContext,
    integers: HashSet<String>,
    candidates: HashMap<VarName, Candidate>,
    budget: usize,
}

impl Trace {
    fn integer(&self, expression: &Expression) -> Option<i64> {
        match eval_expr(expression, &self.values).ok()? {
            EvalValue::Integer(x) => Some(x),
            _ => None,
        }
    }

    fn condition(&self, expression: &Expression) -> Option<bool> {
        eval_expr(expression, &self.values).ok()?.as_bool()
    }

    fn observe(&mut self, expression: &Expression) {
        let mut reads = Vec::new();
        expression.collect_var_refs(&mut reads);
        for name in reads {
            if let Some(c) = self.candidates.get_mut(&name) {
                c.read_early |= c.written.len() != c.extent;
            }
        }
    }

    fn assignment(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        value: &Expression,
    ) -> Option<()> {
        let [part] = comp.parts() else { return None };
        self.observe(value);
        let name = VarName::new(&part.ident);
        if part.subs.is_empty() {
            self.candidates.remove(&name);
            if !self.integers.contains(&part.ident) {
                return Some(());
            }
            let v = self.integer(value);
            self.values.parameters.shift_remove(&part.ident);
            if let Some(v) = v {
                self.values
                    .add_parameter(&part.ident, EvalValue::Integer(v));
            }
            return Some(());
        }
        for sub in &part.subs {
            if let Subscript::Expr { expr, .. } = sub {
                self.observe(expr);
            }
        }
        if !self.candidates.contains_key(&name) {
            return Some(());
        }
        let index = match part.subs.as_slice() {
            [Subscript::Expr { expr, .. }] => self.integer(expr)?,
            [Subscript::Index { value, .. }] => *value,
            _ => return None,
        };
        let c = self.candidates.get_mut(&name)?;
        let i = usize::try_from(index).ok()?.checked_sub(1)?;
        if i >= c.extent {
            return None;
        }
        c.written.insert(i);
        Some(())
    }

    fn visit_all(&mut self, statements: &[Statement]) -> Option<()> {
        statements.iter().try_for_each(|item| self.visit(item))
    }

    fn visit(&mut self, statement: &Statement) -> Option<()> {
        self.budget = self.budget.checked_sub(1)?;
        match statement {
            Statement::Assignment { comp, value, .. } => self.assignment(comp, value),
            Statement::For {
                indices, equations, ..
            } => {
                let [binder] = indices.as_slice() else {
                    return None;
                };
                self.observe(&binder.range);
                let Expression::Range {
                    start, end, step, ..
                } = &binder.range
                else {
                    return None;
                };
                let bounds = (
                    self.integer(start)?,
                    step.as_deref().map_or(Some(1), |s| self.integer(s))?,
                    self.integer(end)?,
                );
                if bounds.1 == 0 {
                    return None;
                }
                let saved = self.values.parameters.shift_remove(&binder.ident);
                for v in super::super::fixed_loops::range_values(bounds) {
                    self.budget = self.budget.checked_sub(1)?;
                    self.values
                        .add_parameter(&binder.ident, EvalValue::Integer(v));
                    self.visit_all(equations)?;
                }
                self.values.parameters.shift_remove(&binder.ident);
                if let Some(v) = saved {
                    self.values.add_parameter(&binder.ident, v);
                }
                Some(())
            }
            Statement::While { block, .. } => {
                self.observe(&block.cond);
                while self.condition(&block.cond)? {
                    self.budget = self.budget.checked_sub(1)?;
                    self.visit_all(&block.stmts)?;
                }
                Some(())
            }
            _ => None,
        }
    }
}

fn has_indirect_write(
    statement: &Statement,
    integers: &HashSet<String>,
    candidates: &HashMap<VarName, Candidate>,
) -> bool {
    match statement {
        Statement::Assignment { comp, .. } => comp.parts().first().is_some_and(|part| {
            candidates.contains_key(&VarName::new(&part.ident))
                && part.subs.iter().any(|sub| {
                    let Subscript::Expr { expr, .. } = sub else {
                        return false;
                    };
                    let mut reads = Vec::new();
                    expr.collect_var_refs(&mut reads);
                    reads.iter().any(|name| integers.contains(name.as_str()))
                })
        }),
        Statement::For { equations, .. } => equations
            .iter()
            .any(|s| has_indirect_write(s, integers, candidates)),
        Statement::While { block, .. } => block
            .stmts
            .iter()
            .any(|s| has_indirect_write(s, integers, candidates)),
        _ => false,
    }
}

pub(super) fn complete_prefix(context: FunctionValidationContext<'_>) -> HashSet<VarName> {
    // Only certify writes in the first outer loop. Reads in the subsequent
    // algorithm then have an owner independent of their numerical input data.
    let body = &context.function.body;
    let Some(end) = body.iter().position(|s| matches!(s, Statement::For { .. })) else {
        return HashSet::new();
    };
    let mut values = EvalContext::new();
    for (name, value) in context.static_integers {
        values.add_parameter(name.as_str(), EvalValue::Integer(*value));
    }
    let mut candidates = HashMap::new();
    let mut integers = HashSet::new();
    for local in context
        .function
        .outputs
        .iter()
        .chain(&context.function.locals)
    {
        let name = VarName::new(&local.name);
        if effective_function_scalar_type(context.flat, local) == Some(dae::ScalarType::Integer)
            && local.shape_expr.is_empty()
        {
            integers.insert(local.name.clone());
        }
        if local.default.is_some() {
            continue;
        }
        if let Some([extent]) = context.shapes.get(&name).map(Vec::as_slice)
            && (1..=32768).contains(extent)
        {
            candidates.insert(
                name,
                Candidate {
                    extent: *extent as usize,
                    written: HashSet::new(),
                    read_early: false,
                },
            );
        }
    }
    if candidates.is_empty() || !has_indirect_write(&body[end], &integers, &candidates) {
        return HashSet::new();
    }
    let mut trace = Trace {
        values,
        integers,
        candidates,
        budget: 32768,
    };
    for item in &body[..=end] {
        if trace.visit(item).is_none() {
            return HashSet::new();
        }
    }
    trace
        .candidates
        .into_iter()
        .filter_map(|(name, c)| (!c.read_early && c.extent == c.written.len()).then_some(name))
        .collect()
}
