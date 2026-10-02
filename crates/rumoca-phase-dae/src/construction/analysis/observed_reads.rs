//! Which model variables the simulation reads.
//!
//! A discrete-valued variable whose definition is not a discrete-time
//! expression (MLS 3.7 §3.8.5) is accepted only when nothing reads it: no
//! equation, binding, attribute, assertion, algorithm, when-clause, or function
//! argument whose value reaches a function result. Such a variable is a pure
//! observation and cannot influence the simulation (see
//! `discrete_time_definitions`).
//!
//! A read is any occurrence of the variable, a component that contains it, or
//! an element of it, with one exception: a plain reference passed to a
//! function input the callee never reads. MSL media pass a whole
//! `ThermodynamicState` (record inputs are decomposed into one input per
//! field) to functions such as `density(state)` that read only `state.d`. An
//! input is unread when every occurrence of it in the callee is itself such a
//! pass-through; inputs of external, impure, and derivative-annotated functions
//! are always read. The relation is the least fixed point over the static call
//! graph, so recursion cannot make an input unread.

use super::*;
use rumoca_core::ExpressionVisitor;
use std::collections::BTreeSet;

/// The model's read sites, keyed by the name each reference spells.
pub(super) struct ModelReads {
    /// Every read reference name and the sites that read it.
    sites: HashMap<String, ReadSites>,
    /// Every name that is a component or element prefix of a read name.
    enclosing: HashMap<String, ReadSites>,
    inputs: FunctionInputReads,
}

/// [`ModelReads`] built on first use: only a definition that is not a
/// discrete-time expression asks for it.
pub(super) struct LazyModelReads<'flat> {
    flat: &'flat flat::Model,
    reads: std::cell::OnceCell<ModelReads>,
}

impl<'flat> LazyModelReads<'flat> {
    pub(super) fn new(flat: &'flat flat::Model) -> Self {
        Self {
            flat,
            reads: std::cell::OnceCell::new(),
        }
    }

    pub(super) fn get(&self) -> &ModelReads {
        self.reads.get_or_init(|| ModelReads::analyze(self.flat))
    }
}

#[derive(Clone, Default)]
struct ReadSites {
    /// Read by a site other than a model equation.
    other: bool,
    /// Model equation rows that read the name.
    equations: BTreeSet<usize>,
}

impl ReadSites {
    fn add(&mut self, site: Site) {
        match site {
            Site::Equation(row) => {
                self.equations.insert(row);
            }
            Site::Other => self.other = true,
        }
    }

    fn merge(&mut self, other: &ReadSites) {
        self.other |= other.other;
        self.equations.extend(other.equations.iter().copied());
    }
}

#[derive(Clone, Copy)]
enum Site {
    Equation(usize),
    Other,
}

impl ModelReads {
    fn analyze(flat: &flat::Model) -> Self {
        let inputs = FunctionInputReads::analyze(flat);
        let mut reads = Self {
            sites: HashMap::new(),
            enclosing: HashMap::new(),
            inputs,
        };
        for (row, equation) in flat.equations.iter().enumerate() {
            reads.record(&equation.residual, Site::Equation(row));
        }
        let mut other = Vec::new();
        for variable in flat.variables.values() {
            other.extend(variable_attribute_expressions(variable));
        }
        other.extend(
            flat.initial_equations
                .iter()
                .map(|equation| &equation.residual),
        );
        other.extend(structured_template_expressions(&flat.structured_equations));
        other.extend(structured_template_expressions(
            &flat.initial_structured_equations,
        ));
        for assertion in flat
            .assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
        {
            other.push(&assertion.condition);
            other.push(&assertion.message);
            other.extend(assertion.level.as_ref());
        }
        for algorithm in flat.algorithms.iter().chain(&flat.initial_algorithms) {
            other.extend(function_shapes::statement_expression_roots(
                &algorithm.statements,
            ));
        }
        for chain in &flat.when_chains {
            for branch in chain.branches() {
                other.push(&branch.condition);
                when_equation_expressions(&branch.equations, &mut other);
            }
        }
        for expression in other {
            reads.record(expression, Site::Other);
        }
        reads
    }

    fn record(&mut self, expression: &Expression, site: Site) {
        let mut names = Vec::new();
        self.inputs.live_reads(expression, &mut names);
        for name in names {
            for prefix in component_prefixes(&name) {
                self.enclosing
                    .entry(prefix.to_string())
                    .or_default()
                    .add(site);
            }
            self.sites.entry(name).or_default().add(site);
        }
    }

    /// Whether `target` is read by anything but its own definition: model
    /// equation row `defining_row` (when the definition is an equation) whose
    /// defining value is `value`.
    pub(super) fn is_unread(
        &self,
        target: &VarName,
        defining_row: Option<usize>,
        value: &Expression,
    ) -> bool {
        let target = target.as_str();
        let mut sites = self.enclosing.get(target).cloned().unwrap_or_default();
        for prefix in component_prefixes(target) {
            if let Some(read) = self.sites.get(prefix) {
                sites.merge(read);
            }
        }
        if sites.other || sites.equations.iter().any(|row| Some(*row) != defining_row) {
            return false;
        }
        let mut names = Vec::new();
        self.inputs.live_reads(value, &mut names);
        !names.iter().any(|name| related(name, target))
    }
}

fn when_equation_expressions<'flat>(
    equations: &'flat [flat::WhenEquation],
    out: &mut Vec<&'flat Expression>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                out.push(value);
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                out.push(condition);
                out.push(message);
                out.extend(level.as_deref());
            }
            flat::WhenEquation::Terminate { message, .. } => out.push(message),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, body) in branches {
                    out.push(condition);
                    when_equation_expressions(body, out);
                }
                when_equation_expressions(else_branch.as_deref().unwrap_or_default(), out);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => out.push(function),
        }
    }
}

/// Every component or element prefix of `name`, ending with `name` itself:
/// `a.b[1].c` yields `a`, `a.b`, `a.b[1]`, `a.b[1].c`. A separator inside a
/// subscript does not split.
fn component_prefixes(name: &str) -> impl Iterator<Item = &str> {
    let mut depth = 0usize;
    let mut cuts = Vec::new();
    for (index, character) in name.char_indices() {
        match character {
            '[' => {
                if depth == 0 && index > 0 {
                    cuts.push(index);
                }
                depth += 1;
            }
            ']' => depth = depth.saturating_sub(1),
            '.' if depth == 0 => cuts.push(index),
            _ => {}
        }
    }
    cuts.push(name.len());
    cuts.into_iter().map(move |cut| &name[..cut])
}

/// Whether one name is a component or element prefix of the other.
fn related(lhs: &str, rhs: &str) -> bool {
    let (short, long) = if lhs.len() <= rhs.len() {
        (lhs, rhs)
    } else {
        (rhs, lhs)
    };
    long.strip_prefix(short)
        .is_some_and(|rest| rest.is_empty() || rest.starts_with(['.', '[']))
}

/// The function input positions whose value can reach a result or effect.
struct FunctionInputReads {
    read: HashMap<VarName, Vec<bool>>,
}

impl FunctionInputReads {
    fn analyze(flat: &flat::Model) -> Self {
        let mut analysis = Self {
            read: flat
                .functions
                .iter()
                .map(|(name, function)| {
                    let opaque = function.external.is_some()
                        || !function.pure
                        || !function.derivatives.is_empty();
                    (name.clone(), vec![opaque; function.inputs.len()])
                })
                .collect(),
        };
        // Each round can only mark more inputs read and the input count is
        // finite, so the least fixed point is reached.
        while flat.functions.iter().fold(false, |grew, (name, function)| {
            analysis.mark_read_inputs(name, function) || grew
        }) {}
        analysis
    }

    /// Mark the inputs of `function` its body reads; whether any was new.
    fn mark_read_inputs(&mut self, name: &VarName, function: &rumoca_core::Function) -> bool {
        let mut names = Vec::new();
        for expression in function_shapes::function_expressions(function) {
            self.live_reads(expression, &mut names);
        }
        let mask = self
            .read
            .get_mut(name)
            .expect("every function seeds its input reads");
        let mut grew = false;
        for (input, read) in function.inputs.iter().zip(mask.iter_mut()) {
            if !*read && names.iter().any(|name| related(name, &input.name)) {
                *read = true;
                grew = true;
            }
        }
        grew
    }

    /// Collect the names `expression` reads, skipping plain references passed
    /// to inputs their callee never reads.
    fn live_reads(&self, expression: &Expression, names: &mut Vec<String>) {
        LiveReads {
            inputs: self,
            names,
        }
        .visit_expression(expression);
    }

    fn passes_through(&self, callee: &VarName, args: &[Expression], ordinal: usize) -> bool {
        let Some(mask) = self.read.get(callee) else {
            return false;
        };
        // A named argument would make positions disagree with declarations.
        if args.len() > mask.len() || args.iter().any(is_named_argument) {
            return false;
        }
        !mask[ordinal] && is_plain_reference(&args[ordinal])
    }
}

struct LiveReads<'a> {
    inputs: &'a FunctionInputReads,
    names: &'a mut Vec<String>,
}

impl ExpressionVisitor for LiveReads<'_> {
    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        self.names.push(name.as_str().to_string());
        self.walk_var_ref(name, subscripts);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        _is_constructor: bool,
    ) {
        for (ordinal, argument) in args.iter().enumerate() {
            if self.inputs.passes_through(name.var_name(), args, ordinal) {
                continue;
            }
            self.visit_expression(argument);
        }
    }
}

fn is_named_argument(argument: &Expression) -> bool {
    matches!(
        argument,
        Expression::FunctionCall { name, .. }
            if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

/// A reference with no computed subscript, or an array of them (a vectorized
/// call, MLS §12.4.6, applies the input element-wise).
fn is_plain_reference(argument: &Expression) -> bool {
    match argument {
        Expression::VarRef { subscripts, .. } => subscripts
            .iter()
            .all(|subscript| !matches!(subscript, Subscript::Expr { .. })),
        Expression::Array { elements, .. } => elements.iter().all(is_plain_reference),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn component_prefixes_split_outside_subscripts() {
        assert_eq!(
            component_prefixes("a.b[x.y].c").collect::<Vec<_>>(),
            ["a", "a.b", "a.b[x.y]", "a.b[x.y].c"]
        );
    }

    #[test]
    fn related_names_share_a_component_boundary() {
        assert!(related("s[1]", "s[1].phase"));
        assert!(related("s[1].phase", "s"));
        assert!(!related("s[1].ph", "s[1].phase"));
        assert!(!related("s[1]", "s[10]"));
    }
}
