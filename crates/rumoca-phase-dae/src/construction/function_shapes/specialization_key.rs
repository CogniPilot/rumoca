//! The value component of a call's specialization key, and the model
//! parameters it fixes at translation (SPEC_0040 DAE-C22).
//!
//! A callee keyed on an input's value (`ValueReadInputs`) folds that value
//! into its body: a declared dimension, a compact `for` range or a `while`
//! condition of the specialization reads it as a translation-time constant.
//! When a model-scope argument reads an ordinary parameter, keying on its
//! value is a structural use of that parameter, exactly as an array dimension
//! or a for-equation range is (MLS 3.7 §10.1, §8.3.3): the parameter is
//! recorded evaluable, with the parameters its binding reads, and reported
//! (WD001) at the call. A parameter MLS 3.7 makes non-evaluable (§4.5
//! `fixed = false`, §18.6 `Evaluate = false`), or any parameter whose binding
//! reads one, never keys a specialization: the position carries no value, so
//! a construct that needs it is refused instead of folding a value the
//! parameter does not have at translation.

use super::*;

/// One model-scope call whose specialization key carries the value of an
/// argument that reads model variables.
#[derive(Clone, Debug)]
pub(in crate::construction) struct KeyedArgumentReads {
    pub(in crate::construction) span: Span,
    /// Every name a keyed argument of the call reads by value.
    pub(in crate::construction) names: Vec<VarName>,
}

/// Whether `name` is a `fixed = false` or `Evaluate = false` parameter.
pub(in crate::construction) fn non_evaluable_parameter(flat: &flat::Model, name: &VarName) -> bool {
    flat.variables.get(name).is_some_and(|variable| {
        variable.evaluate_refused
            || variable
                .fixed
                .as_ref()
                .is_some_and(|fixed| fixed.iter().any(|value| !value))
    })
}

/// Every parameter that is non-evaluable or whose binding reads one, closed
/// over bindings: the names whose translation-time value a key never carries.
pub(super) fn non_evaluable_closure(flat: &flat::Model) -> HashSet<VarName> {
    let mut readers: HashMap<VarName, Vec<&VarName>> = HashMap::new();
    for (name, variable) in &flat.variables {
        if !matches!(variable.variability, Variability::Parameter(_)) {
            continue;
        }
        let mut reads = HashSet::new();
        if let Some(binding) = &variable.binding {
            value_reads(binding, &mut reads);
        }
        for read in reads {
            readers.entry(read).or_default().push(name);
        }
    }
    let mut closed = HashSet::new();
    let mut pending = flat
        .variables
        .keys()
        .filter(|name| non_evaluable_parameter(flat, name))
        .collect::<Vec<_>>();
    while let Some(name) = pending.pop() {
        if closed.insert(name.clone()) {
            pending.extend(readers.get(name).into_iter().flatten().copied());
        }
    }
    closed
}

/// The names `expression` reads by value: a `size(a, k)` reads only the
/// translation-time shape of `a`.
fn value_reads(expression: &Expression, reads: &mut HashSet<VarName>) {
    match expression {
        Expression::VarRef { name, .. } => {
            reads.insert(name.var_name().clone());
        }
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => {
            for arg in args.iter().skip(1) {
                value_reads(arg, reads);
            }
            return;
        }
        _ => {}
    }
    for child in expression_children(expression) {
        value_reads(child, reads);
    }
}

impl FunctionShapeAnalysis {
    /// The proven argument values that identify one call's specialization.
    ///
    /// A position carries a value only when the callee's declared dimensions,
    /// ranges or `while` conditions read that input's value, only when the
    /// argument is evaluable at translation time, and, in the model scope,
    /// only when it reads no non-evaluable parameter. Every other position
    /// records `None`, which keeps calls that differ only in a value no shape
    /// reads inside one shared specialization: the property that both prevents
    /// duplicate DAE functions and lets a recursive call repeat its key.
    pub(super) fn proven_input_values(
        &self,
        function: &VarName,
        arguments: &[Expression],
        values: &ShapeEnvironment,
    ) -> Vec<Option<ProvenValue>> {
        arguments
            .iter()
            .enumerate()
            .map(|(ordinal, argument)| {
                if !self.value_read_inputs.reads_value(function, ordinal)
                    || self.reads_non_evaluable(argument, values)
                {
                    return None;
                }
                values.proven_value(argument)
            })
            .collect()
    }

    /// Whether a model-scope `argument` reads a parameter with no
    /// translation-time value. A specialization scope reads only its own
    /// inputs and locals, whose keyed values its caller already proved.
    fn reads_non_evaluable(&self, argument: &Expression, values: &ShapeEnvironment) -> bool {
        if values.specialized || self.non_evaluable.is_empty() {
            return false;
        }
        let mut reads = HashSet::new();
        value_reads(argument, &mut reads);
        reads.iter().any(|name| self.non_evaluable.contains(name))
    }

    /// Model-scope calls whose keys carry a value read from model variables.
    pub(in crate::construction) fn keyed_argument_reads(&self) -> &[KeyedArgumentReads] {
        &self.keyed_argument_reads
    }
}

impl ShapeAnalyzer<'_> {
    /// [`FunctionShapeAnalysis::proven_input_values`] for a call discovery
    /// certifies, recording the model variables a model-scope key reads.
    pub(super) fn keyed_input_values(
        &mut self,
        function: &VarName,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Vec<Option<ProvenValue>> {
        let input_values = self
            .analysis
            .proven_input_values(function, arguments, values);
        if values.specialized {
            return input_values;
        }
        let mut reads = HashSet::new();
        for (argument, _) in arguments
            .iter()
            .zip(&input_values)
            .filter(|(_, value)| value.is_some())
        {
            value_reads(argument, &mut reads);
        }
        if !reads.is_empty() {
            let mut names = reads.into_iter().collect::<Vec<_>>();
            names.sort();
            self.analysis
                .keyed_argument_reads
                .push(KeyedArgumentReads { span, names });
        }
        input_values
    }
}
