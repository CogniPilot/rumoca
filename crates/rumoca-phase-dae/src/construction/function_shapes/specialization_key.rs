//! The value component of a call's specialization key, and the model
//! parameters it fixes at translation (SPEC_0040 DAE-C22).
//!
//! A callee keyed on an input's value (`ValueReadInputs`) folds that value
//! into its body: a declared dimension, a compact `for` range or a `while`
//! condition of the specialization reads it as a translation-time constant.
//! Only a declared input or output dimension makes that value structural: the
//! call's argument and result shapes are fixed at translation (MLS 3.7 §10.1,
//! §12.2), exactly as a model array dimension is, so a model-scope argument
//! read there makes the ordinary parameters it reads evaluable, with the
//! parameters their bindings read, reported (WD001) at the call. A range, a
//! `while` condition, a local dimension or a `fill` extent is evaluated when
//! the function runs (§11.2.2, §11.2.3), so a position read only there is
//! keyed only on a value that is already fixed at translation (constants,
//! evaluable parameters, and parameters a flatten structural use fixes); an
//! argument reading a tunable parameter keys no value there, and the body is
//! lowered over a run-time bounded domain or refused. A parameter MLS 3.7
//! makes non-evaluable (§4.5 `fixed = false`, §18.6 `Evaluate = false`), or
//! any parameter whose binding reads one, never keys a specialization.

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

/// The parameters whose value can be set after translation: every parameter
/// that is not evaluable (`evaluable`: constants' dependents, `final` and
/// `Evaluate = true` parameters and theirs) and that no flatten structural
/// use (a model dimension, a for-equation range, a selected branch) fixes.
/// With no evaluable set known, every parameter is treated as fixed.
pub(super) fn tunable_parameters(
    flat: &flat::Model,
    evaluable: Option<&HashSet<VarName>>,
) -> HashSet<VarName> {
    let Some(evaluable) = evaluable else {
        return HashSet::new();
    };
    let structural = flat
        .parameter_branch_selections
        .iter()
        .flat_map(|selection| selection.declared_references(flat))
        .collect::<HashSet<_>>();
    flat.variables
        .iter()
        .filter(|(name, variable)| {
            matches!(variable.variability, Variability::Parameter(_))
                && !evaluable.contains(*name)
                && !structural.contains(*name)
        })
        .map(|(name, _)| name.clone())
        .collect()
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

/// Whether a model-scope `argument` reads a member of `names`. A
/// specialization scope reads only its own inputs and locals, whose keyed
/// values its caller already proved.
fn model_scope_reads(
    argument: &Expression,
    values: &ShapeEnvironment,
    names: &HashSet<VarName>,
) -> bool {
    if values.specialized || names.is_empty() {
        return false;
    }
    let mut reads = HashSet::new();
    value_reads(argument, &mut reads);
    reads.iter().any(|name| names.contains(name))
}

impl FunctionShapeAnalysis {
    /// The proven argument values that identify one call's specialization.
    ///
    /// A position carries a value only when the callee's declared dimensions,
    /// ranges or `while` conditions read that input's value, only when the
    /// argument is evaluable at translation time, and, in the model scope,
    /// only when it reads no non-evaluable parameter and, unless a declared
    /// interface dimension reads the position, no tunable one. Every other position
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
                    || model_scope_reads(argument, values, &self.non_evaluable)
                    || (!self.value_read_inputs.is_structural(function, ordinal)
                        && model_scope_reads(argument, values, &self.tunable))
                {
                    return None;
                }
                values.proven_value(argument)
            })
            .collect()
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
        // A position only the run-time body reads keys values already fixed
        // at translation, so only interface dimensions fix parameters here.
        let value_read_inputs = &self.analysis.value_read_inputs;
        let mut reads = HashSet::new();
        for (_, (argument, _)) in
            arguments
                .iter()
                .zip(&input_values)
                .enumerate()
                .filter(|(ordinal, (_, value))| {
                    value.is_some() && value_read_inputs.is_structural(function, *ordinal)
                })
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
