use super::*;

#[path = "function_shapes/expression_rules.rs"]
mod expression_rules;
#[path = "function_shapes/value_relevance.rs"]
mod value_relevance;
pub(in crate::construction) use expression_rules::{
    call_free_expression_shape, call_free_target_shape,
};
use expression_rules::{expression_shape, reject_shape_call};
use value_relevance::ValueReadInputs;

#[cfg(test)]
#[path = "function_shapes/tests/missing_provenance.rs"]
mod provenance_tests;
#[cfg(test)]
#[path = "function_shapes/tests/value_proven_shapes.rs"]
mod value_proven_shape_tests;

pub(super) type ValueShape = Vec<u32>;

/// Maximum nesting of value-proven specializations under one call.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// A specialization is memoized before its body is discovered, so a call chain
/// terminates as soon as it repeats a key. Two things make a chain repeat:
/// [`ValueReadInputs`] keeps an input out of the key unless a declared dimension
/// or range reads its value, so `f(n) = if n <= 0 then 0 else 1 + f(n - 1)` has
/// one key for every `n`; and a value-keyed argument that converges — `q(m)`
/// calling `q(integer(m/2))` — reaches its base value in `log m` activations.
///
/// **Accepted.** Every chain that repeats a key, including self- and mutual
/// recursion over non-value-keyed inputs, and value-keyed recursion whose
/// arguments converge within this bound.
///
/// **Typed-rejected.** A chain that needs more than `SPECIALIZATION_DEPTH_LIMIT`
/// *distinct* value-keyed activations. `f(n)` with `output Real y[n]` calling
/// `f(n + 1)` is the shape of it: each activation declares a wider result, so no
/// key repeats. The report states the bound it exceeded rather than claiming a
/// proof that no fixed point exists, because this analysis does not decide that.
///
/// **Owner.** `ShapeAnalyzer::ensure_specialization`, the only place a
/// certificate is minted.
///
/// **Evidence.** `function_shapes/tests/value_proven_shapes.rs`:
/// `value_recursion_without_a_fixed_point_is_bounded` (rejected),
/// `scalar_valued_recursion_reuses_one_specialization` and
/// `converging_value_keyed_recursion_terminates` (accepted).
const SPECIALIZATION_DEPTH_LIMIT: usize = 256;

/// Shapes, and the translation-time values, every function shape proof reads.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// MLS §12.2 (SPEC_0022 FUNC-009) gives a function's non-input array dimension
/// sizes as expressions over "inputs, constants, or parameter expressions", and
/// MLS §4.4.2 (SPEC_0022 DECL-018) restricts every array dimension to a "scalar
/// non-negative evaluable expression of type Integer or enumeration/Boolean".
/// Two disjoint kinds of dimension expression follow from that rule, and this
/// environment must prove both:
///
/// * a dimension over an argument's **shape** — `output Real y[size(u, 1)]` —
///   needs only the extents `shapes` records, and was already accepted;
/// * a dimension over an argument's **value** — `output Real y[n]` for
///   `input Integer n` — needs the value, and MLS §4.4.2 bounds that value to
///   the Integer/enumeration/Boolean domain, so proving it is an exact `i64`
///   question and never a floating-point one.
///
/// **Accepted.** A dimension whose value operands are all *evaluable* in the
/// MLS §4.5 sense (SPEC_0022 INST-007, "structural parameters must be
/// compile-time evaluable"): an Integer literal; a model `constant`/`parameter`
/// whose binding the `EvalContext` fixed point settled at translation time; an
/// enumeration literal read through its MLS §4.8.5.2 ordinal; a function input
/// bound to such a value at the call site; and any exact Integer arithmetic
/// over those. Every distinct proven value tuple is owned by its own
/// specialization certificate, so `f(3)` and `f(5)` construct two DAE functions
/// with two exact shapes rather than one mis-shaped function.
///
/// **Typed-rejected.** A dimension whose value operand is genuinely not
/// evaluable — a non-evaluable parameter settled only by the initialization
/// problem (MLS §4.5), a discrete-time or continuous-time variable, a loop
/// index, a runtime function result — keeps the existing `ED019`
/// `function shape proof` rejection naming the scalar whose value is missing.
/// Such a shape is never silently defaulted, widened, or guessed.
///
/// **Owner.** `rumoca_phase_dae::construction::function_shapes`, which is the
/// only producer of `FunctionShapeCertificate` and therefore the only place a
/// DAE function's declared extents come from.
///
/// **Evidence.** `function_shapes/tests/value_proven_shapes.rs`.
#[derive(Clone, Debug, Default)]
pub(super) struct ShapeEnvironment {
    shapes: HashMap<VarName, ValueShape>,
    /// Values proven for scalar coordinates of MLS §4.4.2 dimension type.
    ///
    /// The proven values are held in the same `EvalContext` the rest of this
    /// phase evaluates translation-time expressions through, so an extent is
    /// folded by exactly one arithmetic — MLS §10.6 mixed Integer/Real division,
    /// `integer(...)` truncation, `mod`/`div`, enumeration ordinals — instead of
    /// a second rule set written for shapes alone.
    values: EvalContext,
    /// Whether this scope is one function specialization rather than the model.
    ///
    /// MLS §12.2 makes a function body's translation-time constants a property
    /// of the specialization that fixed its inputs, so a construct whose extent
    /// is folded from this environment — an MLS §10.4.1 array constructor — is
    /// owned by the specialization and never by the one model-wide plan a
    /// source span keys: `f(3)` and `f(5)` share the span and not the extent.
    /// "Has a Modelica body to lower into" is *not* that predicate — an MLS
    /// §12.9 external argument is in specialization scope with no body — so the
    /// distinction is carried by the environment that actually differs.
    specialized: bool,
}

impl ShapeEnvironment {
    pub(super) fn with_capacity(capacity: usize) -> Self {
        Self {
            shapes: HashMap::with_capacity(capacity),
            values: EvalContext::with_capacity(capacity, 0, 0),
            specialized: false,
        }
    }

    /// Mark this scope as one function specialization's proven environment.
    fn into_specialization(mut self) -> Self {
        self.specialized = true;
        self
    }

    /// Whether this scope is a function specialization (MLS §12.2) rather than
    /// the model scope.
    pub(in crate::construction) fn is_specialization(&self) -> bool {
        self.specialized
    }

    pub(super) fn get(&self, name: &VarName) -> Option<&ValueShape> {
        self.shapes.get(name)
    }

    /// Bind a coordinate's shape and drop any value inherited for that name.
    ///
    /// Dropping is the point: a function formal shadows an enclosing model
    /// coordinate of the same flat name, so a formal bound without a proven
    /// value must not read the model coordinate's value through the shadowed
    /// name. Absence of a value here means "not proven at this scope", which is
    /// exactly what the shape proof must then reject on.
    pub(super) fn insert(&mut self, name: VarName, shape: ValueShape) {
        self.values.remove_parameter(name.as_str());
        self.shapes.insert(name, shape);
    }

    /// Bind a scalar coordinate whose translation-time value is proven.
    ///
    /// The shape and the value are inserted together because MLS §4.4.2 admits
    /// a value only for a scalar, so a bound value that disagreed with a
    /// non-scalar shape would be unrepresentable rather than merely wrong.
    pub(super) fn bind_scalar_value(&mut self, name: VarName, value: EvalValue) {
        self.shapes.insert(name.clone(), Vec::new());
        self.values.add_parameter(name.to_string(), value);
    }

    /// The proven translation-time value of an expression, if it has one.
    ///
    /// Absence is a valid semantic outcome — MLS §4.5 lets a parameter be
    /// established by the initialization problem instead — so the caller
    /// decides whether the missing value is fatal for the dimension or the
    /// specialization it is proving.
    pub(super) fn proven_value(&self, expression: &Expression) -> Option<ProvenValue> {
        ProvenValue::from_settled(&eval_expr(expression, &self.values).ok()?)
    }

    /// The exact Integer extent this scope proves for `expression`, if any.
    ///
    /// This is the one predicate that answers "is this bound statically known
    /// here?" for both the validator that admits a compact range (MLS §10.4.1)
    /// and the lowering that folds its bounds, so the two can never disagree
    /// about which ranges are static.
    pub(in crate::construction) fn proven_extent(&self, expression: &Expression) -> Option<i64> {
        self.proven_value(expression).and_then(ProvenValue::extent)
    }
}

/// A translation-time value MLS §4.4.2 admits in an array dimension.
///
/// `Real` and `String` are deliberately absent: MLS §4.4.2 restricts an array
/// dimension to "type Integer or enumeration/Boolean", so a `Real` that happens
/// to be whole is not an extent and must not silently become one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum ProvenValue {
    /// An Integer, or an enumeration literal read through its MLS §4.8.5.2
    /// ordinal.
    Integer(i64),
    Boolean(bool),
}

impl ProvenValue {
    fn from_settled(value: &EvalValue) -> Option<Self> {
        match value {
            EvalValue::Integer(value) => Some(Self::Integer(*value)),
            EvalValue::Bool(value) => Some(Self::Boolean(*value)),
            _ => None,
        }
    }

    fn into_settled(self) -> EvalValue {
        match self {
            Self::Integer(value) => EvalValue::Integer(value),
            Self::Boolean(value) => EvalValue::Bool(value),
        }
    }

    /// The extent this value names, when it names one.
    ///
    /// A `Boolean` proves a *branch* of a dimension expression — `if useLosses
    /// then 3 else 1` — and never an extent by itself.
    pub(in crate::construction) fn extent(self) -> Option<i64> {
        match self {
            Self::Integer(value) => Some(value),
            Self::Boolean(_) => None,
        }
    }
}

/// The branch of an MLS §11.5 function conditional this scope decides.
///
/// MLS §11.5 evaluates the conditions in declaration order and executes the
/// first branch whose condition is `true`, or the else part when none is. This
/// answers that question at translation time and only when the answer is exact:
/// `Some(Some(ordinal))` is a proven taken branch, `Some(None)` is the proven
/// else part, and `None` means this scope does not settle the choice — which is
/// the ordinary case for a condition over a runtime value, and keeps the
/// checked-branch rule that owns those conditionals.
///
/// The scan stops at the first condition without a proven value even when a
/// later condition is proven `true`, because MLS §11.5 would only reach that
/// later condition if the earlier one evaluated to `false`.
pub(in crate::construction) fn proven_conditional_branch(
    blocks: &[rumoca_core::StatementBlock],
    values: &ShapeEnvironment,
) -> Option<Option<usize>> {
    for (ordinal, block) in blocks.iter().enumerate() {
        match values.proven_value(&block.cond)? {
            ProvenValue::Boolean(true) => return Some(Some(ordinal)),
            ProvenValue::Boolean(false) => {}
            // MLS §11.5 requires a Boolean condition; a scope that folded one to
            // an Integer proves nothing about which branch runs.
            ProvenValue::Integer(_) => return None,
        }
    }
    Some(None)
}

impl std::ops::Index<&VarName> for ShapeEnvironment {
    type Output = ValueShape;

    fn index(&self, name: &VarName) -> &ValueShape {
        &self.shapes[name]
    }
}

/// The exact identity of one function specialization.
///
/// Two calls share a DAE function only when they agree on every input shape
/// *and* on every input value a declared dimension could read (MLS §12.2). The
/// value component is what makes `symmetricOrientation(3)` and
/// `symmetricOrientation(5)` distinct owners of distinct result extents.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct FunctionSpecializationKey {
    pub(super) function: VarName,
    pub(super) inputs: Vec<ValueShape>,
    /// Proven translation-time value of each input, positionally.
    ///
    /// `None` records an input with no proven value; the specialization is then
    /// accepted only if no declared dimension reads that input's value.
    pub(super) input_values: Vec<Option<ProvenValue>>,
}

#[derive(Clone, Debug)]
pub(super) struct FunctionShapeCertificate {
    pub(super) key: FunctionSpecializationKey,
    pub(super) parameters: Vec<ValueShape>,
    pub(super) results: Vec<ValueShape>,
    pub(super) values: ShapeEnvironment,
}

pub(super) struct FunctionShapeAnalysis {
    model_values: ShapeEnvironment,
    certificates: Vec<FunctionShapeCertificate>,
    certificate_by_key: HashMap<FunctionSpecializationKey, usize>,
    dependencies: Vec<Vec<usize>>,
    constructor_names: HashSet<VarName>,
    constructor_fields_by_key: HashMap<FunctionSpecializationKey, Vec<ValueShape>>,
    /// Input positions each callable keys a specialization on by value.
    ///
    /// An input qualifies only when MLS §4.4.2 admits its declared type in a
    /// dimension *and* some declared dimension or compact range of the callable
    /// actually reads its value (MLS §12.2). Keying on any other input would
    /// split certificates that are proven identical, which both duplicates DAE
    /// functions and denies a recursive call the repeated key it terminates on.
    value_read_inputs: ValueReadInputs,
}

impl FunctionShapeAnalysis {
    pub(super) fn analyze(flat: &flat::Model, constants: &EvalContext) -> Result<Self, ToDaeError> {
        let model_values = concrete_model_shapes(flat, constants)?;
        let constructor_names = flat
            .functions
            .values()
            .filter(|function| function.is_constructor)
            .map(|function| function.name.clone())
            .collect();
        let dimension_typed_inputs = flat
            .functions
            .iter()
            .map(|(name, function)| {
                let mask = function
                    .inputs
                    .iter()
                    .map(|input| is_dimension_typed_scalar(flat, input))
                    .collect();
                (name.clone(), mask)
            })
            .collect();
        let value_read_inputs = ValueReadInputs::analyze(flat, &dimension_typed_inputs);
        let mut analyzer = ShapeAnalyzer {
            flat,
            analysis: Self {
                model_values,
                certificates: Vec::new(),
                certificate_by_key: HashMap::new(),
                dependencies: Vec::new(),
                constructor_names,
                constructor_fields_by_key: HashMap::new(),
                value_read_inputs,
            },
            active_specializations: Vec::new(),
        };
        analyzer.discover_model_calls()?;
        Ok(analyzer.analysis)
    }

    /// The proven argument values that identify one call's specialization.
    ///
    /// A position carries a value only when the callee's declared dimensions or
    /// ranges actually read that input's value, and only when the argument is
    /// evaluable at translation time. Every other position records `None`, which
    /// keeps calls that differ only in a value no shape reads inside one shared
    /// specialization — the property that both prevents duplicate DAE functions
    /// and lets a recursive call repeat its key.
    fn proven_input_values(
        &self,
        function: &VarName,
        arguments: &[Expression],
        values: &ShapeEnvironment,
    ) -> Vec<Option<ProvenValue>> {
        arguments
            .iter()
            .enumerate()
            .map(|(ordinal, argument)| {
                if self.value_read_inputs.reads_value(function, ordinal) {
                    values.proven_value(argument)
                } else {
                    None
                }
            })
            .collect()
    }

    pub(super) fn model_values(&self) -> &ShapeEnvironment {
        &self.model_values
    }

    pub(super) fn certificates(&self) -> &[FunctionShapeCertificate] {
        &self.certificates
    }

    pub(super) fn construction_components(&self) -> Vec<rumoca_core::DependencyScc> {
        rumoca_core::dependency_first_sccs(&self.dependencies)
            .expect("function shape dependencies reference known certificates")
    }

    pub(super) fn constructor_field_shapes(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
    ) -> Option<&[ValueShape]> {
        let inputs = arguments
            .iter()
            .map(|argument| self.expression_shape(argument, values))
            .collect::<Result<Vec<_>, _>>()
            .ok()?;
        let function = name.var_name().clone();
        let input_values = self.proven_input_values(&function, arguments, values);
        self.constructor_fields_by_key
            .get(&FunctionSpecializationKey {
                function,
                inputs,
                input_values,
            })
            .map(Vec::as_slice)
    }

    pub(super) fn certificate(
        &self,
        key: &FunctionSpecializationKey,
    ) -> Option<&FunctionShapeCertificate> {
        self.certificate_by_key
            .get(key)
            .map(|index| &self.certificates[*index])
    }

    pub(super) fn call_key(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Result<FunctionSpecializationKey, ToDaeError> {
        let inputs = arguments
            .iter()
            .map(|argument| self.expression_shape(argument, values))
            .collect::<Result<Vec<_>, _>>()?;
        let function = name.var_name().clone();
        let input_values = self.proven_input_values(&function, arguments, values);
        let key = FunctionSpecializationKey {
            function,
            inputs,
            input_values,
        };
        self.certificate(&key).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape specialization",
                format!(
                    "`{}` has no constructor-proven specialization for this call signature",
                    name.as_str()
                ),
                span,
            )
        })?;
        Ok(key)
    }

    pub(super) fn expression_shape(
        &self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        let mut resolve = |name: &rumoca_core::Reference, arguments: &[Expression], span: Span| {
            if self.constructor_names.contains(name.var_name()) {
                return Ok(Vec::new());
            }
            let key = self.call_key(name, arguments, values, span)?;
            self.certificate(&key)
                .and_then(|certificate| certificate.results.first())
                .cloned()
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function result shape",
                        format!("`{}` has no first result", name.as_str()),
                        span,
                    )
                })
        };
        expression_shape(expression, values, &mut resolve)
    }
}

struct ShapeAnalyzer<'flat> {
    flat: &'flat flat::Model,
    analysis: FunctionShapeAnalysis,
    active_specializations: Vec<usize>,
}

impl ShapeAnalyzer<'_> {
    fn discover_model_calls(&mut self) -> Result<(), ToDaeError> {
        let values = self.analysis.model_values.clone();
        for expression in all_model_expressions(self.flat) {
            self.discover_calls(expression, &values)?;
        }
        for algorithm in self
            .flat
            .algorithms
            .iter()
            .chain(&self.flat.initial_algorithms)
        {
            self.discover_statements(&algorithm.statements, &values)?;
        }
        for chain in &self.flat.when_chains {
            for branch in chain.branches() {
                self.discover_calls(&branch.condition, &values)?;
                self.discover_when_equations(&branch.equations, &values)?;
            }
        }
        for assertion in self
            .flat
            .assert_equations
            .iter()
            .chain(&self.flat.initial_assert_equations)
        {
            self.discover_calls(&assertion.condition, &values)?;
            self.discover_calls(&assertion.message, &values)?;
            if let Some(level) = &assertion.level {
                self.discover_calls(level, &values)?;
            }
        }
        Ok(())
    }

    fn discover_calls(
        &mut self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        if matches!(expression, Expression::FunctionCall { .. }) {
            self.discover_expression(expression, values)?;
            return Ok(());
        }
        for child in expression_children(expression) {
            self.discover_calls(child, values)?;
        }
        Ok(())
    }

    fn discover_expression(
        &mut self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        if let Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            span,
        } = expression
            && !name.as_str().starts_with("__rumoca_named_arg__.")
        {
            return self.discover_constructor(name, args, *span, values);
        }
        if let Expression::FunctionCall {
            name, args, span, ..
        } = expression
            && enumeration_conversion(self.flat, name, args, *span)?.is_some()
        {
            // MLS §4.9.5.2: the conversion yields one enumeration value, and its
            // Integer ordinal is already proven constant by the recognizer.
            return Ok(Vec::new());
        }
        let mut resolve = |name: &rumoca_core::Reference, arguments: &[Expression], span: Span| {
            let inputs = arguments
                .iter()
                .map(|argument| self.discover_expression(argument, values))
                .collect::<Result<Vec<_>, _>>()?;
            let function = name.var_name().clone();
            let input_values = self
                .analysis
                .proven_input_values(&function, arguments, values);
            let key = FunctionSpecializationKey {
                function,
                inputs,
                input_values,
            };
            let index = self.ensure_specialization(key, span)?;
            self.analysis.certificates[index]
                .results
                .first()
                .cloned()
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function result shape",
                        format!("`{}` has no first result", name.as_str()),
                        span,
                    )
                })
        };
        expression_shape(expression, values, &mut resolve)
    }

    fn discover_constructor(
        &mut self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        span: Span,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        let constructor = self
            .flat
            .functions
            .get(name.var_name())
            .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
        if !constructor.is_constructor {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                format!("`{}` is not constructor metadata", name.as_str()),
                span,
            ));
        }
        if constructor.inputs.len() != arguments.len() {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                format!(
                    "`{}` expects {} fields but receives {}",
                    name.as_str(),
                    constructor.inputs.len(),
                    arguments.len()
                ),
                span,
            ));
        }
        let mut inputs = Vec::with_capacity(arguments.len());
        let mut fields = Vec::with_capacity(arguments.len());
        for (parameter, argument) in constructor.inputs.iter().zip(arguments) {
            let actual = self.discover_expression(argument, values)?;
            inputs.push(actual.clone());
            fields.push(resolve_declared_shape(parameter, Some(&actual), values)?);
        }
        let function = name.var_name().clone();
        let input_values = self
            .analysis
            .proven_input_values(&function, arguments, values);
        let key = FunctionSpecializationKey {
            function,
            inputs,
            input_values,
        };
        if let Some(previous) = self
            .analysis
            .constructor_fields_by_key
            .insert(key, fields.clone())
            && previous != fields
        {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                "one constructor specialization resolved to inconsistent field shapes",
                span,
            ));
        }
        Ok(Vec::new())
    }

    fn ensure_specialization(
        &mut self,
        key: FunctionSpecializationKey,
        call_span: Span,
    ) -> Result<usize, ToDaeError> {
        let caller = self.active_specializations.last().copied();
        if let Some(index) = self.analysis.certificate_by_key.get(&key).copied() {
            self.record_dependency(caller, index);
            return Ok(index);
        }
        if self.active_specializations.len() >= SPECIALIZATION_DEPTH_LIMIT {
            return Err(ToDaeError::unsupported_flat(
                "function shape specialization",
                format!(
                    "`{}` needs more than {SPECIALIZATION_DEPTH_LIMIT} nested value-proven \
                     specializations, which is the bound this analysis admits; no activation in \
                     the chain repeated an earlier proven argument",
                    key.function
                ),
                call_span,
            ));
        }
        let function =
            self.flat.functions.get(&key.function).ok_or_else(|| {
                ToDaeError::unresolved_reference(key.function.as_str(), call_span)
            })?;
        let certificate = resolve_certificate(
            self.flat,
            function,
            key.clone(),
            call_span,
            &self.analysis.model_values,
        )?;
        let index = self.analysis.certificates.len();
        self.analysis.certificate_by_key.insert(key, index);
        self.analysis.certificates.push(certificate);
        self.analysis.dependencies.push(Vec::new());
        self.record_dependency(caller, index);

        let values = self.analysis.certificates[index].values.clone();
        self.active_specializations.push(index);
        let result = (|| {
            self.discover_parameter_defaults(function, &values)?;
            self.discover_statements(&function.body, &values)
        })();
        let completed = self.active_specializations.pop();
        debug_assert_eq!(completed, Some(index));
        result?;
        Ok(index)
    }

    fn discover_parameter_defaults(
        &mut self,
        function: &rumoca_core::Function,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        let parameters = function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals);
        for default in parameters.filter_map(|parameter| parameter.default.as_ref()) {
            self.discover_calls(default, values)?;
        }
        Ok(())
    }

    fn record_dependency(&mut self, caller: Option<usize>, dependency: usize) {
        let Some(caller) = caller else {
            return;
        };
        let dependencies = &mut self.analysis.dependencies[caller];
        if !dependencies.contains(&dependency) {
            dependencies.push(dependency);
        }
    }

    fn discover_statements(
        &mut self,
        statements: &[rumoca_core::Statement],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for statement in statements {
            self.discover_statement(statement, values)?;
        }
        Ok(())
    }

    fn discover_statement(
        &mut self,
        statement: &rumoca_core::Statement,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                self.discover_component_subscripts(comp, values)?;
                self.discover_calls(value, values)
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                let mut loop_values = values.clone();
                for index in indices {
                    self.discover_calls(&index.range, &loop_values)?;
                    loop_values.insert(VarName::new(&index.ident), Vec::new());
                }
                self.discover_statements(equations, &loop_values)
            }
            rumoca_core::Statement::While { block, .. } => {
                self.discover_calls(&block.cond, values)?;
                self.discover_statements(&block.stmts, values)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => match proven_conditional_branch(cond_blocks, values) {
                // MLS §11.5 executes one branch. When this specialization
                // settles the conditions, the branches it does not execute
                // contain no call this specialization makes, so discovering
                // them would mint certificates for callees the program never
                // reaches — and would demand shape rules for statements it
                // never runs. A proven condition is itself folded by the same
                // environment, so it contains no call either.
                Some(selected) => {
                    let selected = match selected {
                        Some(ordinal) => &cond_blocks[ordinal].stmts,
                        None => else_block.as_deref().unwrap_or_default(),
                    };
                    self.discover_statements(selected, values)
                }
                None => {
                    self.discover_statement_blocks(cond_blocks, values)?;
                    self.discover_statements(else_block.as_deref().unwrap_or_default(), values)
                }
            },
            rumoca_core::Statement::When { blocks, .. } => {
                self.discover_statement_blocks(blocks, values)
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => {
                let inputs = args
                    .iter()
                    .map(|argument| self.discover_expression(argument, values))
                    .collect::<Result<Vec<_>, _>>()?;
                // A call statement that binds no output produces no value any
                // owner reads, so no specialization of the callee is executed.
                // Two such statements reach here and both are owned without
                // one: MLS §8.3.7 `assert`, a call to a predefined operator
                // the Flat function table never registers, and an initial
                // algorithm's checking call, which the initialization
                // partition replays into the assertions its body raises. Every
                // other zero-output call statement is rejected by the owner
                // check of the section it appears in, so selecting a
                // specialization here would only report a callee ahead of that
                // rejection. The arguments are ordinary expressions and were
                // discovered above either way.
                if outputs.iter().all(Option::is_none) {
                    return Ok(());
                }
                let function = comp.to_var_name();
                let input_values = self.analysis.proven_input_values(&function, args, values);
                self.ensure_specialization(
                    FunctionSpecializationKey {
                        function,
                        inputs,
                        input_values,
                    },
                    *span,
                )?;
                Ok(())
            }
            rumoca_core::Statement::Reinit {
                variable, value, ..
            } => {
                self.discover_component_subscripts(variable, values)?;
                self.discover_calls(value, values)
            }
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.discover_calls(condition, values)?;
                self.discover_calls(message, values)?;
                match level {
                    Some(level) => self.discover_calls(level, values),
                    None => Ok(()),
                }
            }
            rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. } => Ok(()),
        }
    }

    fn discover_statement_blocks(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for block in blocks {
            self.discover_calls(&block.cond, values)?;
            self.discover_statements(&block.stmts, values)?;
        }
        Ok(())
    }

    fn discover_component_subscripts(
        &mut self,
        component: &rumoca_core::ComponentReference,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for subscript in component.parts().iter().flat_map(|part| part.subs.iter()) {
            if let Subscript::Expr { expr, .. } = subscript {
                self.discover_calls(expr.as_ref(), values)?;
            }
        }
        Ok(())
    }

    fn discover_when_equations(
        &mut self,
        equations: &[flat::WhenEquation],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for equation in equations {
            self.discover_when_equation(equation, values)?;
        }
        Ok(())
    }

    fn discover_when_equation(
        &mut self,
        equation: &flat::WhenEquation,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                self.discover_calls(value, values)
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.discover_calls(condition, values)?;
                self.discover_calls(message, values)?;
                if let Some(level) = level {
                    self.discover_calls(level, values)?;
                }
                Ok(())
            }
            flat::WhenEquation::Terminate { message, .. } => self.discover_calls(message, values),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    self.discover_calls(condition, values)?;
                    self.discover_when_equations(equations, values)?;
                }
                if let Some(else_branch) = else_branch {
                    self.discover_when_equations(else_branch, values)?;
                }
                Ok(())
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                self.discover_calls(function, values)
            }
        }
    }
}

/// The shapes, and the settled translation-time values, of the model scope.
///
/// `constants` is the fixed point the phase already folds over every
/// `constant`/`parameter` binding (MLS §4.5 evaluable parameters, SPEC_0022
/// INST-007). Reading it here is what makes `Real y[m]` provable for a model
/// that declares `parameter Integer m = 3`: the extent is a parameter
/// expression in the sense MLS §12.2 admits, and its value is already known.
fn concrete_model_shapes(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ShapeEnvironment, ToDaeError> {
    let mut values = ShapeEnvironment::with_capacity(flat.variables.len());
    for (name, variable) in &flat.variables {
        let shape = concrete_dimensions(&variable.dims, variable.source_span, "model variable")?;
        match constants.get(name.as_str()) {
            Some(value) if shape.is_empty() && is_scalar_value(value) => {
                values.bind_scalar_value(name.clone(), value.clone());
            }
            _ => values.insert(name.clone(), shape),
        }
    }
    values.insert(VarName::new("time"), Vec::new());
    // MLS §4.8.5.2: an enumeration literal's semantic identity is its ordinal,
    // so a dimension written over a literal is an exact Integer extent.
    for (name, ordinal) in &flat.enum_literal_ordinals {
        values.bind_scalar_value(VarName::new(name), EvalValue::Integer(*ordinal));
    }
    Ok(values)
}

fn concrete_dimensions(
    dimensions: &[i64],
    span: Span,
    owner: &'static str,
) -> Result<ValueShape, ToDaeError> {
    dimensions
        .iter()
        .map(|extent| {
            u32::try_from(*extent).ok().ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("{owner} has non-concrete extent `{extent}`"),
                    span,
                )
            })
        })
        .collect()
}

fn resolve_certificate(
    flat: &flat::Model,
    function: &rumoca_core::Function,
    key: FunctionSpecializationKey,
    call_span: Span,
    global_values: &ShapeEnvironment,
) -> Result<FunctionShapeCertificate, ToDaeError> {
    require_span(call_span, "function specialization call")?;
    if key.inputs.len() != function.inputs.len() {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!(
                "`{}` expects {} input shapes but receives {}",
                function.name,
                function.inputs.len(),
                key.inputs.len()
            ),
            call_span,
        ));
    }
    let mut values = global_values.clone().into_specialization();
    let mut parameters = Vec::with_capacity(function.inputs.len());
    // Inputs are bound in declaration order because MLS §12.2 lets a later
    // formal's dimension read an earlier formal — both its shape and, for a
    // dimension-typed scalar, its proven value.
    for (ordinal, (parameter, actual)) in function.inputs.iter().zip(&key.inputs).enumerate() {
        let shape = resolve_declared_shape(parameter, Some(actual), &values)?;
        let name = VarName::new(&parameter.name);
        match key.input_values.get(ordinal).copied().flatten() {
            Some(value) if shape.is_empty() => {
                values.bind_scalar_value(name, value.into_settled());
            }
            _ => values.insert(name, shape.clone()),
        }
        parameters.push(shape);
    }
    let mut results = Vec::with_capacity(function.outputs.len());
    for result in &function.outputs {
        let shape = resolve_declared_shape(result, None, &values)?;
        values.insert(VarName::new(&result.name), shape.clone());
        results.push(shape);
    }
    // MLS §12.2 admits a declared dimension "given by the input formal
    // parameters ... or by constant or parameter expressions", and MLS §12.4.4
    // makes a protected declaration equation the value that holds on function
    // entry. `Integer m = size(x, 1); Real phi[m];` is that rule read
    // literally, so a local's settled declaration value is bound before the
    // later locals whose extents read it — but only when the body never assigns
    // it, because a reassigned local has no single entry value to read.
    let assigned = assigned_function_targets(&function.body);
    for local in &function.locals {
        let shape = resolve_declared_shape(local, None, &values)?;
        let name = VarName::new(&local.name);
        match local.default.as_ref() {
            Some(default)
                if shape.is_empty()
                    && !assigned.contains(&local.name)
                    && is_dimension_typed_scalar(flat, local) =>
            {
                // A declaration equation this scope cannot settle simply leaves
                // the local without a value. That is not an error here: the
                // local is still a well-shaped scalar, and any *extent* written
                // over it is rejected by name where it is read.
                match evaluate_shape_integer(default, &values).ok() {
                    Some(value) => {
                        values.bind_scalar_value(name, EvalValue::Integer(value));
                    }
                    None => values.insert(name, shape),
                }
            }
            _ => values.insert(name, shape),
        }
    }
    // MLS §12.2: a record value's declared fields are readable through the
    // joined reference identity Flat renders, so each field carries its own
    // proven shape in the same environment as the value that declares it.
    for value in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        for (path, field) in record_field_projections(value, flat) {
            let shape = resolve_declared_shape(field, None, &values)?;
            values.insert(path, shape);
        }
    }
    Ok(FunctionShapeCertificate {
        key,
        parameters,
        results,
        values,
    })
}

fn resolve_declared_shape(
    value: &rumoca_core::FunctionParam,
    actual: Option<&ValueShape>,
    values: &ShapeEnvironment,
) -> Result<ValueShape, ToDaeError> {
    if let Some(actual) = actual
        && actual.len() != value.dimensions().len()
    {
        return Err(shape_error(
            value,
            format!(
                "declared rank {} does not match call-site rank {}",
                value.dimensions().len(),
                actual.len()
            ),
        ));
    }
    let mut shape = Vec::with_capacity(value.dimensions().len());
    for (axis, declared) in value.dimensions().iter().copied().enumerate() {
        let source = value.shape_expr.get(axis);
        let resolved = if declared > 0 {
            u32::try_from(declared).map_err(|_| {
                shape_error(
                    value,
                    format!("extent `{declared}` exceeds the DAE shape domain"),
                )
            })?
        } else {
            match source {
                Some(Subscript::Colon { .. }) => actual
                    .and_then(|shape| shape.get(axis))
                    .copied()
                    .ok_or_else(|| {
                        shape_error(
                            value,
                            format!(
                                "axis {} is variable-size but has no call-site equality",
                                axis + 1
                            ),
                        )
                    })?,
                Some(Subscript::Index { value: extent, .. }) => {
                    concrete_extent(*extent, value, axis)?
                }
                Some(Subscript::Expr { expr, .. }) => {
                    let extent = evaluate_shape_integer(expr, values)?;
                    concrete_extent(extent, value, axis)?
                }
                None => {
                    return Err(shape_error(
                        value,
                        format!(
                            "axis {} uses a dynamic sentinel without a symbolic declaration",
                            axis + 1
                        ),
                    ));
                }
            }
        };
        if let Some(actual) = actual
            && actual[axis] != resolved
        {
            return Err(shape_error(
                value,
                format!(
                    "axis {} requires extent {resolved} but the call site proves {}",
                    axis + 1,
                    actual[axis]
                ),
            ));
        }
        shape.push(resolved);
    }
    Ok(shape)
}

/// Whether an input's declared type lets its *value* name an array dimension.
///
/// MLS §4.4.2 (SPEC_0022 DECL-018) admits exactly a scalar Integer,
/// enumeration, or Boolean there. A `Real`, `String`, record, or array input can
/// never appear as an extent, so its value is not part of a specialization's
/// identity.
fn is_dimension_typed_scalar(flat: &flat::Model, input: &rumoca_core::FunctionParam) -> bool {
    input.dimensions().is_empty()
        && matches!(
            effective_function_scalar_type(flat, input),
            Some(
                dae::ScalarType::Integer | dae::ScalarType::Enumeration | dae::ScalarType::Boolean
            )
        )
}

/// Whether a settled parameter value is a scalar the shape proof can read.
fn is_scalar_value(value: &EvalValue) -> bool {
    !matches!(value, EvalValue::Array(_) | EvalValue::Record(_))
}

fn concrete_extent(
    extent: i64,
    value: &rumoca_core::FunctionParam,
    axis: usize,
) -> Result<u32, ToDaeError> {
    u32::try_from(extent).ok().ok_or_else(|| {
        shape_error(
            value,
            format!("axis {} resolves to invalid extent `{extent}`", axis + 1),
        )
    })
}

fn shape_error(value: &rumoca_core::FunctionParam, detail: impl Into<String>) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function shape proof",
        format!("`{}`: {}", value.name, detail.into()),
        value.span,
    )
}

pub(super) fn evaluate_shape_integer(
    expression: &Expression,
    values: &ShapeEnvironment,
) -> Result<i64, ToDaeError> {
    let span = expression_span(expression)?;
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Ok(*value),
        Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            args,
            ..
        } => {
            let [array, dimension] = args.as_slice() else {
                return Err(ToDaeError::unsupported_flat(
                    "function shape proof",
                    "a dependent extent requires size(value, literal_axis)",
                    span,
                ));
            };
            let axis = evaluate_shape_integer(dimension, values)?;
            let axis = usize::try_from(axis)
                .ok()
                .and_then(|axis| axis.checked_sub(1))
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        "size axis must be a positive integer",
                        span,
                    )
                })?;
            let shape = expression_shape(array, values, &mut reject_shape_call)?;
            shape.get(axis).copied().map(i64::from).ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("size axis {} exceeds rank {}", axis + 1, shape.len()),
                    span,
                )
            })
        }
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => evaluate_shape_integer(rhs, values),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => evaluate_shape_integer(rhs, values)?
            .checked_neg()
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "dependent extent arithmetic overflowed",
                    span,
                )
            }),
        Expression::Binary { op, lhs, rhs, .. } => {
            let (Ok(lhs), Ok(rhs)) = (
                evaluate_shape_integer(lhs, values),
                evaluate_shape_integer(rhs, values),
            ) else {
                // One operand is not itself an extent — `m/2` under
                // `integer(m/2)` is Real by MLS §10.6.6 — so the whole
                // expression is folded by the translation-time evaluator.
                return proven_extent(expression, values, span);
            };
            // The structural rules cover the operators an extent is usually
            // written with; `n^2` and the rest are still MLS §4.4.2 evaluable
            // expressions, so the whole node goes to the evaluator rather than
            // being reported as if it had no value.
            checked_shape_arithmetic(op.clone(), lhs, rhs, span)
                .or_else(|error| proven_extent(expression, values, span).map_err(|_| error))
        }
        // A scalar coordinate this environment proves the *shape* of carries an
        // extent only when its value is also proven. MLS §12.2 accepts the
        // dimension either way; what differs is whether the call site settled
        // the value. Naming that cause keeps the rejection honest: the missing
        // owner is a value-proven specialization, not a malformed extent.
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && values.get(name.var_name()).is_some_and(Vec::is_empty) => {
            values
                .proven_value(expression)
                .and_then(ProvenValue::extent)
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        format!(
                            "extent depends on the value of scalar `{}`, which requires a \
                             value-proven function specialization",
                            name.as_str()
                        ),
                        span,
                    )
                })
        }
        _ => proven_extent(expression, values, span),
    }
}

/// Fold an extent the structural rules above do not decompose.
///
/// MLS §4.4.2 requires an array dimension to be an *evaluable* expression, so
/// the honest last resort is the same translation-time evaluator the phase
/// already uses for parameter bindings — `integer(m/2)`, `mod(m, 2)`,
/// `if n > 0 then n else 1` are extents exactly when it settles them.
fn proven_extent(
    expression: &Expression,
    values: &ShapeEnvironment,
    span: Span,
) -> Result<i64, ToDaeError> {
    values
        .proven_value(expression)
        .and_then(ProvenValue::extent)
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "dependent extent is not an exact Integer expression over proven shape axes",
                span,
            )
        })
}

fn checked_shape_arithmetic(
    operator: OpBinary,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, ToDaeError> {
    let result = match operator {
        OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
        OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
        OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
        OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => lhs.checked_div(rhs),
        _ => None,
    };
    result.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            "dependent extent arithmetic is non-integral, unsupported, or overflowing",
            span,
        )
    })
}

#[cfg(test)]
mod tests {
    use rumoca_core::{EffectiveType, Reference, SourceMap, Subscript, TypeId};

    use super::*;

    fn literal(value: f64, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span,
        }
    }

    fn array(extent: usize, span: Span) -> Expression {
        Expression::Array {
            elements: (0..extent)
                .map(|ordinal| literal(ordinal as f64, span))
                .collect(),
            is_matrix: false,
            span,
        }
    }

    fn real_param(name: &str, dimensions: Vec<i64>, span: Span) -> rumoca_core::FunctionParam {
        let value_type = EffectiveType::new(TypeId::new(1), TypeId::new(1), dimensions)
            .expect("fixture function type is resolved");
        rumoca_core::FunctionParam::new(name, "Real", value_type, span)
    }

    fn identity_function(span: Span, result_has_shape_equality: bool) -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new("identity", span);
        function.add_input(
            real_param("u", vec![0], span).with_shape_expr(vec![Subscript::colon(span)]),
        );
        let mut output = real_param("y", vec![0], span);
        if result_has_shape_equality {
            output = output.with_shape_expr(vec![Subscript::expr(
                Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: vec![
                        Expression::VarRef {
                            name: Reference::new("u"),
                            subscripts: Vec::new(),
                            span,
                        },
                        Expression::Literal {
                            value: Literal::Integer(1),
                            span,
                        },
                    ],
                    span,
                }),
                span,
            )]);
        }
        function.add_output(output);
        function
    }

    fn call(extent: usize, span: Span) -> flat::Equation {
        flat::Equation::new(
            Expression::FunctionCall {
                name: Reference::new("identity"),
                args: vec![array(extent, span)],
                is_constructor: false,
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        )
    }

    #[test]
    fn record_constructor_arity_remains_strict() {
        let mut sources = SourceMap::new();
        let source = sources.add("record_arity.mo", "Pair(1.0);");
        let span = Span::from_offsets(source, 0, 9);
        let mut constructor = rumoca_core::Function::new("Pair", span);
        constructor.is_constructor = true;
        constructor.add_input(real_param("left", Vec::new(), span));
        constructor.add_input(real_param("right", Vec::new(), span));

        let mut model = flat::Model::new();
        model.add_function(constructor);
        model.add_equation(flat::Equation::new(
            Expression::FunctionCall {
                name: Reference::new("Pair"),
                args: vec![literal(1.0, span)],
                is_constructor: true,
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        ));

        let Err(error) = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) else {
            panic!("record constructor with one missing field must be rejected");
        };
        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics {
                feature,
                detail,
                span: error_span,
            } if feature == "record constructor"
                && detail == "`Pair` expects 2 fields but receives 1"
                && error_span == span
        ));
    }

    #[test]
    fn reachable_calls_receive_distinct_concrete_shape_certificates() {
        let mut sources = SourceMap::new();
        let source = sources.add("shape.mo", "identity({1,2}); identity({1,2,3});");
        let first = Span::from_offsets(source, 0, 15);
        let second = Span::from_offsets(source, 17, 34);
        let mut model = flat::Model::new();
        model.add_function(identity_function(first, true));
        model.add_equation(call(2, first));
        model.add_equation(call(3, second));

        let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
        let certificates = analysis.certificates();
        assert_eq!(certificates.len(), 2);
        assert_eq!(certificates[0].parameters, vec![vec![2]]);
        assert_eq!(certificates[0].results, vec![vec![2]]);
        assert_eq!(certificates[1].parameters, vec![vec![3]]);
        assert_eq!(certificates[1].results, vec![vec![3]]);
    }

    #[test]
    fn empty_array_call_has_a_zero_extent_shape_certificate() {
        let mut sources = SourceMap::new();
        let source = sources.add("empty_shape.mo", "identity({});");
        let span = Span::from_offsets(source, 0, 13);
        let mut model = flat::Model::new();
        model.add_function(identity_function(span, true));
        model.add_equation(call(0, span));

        let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
        let [certificate] = analysis.certificates() else {
            panic!("empty array call should have one shape certificate");
        };
        assert_eq!(certificate.parameters, vec![vec![0]]);
        assert_eq!(certificate.results, vec![vec![0]]);
    }

    #[test]
    fn unresolved_result_axis_is_rejected_at_analysis() {
        let mut sources = SourceMap::new();
        let source = sources.add("shape_error.mo", "identity({1,2});");
        let span = Span::from_offsets(source, 0, 15);
        let mut model = flat::Model::new();
        model.add_function(identity_function(span, false));
        model.add_equation(call(2, span));

        let error = match FunctionShapeAnalysis::analyze(&model, &EvalContext::new()) {
            Ok(_) => panic!("an unresolved result axis must not produce a certificate"),
            Err(error) => error,
        };
        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics { feature, span: error_span, .. }
                if feature == "function shape proof" && error_span == span
        ));
    }
}
