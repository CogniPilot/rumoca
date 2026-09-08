use super::*;

/// Which declaration list [`FunctionEnv::declare_all`] is binding.
#[derive(Clone, Copy)]
enum DeclaredKind {
    Output,
    Local,
}

/// The order a function's outputs and locals may be bound in on entry.
///
/// MLS 3.6 §12.4.4: the declaration bindings "are executed in an order where a
/// variable is not used before its binding"; the *only* error the rule names is
/// that no such order exists. So this is a topological sort over Resolve-issued
/// declaration identities, seeded in written order so an independent set keeps
/// its declaration order, with a cycle reported by name.
///
/// A declaration's reads include the names its written extent mentions
/// (MLS §12.2), because the shaped default cannot be built before them either.
fn binding_order(
    func: &Function,
    span: Span,
) -> Result<Vec<(DeclaredKind, &rumoca_core::FunctionParam)>, EvalError> {
    let declarations: Vec<(DeclaredKind, &rumoca_core::FunctionParam)> = func
        .outputs
        .iter()
        .map(|output| (DeclaredKind::Output, output))
        .chain(func.locals.iter().map(|local| (DeclaredKind::Local, local)))
        .collect();
    let mut position = rustc_hash::FxHashMap::default();
    for (index, (_, param)) in declarations.iter().enumerate() {
        let Some(def_id) = param.def_id else {
            return Err(EvalError::InvalidSemanticIr {
                reason: format!("function declaration `{}` has no DefId", param.name),
                span: param.span,
            });
        };
        if position.insert(def_id, index).is_some() {
            return Err(EvalError::InvalidSemanticIr {
                reason: format!("function declaration DefId {def_id} is issued more than once"),
                span: param.span,
            });
        }
    }

    let mut ordered = Vec::with_capacity(declarations.len());
    let mut state = vec![VisitState::Unvisited; declarations.len()];
    for index in 0..declarations.len() {
        visit_declaration(
            index,
            &declarations,
            &position,
            &mut state,
            &mut ordered,
            span,
        )?;
    }
    Ok(ordered
        .into_iter()
        .map(|index| declarations[index])
        .collect())
}

#[derive(Clone, Copy, PartialEq)]
enum VisitState {
    Unvisited,
    InProgress,
    Placed,
}

fn visit_declaration(
    index: usize,
    declarations: &[(DeclaredKind, &rumoca_core::FunctionParam)],
    position: &rustc_hash::FxHashMap<rumoca_core::DefId, usize>,
    state: &mut [VisitState],
    ordered: &mut Vec<usize>,
    span: Span,
) -> Result<(), EvalError> {
    match state[index] {
        VisitState::Placed => return Ok(()),
        VisitState::InProgress => {
            return Err(EvalError::CircularDependency {
                path: declarations[index].1.name.clone(),
                span,
            });
        }
        VisitState::Unvisited => {}
    }
    state[index] = VisitState::InProgress;
    for read in declaration_reads(declarations[index].1)? {
        let Some(dependency) = position.get(&read).copied() else {
            continue;
        };
        visit_declaration(dependency, declarations, position, state, ordered, span)?;
    }
    state[index] = VisitState::Placed;
    ordered.push(index);
    Ok(())
}

/// Every resolved declaration identity read by `param`'s binding and extents.
fn declaration_reads(
    param: &rumoca_core::FunctionParam,
) -> Result<Vec<rumoca_core::DefId>, EvalError> {
    let mut reads = ReferenceCollector::default();
    if let Some(default) = &param.default {
        reads.visit_expression(default);
    }
    for subscript in &param.shape_expr {
        reads.visit_subscript(subscript);
    }
    if let Some(reference) = reads.identity_free_reference {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "function declaration `{}` reads identity-free reference `{reference}`",
                param.name
            ),
            span: param.span,
        });
    }
    Ok(reads.identities)
}

#[derive(Default)]
struct ReferenceCollector {
    identities: Vec<rumoca_core::DefId>,
    identity_free_reference: Option<String>,
}

impl rumoca_core::ExpressionVisitor for ReferenceCollector {
    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        if let Some(def_id) = name.root_def_id() {
            self.identities.push(def_id);
        } else if name.structured_binder().is_none() && self.identity_free_reference.is_none() {
            self.identity_free_reference = Some(name.as_str().to_string());
        }
        self.walk_var_ref(name, subscripts);
    }
}

/// Function execution environment with mutable variable bindings.
#[derive(Clone)]
pub(super) struct FunctionEnv {
    /// Exact source identities of this function's declarations. Values remain
    /// in the role-specific maps below; this table only selects the declaration
    /// a resolved read occurrence denotes.
    pub(super) declarations: Vec<(Option<rumoca_core::DefId>, String)>,
    /// Evaluated input extent obligations, retained in formal order after all
    /// explicit and default inputs are bound.
    pub(super) formal_extents: Vec<Vec<i64>>,
    /// Active lexical for-index bindings, from outer-most to inner-most.
    /// Iterators may hide function declarations and each other (MLS §11.2),
    /// so they cannot share the ordinary local map whose lookup follows
    /// inputs and outputs.
    pub(super) loop_bindings: Vec<(String, Value)>,
    /// Input parameters (bound from arguments).
    pub(super) inputs: IndexMap<String, Value>,
    /// Output variables (assigned by function body).
    pub(super) outputs: IndexMap<String, Value>,
    /// Local/protected variables.
    pub(super) locals: IndexMap<String, Value>,
    /// Declared slots are distinct from initialized values. This lets a record
    /// output be assigned by the body without inventing a scalar placeholder.
    pub(super) declared_outputs: IndexSet<String>,
    pub(super) declared_locals: IndexSet<String>,
}

/// Function-call argument after expression evaluation.
#[cfg(test)]
#[derive(Debug, Clone)]
pub(super) struct FunctionCallArg {
    name: Option<String>,
    value: Value,
}

#[cfg(test)]
impl FunctionCallArg {
    pub(super) fn positional(value: Value) -> Self {
        Self { name: None, value }
    }

    pub(super) fn named(name: String, value: Value) -> Self {
        Self {
            name: Some(name),
            value,
        }
    }
}

impl FunctionEnv {
    #[cfg(test)]
    pub(super) fn new_with_call_args(
        func: &Function,
        args: Vec<FunctionCallArg>,
        eval: &EvalState<'_>,
    ) -> Result<Self, EvalError> {
        let inputs = Self::bind_inputs(func, args, eval.span)?;
        let mut env = Self::with_inputs(func, inputs);
        env.bind_omitted_input_defaults(func, eval)?;
        env.evaluate_input_extents(func, eval)?;
        env.declare_all(func, eval)?;
        Ok(env)
    }

    pub(super) fn new_with_checked_call(
        call: &crate::constant::EvaluatedCall<'_>,
        eval: &EvalState<'_>,
    ) -> Result<Self, EvalError> {
        let plan = call.plan();
        let func = plan.function();
        let mut inputs = IndexMap::new();
        for (index, param) in func.inputs.iter().enumerate() {
            if let Some(value) = call.explicit_value(index) {
                inputs.insert(param.name.clone(), value.clone());
            }
        }
        let mut env = Self::with_inputs(func, inputs);
        for &index in plan.default_order() {
            let Some(default) = plan.default(index) else {
                return Err(EvalError::Internal {
                    message: "checked default order selected an explicit actual".to_string(),
                });
            };
            let value = eval_expr_in_function(default, &env, eval)?;
            env.inputs.insert(func.inputs[index].name.clone(), value);
        }
        env.evaluate_input_extents(func, eval)?;
        env.declare_all(func, eval)?;
        Ok(env)
    }

    fn with_inputs(func: &Function, inputs: IndexMap<String, Value>) -> Self {
        Self {
            declarations: func
                .inputs
                .iter()
                .chain(func.outputs.iter())
                .chain(func.locals.iter())
                .map(|param| (param.def_id, param.name.clone()))
                .collect(),
            formal_extents: vec![Vec::new(); func.inputs.len()],
            loop_bindings: Vec::new(),
            inputs,
            outputs: IndexMap::new(),
            locals: IndexMap::new(),
            declared_outputs: func
                .outputs
                .iter()
                .map(|param| param.name.clone())
                .collect(),
            declared_locals: func.locals.iter().map(|param| param.name.clone()).collect(),
        }
    }

    fn evaluate_input_extents(
        &mut self,
        func: &Function,
        eval: &EvalState<'_>,
    ) -> Result<(), EvalError> {
        for (index, param) in func.inputs.iter().enumerate() {
            let Some(value) = self.inputs.get(&param.name) else {
                return Err(EvalError::Internal {
                    message: format!("bound call has no value for input `{}`", param.name),
                });
            };
            let actual = if matches!(value, Value::Array(_)) {
                crate::constant::value::checked_rectangular_shape(
                    value,
                    &param.type_name,
                    param.dimensions().len(),
                    param.span,
                )?
            } else {
                Vec::new()
            };
            // `declared_dimensions` settles each written dimension to an
            // integer and hands `:` its retained sentinel. The obligation
            // kinds are read from the formal, so an unspecified dimension
            // binds the actual's extent and every other one must equal it.
            let expected = self.declared_dimensions(param, eval)?;
            self.formal_extents[index] =
                crate::constant::bind_formal_extents(param, &expected, &actual)?;
        }
        Ok(())
    }

    /// Bind every omitted input formal to its declared default (MLS §12.4.1).
    ///
    /// A default is an expression over the function's other formals, and MLS
    /// §12.4.4's ordering rule applies: the defaults run in an order where no
    /// formal is read before its binding, an error being reported only when
    /// no such order exists.
    ///
    /// The order is established by dependency proof, never by trying an
    /// evaluation: every name a function declares shadows the outer scope
    /// for the whole body (MLS §12.2), including while its own binding is
    /// still pending, so evaluating a default whose reads are unproven could
    /// fall through a pending formal to a same-named context value and bind
    /// a value the program never selected. A default is evaluated only once
    /// every declared-name root it reads is bound in this environment;
    /// reads of undeclared names legitimately consult the enclosing
    /// context. When no pending default is provably ready, the cycle is
    /// reported. An evaluation error fails the call closed — a type zero is
    /// never substituted for a declared default.
    #[cfg(test)]
    fn bind_omitted_input_defaults(
        &mut self,
        func: &Function,
        eval: &EvalState<'_>,
    ) -> Result<(), EvalError> {
        let mut pending: Vec<&rumoca_core::FunctionParam> = func
            .inputs
            .iter()
            .filter(|param| !self.inputs.contains_key(&param.name))
            .collect();
        while !pending.is_empty() {
            let ready = pending
                .iter()
                .position(|param| self.omitted_default_is_ready(param, func));
            let Some(ready) = ready else {
                return Err(EvalError::CircularDependency {
                    path: pending[0].name.clone(),
                    span: eval.span,
                });
            };
            let param = pending.remove(ready);
            let Some(default) = param.default.as_ref() else {
                return Err(EvalError::Internal {
                    message: "unbound formal has no default".to_string(),
                });
            };
            let value = eval_expr_in_function(default, self, eval)?;
            self.inputs.insert(param.name.clone(), value);
        }
        Ok(())
    }

    #[cfg(test)]
    fn omitted_default_is_ready(
        &self,
        param: &rumoca_core::FunctionParam,
        func: &Function,
    ) -> bool {
        crate::constant::default_dependencies_bound(param, func, |def_id| {
            self.bound_input_has_identity(func, def_id)
        })
    }

    #[cfg(test)]
    fn bound_input_has_identity(&self, func: &Function, def_id: rumoca_core::DefId) -> bool {
        func.inputs
            .iter()
            .any(|input| input.def_id == Some(def_id) && self.inputs.contains_key(&input.name))
    }

    /// Bind input arguments to parameters.
    #[cfg(test)]
    fn bind_inputs(
        func: &Function,
        args: Vec<FunctionCallArg>,
        span: Span,
    ) -> Result<IndexMap<String, Value>, EvalError> {
        let mut inputs = IndexMap::new();
        let mut next_positional = 0;
        let mut seen_named = false;

        for arg in args {
            match arg.name {
                Some(name) => {
                    seen_named = true;
                    Self::bind_named_input(func, &mut inputs, name, arg.value, span)?;
                }
                None => {
                    Self::bind_positional_input(
                        func,
                        &mut inputs,
                        next_positional,
                        seen_named,
                        arg.value,
                        span,
                    )?;
                    next_positional += 1;
                }
            }
        }

        for param in &func.inputs {
            if inputs.contains_key(&param.name) || param.default.is_some() {
                // An omitted formal with a declared default is bound by
                // bind_omitted_input_defaults once the explicit actuals are
                // in place.
                continue;
            }
            return Err(EvalError::function_error(
                format!(
                    "missing required argument {} for function {}",
                    param.name, func.name
                ),
                span,
            ));
        }
        Ok(inputs)
    }

    #[cfg(test)]
    fn bind_named_input(
        func: &Function,
        inputs: &mut IndexMap<String, Value>,
        name: String,
        value: Value,
        span: Span,
    ) -> Result<(), EvalError> {
        if !func.inputs.iter().any(|param| param.name == name) {
            return Err(EvalError::function_error(
                format!("unknown named argument {name} for function {}", func.name),
                span,
            ));
        }
        if inputs.insert(name.clone(), value).is_some() {
            return Err(EvalError::function_error(
                format!("duplicate argument {name} for function {}", func.name),
                span,
            ));
        }
        Ok(())
    }

    #[cfg(test)]
    fn bind_positional_input(
        func: &Function,
        inputs: &mut IndexMap<String, Value>,
        next_positional: usize,
        seen_named: bool,
        value: Value,
        span: Span,
    ) -> Result<(), EvalError> {
        if seen_named {
            return Err(EvalError::function_error(
                format!("positional argument after named argument in {}", func.name),
                span,
            ));
        }
        let Some(param) = func.inputs.get(next_positional) else {
            return Err(EvalError::function_error(
                format!("too many arguments for function {}", func.name),
                span,
            ));
        };
        if inputs.insert(param.name.clone(), value).is_some() {
            return Err(EvalError::function_error(
                format!(
                    "duplicate argument {} for function {}",
                    param.name, func.name
                ),
                span,
            ));
        }
        Ok(())
    }

    /// Bind a function's declared outputs and locals on entry.
    ///
    /// MLS 3.6 §12.4.4 makes a declaration equation inside a function the value
    /// the component holds on entry, and fixes the order they run in: the
    /// bindings "are executed in an order where a variable is not used before
    /// its binding", an error being reported only when no such order exists.
    /// That is a topological order over the declarations, not the written one —
    /// `Integer a = b + 1; Integer b = 2;` is a legal acyclic program OMC folds
    /// to `3`, so evaluating in declaration order would refuse it. A cycle is
    /// the one case §12.4.4 calls an error, and it is reported by name.
    ///
    /// Substituting a type default for a declaration that *has* a binding is
    /// what silently folded `Integer mBasic = integer(m/n)` to `0`, so a
    /// binding that cannot be evaluated propagates its error and the whole call
    /// refuses to fold: there is no second value the entry state could take.
    ///
    /// A primitive declaration without a binding gets the zero of its declared
    /// shape and therefore has the container a later `y[i] := …` writes into.
    /// A structured declaration remains a declared but uninitialized slot
    /// until a whole-value assignment supplies it; no scalar stand-in is
    /// fabricated for a record, enumeration, or other user type.
    pub(super) fn declare_all(
        &mut self,
        func: &Function,
        eval: &EvalState<'_>,
    ) -> Result<(), EvalError> {
        for (kind, param) in binding_order(func, eval.span)? {
            let value = match &param.default {
                Some(default) => Some(eval_expr_in_function(default, self, eval)?),
                None => self.shaped_default(param, eval)?,
            };
            if let Some(value) = value {
                match kind {
                    DeclaredKind::Output => self.outputs.insert(param.name.clone(), value),
                    DeclaredKind::Local => self.locals.insert(param.name.clone(), value),
                };
            }
        }
        Ok(())
    }

    /// The zero of `param`'s declared shape, for a declaration with no binding.
    ///
    /// A declared extent is now read from the call's own bound inputs
    /// (MLS §12.2), so the size of the container about to be allocated is
    /// decided by model values. The evaluator's element budget bounds that
    /// decision and reports a form it will not fold rather than materializing
    /// whatever the model asked for.
    pub(super) fn shaped_default(
        &self,
        param: &rumoca_core::FunctionParam,
        eval: &EvalState<'_>,
    ) -> Result<Option<Value>, EvalError> {
        let Some(scalar) = scalar_type_default(&param.type_name) else {
            return Ok(None);
        };
        let dimensions = self.declared_dimensions(param, eval)?;
        let Ok(host_dimensions) = dimensions
            .iter()
            .map(|extent| usize::try_from(*extent))
            .collect::<Result<Vec<_>, _>>()
        else {
            return Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "declared extent {dimensions:?} of `{}` is beyond the \
                     host index range",
                    param.name
                ),
                span: eval.span,
            });
        };
        if host_dimensions.len() > DEFAULT_MATERIALIZED_RANK_BUDGET {
            return Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "declared rank of `{}` exceeds the constant-evaluation rank \
                     budget of {DEFAULT_MATERIALIZED_RANK_BUDGET}",
                    param.name
                ),
                span: eval.span,
            });
        }
        if !rectangular_shape_is_representable(&host_dimensions) {
            return Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "declared extent {dimensions:?} of `{}` contains a zero \
                     before the final dimension, which the compatibility value \
                     representation cannot retain",
                    param.name
                ),
                span: eval.span,
            });
        }
        if rectangular_materialized_node_count(&host_dimensions, 1)
            .is_none_or(|nodes| nodes > eval.limits.max_iterations)
        {
            return Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "declared extent {dimensions:?} of `{}` is beyond the \
                     constant-evaluation node budget",
                    param.name
                ),
                span: eval.span,
            });
        }
        Ok(Some(if dimensions.is_empty() {
            scalar
        } else {
            create_array_value(&scalar, &dimensions)
        }))
    }

    /// The declared extent of `param` in the environment bound so far.
    ///
    /// MLS 3.6 §12.2 admits a function component's array dimension "given by
    /// the input formal parameters", so `output Real orientation[m]` only has
    /// an extent once `m` is bound. `effective_type` can only carry the extent
    /// of a declaration whose dimensions are literal, and reports `0` for the
    /// rest; the written `shape_expr` is what actually names `m`.
    ///
    /// An extent this environment cannot settle refuses the call, exactly as an
    /// unsettleable binding does. Falling back to the declared `0` built an
    /// *empty* container that `size()` and a later loop then read as the
    /// component's real extent — a wrong value, not a missing one.
    pub(super) fn declared_dimensions(
        &self,
        param: &rumoca_core::FunctionParam,
        eval: &EvalState<'_>,
    ) -> Result<Vec<i64>, EvalError> {
        let declared = param.dimensions();
        if param.shape_expr.len() != declared.len() {
            return Ok(declared.to_vec());
        }
        param
            .shape_expr
            .iter()
            .zip(declared)
            .map(|(subscript, fallback)| self.declared_extent(param, subscript, *fallback, eval))
            .collect()
    }

    /// One declared dimension of `param`.
    pub(super) fn declared_extent(
        &self,
        param: &rumoca_core::FunctionParam,
        subscript: &Subscript,
        fallback: i64,
        eval: &EvalState<'_>,
    ) -> Result<i64, EvalError> {
        let extent = match subscript {
            Subscript::Index { value, .. } => *value,
            // A written dimension that is a plain `:` carries no extent at all,
            // so there is nothing for this environment to settle.
            Subscript::Colon { .. } => fallback,
            Subscript::Expr { expr, .. } => {
                let value = eval_expr_in_function(expr, self, eval)?;
                value.as_integer().ok_or_else(|| {
                    EvalError::type_mismatch("Integer", value.type_name(), eval.span)
                })?
            }
        };
        if extent < 0 {
            return Err(EvalError::function_error(
                format!(
                    "declared dimension of `{}` evaluates to the negative extent {extent}",
                    param.name
                ),
                eval.span,
            ));
        }
        Ok(extent)
    }

    /// Look up a variable by lexical scope, then declaration class.
    pub(super) fn get(&self, name: &str) -> Option<&Value> {
        self.loop_bindings
            .iter()
            .rev()
            .find_map(|(binding, value)| (binding == name).then_some(value))
            .or_else(|| self.inputs.get(name))
            .or_else(|| self.outputs.get(name))
            .or_else(|| self.locals.get(name))
    }

    /// The active iterator bound under `name` that `reference` may denote.
    ///
    /// Flat carries a `for`/comprehension iterator only as the lexical name on
    /// its index declaration, while Resolve issues the iterator's declaration
    /// identity to the read occurrences alone: such a read carries that
    /// identity as its root `DefId` and never an occurrence `InstanceId`,
    /// because an iterator is no Flat variable. The environment therefore
    /// cannot compare declaration identities and matches the read against the
    /// active bindings by spelling, which MLS §11.2.2.1 and §10.4.1 make
    /// sufficient inside the binding's extent: the innermost iterator of a
    /// spelling hides every outer meaning of it, so Resolve bound any
    /// unqualified read of that spelling written there to the iterator. An
    /// occurrence identity proves the read names a model variable instead, and
    /// no iterator is ever one.
    pub(super) fn iterator_binding(
        &self,
        name: &str,
        reference: &rumoca_core::Reference,
    ) -> Option<&Value> {
        if reference.instance_id().is_some() {
            return None;
        }
        self.loop_bindings
            .iter()
            .rev()
            .find_map(|(binding, value)| (binding == name).then_some(value))
    }

    /// Select a function declaration by resolved identity. A foreign resolved
    /// id remains an outer reference even when its display spelling equals a
    /// formal; an active iterator of the same spelling is the one exception,
    /// see [`Self::iterator_binding`].
    pub(super) fn get_reference(&self, reference: &rumoca_core::Reference) -> Option<&Value> {
        if let Some(value) = self.iterator_binding(reference.as_str(), reference) {
            return Some(value);
        }
        let name = self.selected_declaration_name(reference)?;
        self.get(name)
    }

    pub(super) fn selected_declaration_name<'a>(
        &'a self,
        reference: &rumoca_core::Reference,
    ) -> Option<&'a str> {
        let read = reference.root_def_id()?;
        self.declarations
            .iter()
            .find_map(|(declared, name)| (*declared == Some(read)).then_some(name.as_str()))
    }

    pub(super) fn is_declared_reference(&self, reference: &rumoca_core::Reference) -> bool {
        self.iterator_binding(reference.as_str(), reference)
            .is_some()
            || self
                .selected_declaration_name(reference)
                .is_some_and(|name| self.is_declared(name))
    }

    /// Set a variable value (must be output or local, not input).
    pub(super) fn set(&mut self, name: &str, value: Value) -> bool {
        if self
            .loop_bindings
            .iter()
            .rev()
            .any(|(binding, _)| binding == name)
        {
            return false;
        }
        if self.declared_outputs.contains(name) {
            self.outputs.insert(name.to_string(), value);
            true
        } else if self.declared_locals.contains(name) {
            self.locals.insert(name.to_string(), value);
            true
        } else {
            false
        }
    }

    pub(super) fn is_declared(&self, name: &str) -> bool {
        self.loop_bindings
            .iter()
            .rev()
            .any(|(binding, _)| binding == name)
            || self.inputs.contains_key(name)
            || self.declared_outputs.contains(name)
            || self.declared_locals.contains(name)
    }

    /// Get the return value (single output or tuple of outputs).
    pub(super) fn return_value(&self, span: Span) -> Result<Value, EvalError> {
        let values =
            self.declared_outputs
                .iter()
                .map(|name| {
                    self.outputs.get(name).cloned().ok_or_else(|| {
                        EvalError::UnsupportedExpression {
                            kind: format!("function output `{name}` is not initialized"),
                            span,
                        }
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        if let [value] = values.as_slice() {
            Ok(value.clone())
        } else {
            Ok(Value::Array(values))
        }
    }
}

/// Create a default value for a given type.
fn scalar_type_default(type_name: &str) -> Option<Value> {
    match type_name {
        "Real" => Some(Value::Real(0.0)),
        "Integer" => Some(Value::Integer(0)),
        "Boolean" => Some(Value::Bool(false)),
        "String" => Some(Value::String(String::new())),
        _ => None,
    }
}

/// Create a multi-dimensional array filled with a default value.
fn create_array_value(default: &Value, dims: &[i64]) -> Value {
    if dims.is_empty() {
        default.clone()
    } else {
        let size = dims[0] as usize;
        if size == 0 {
            return Value::Array(Vec::new());
        }
        let inner = create_array_value(default, &dims[1..]);
        Value::Array(vec![inner; size])
    }
}
