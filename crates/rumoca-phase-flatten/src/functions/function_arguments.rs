//! Static specialization of functions over their function-valued arguments.
//!
//! MLS 3.7 §12.4.2 lets a function declare an input whose type is a function
//! (`input Equation f`), and §12.4.2.1 lets a call pass either a function name
//! or a partial application `function g(p = p, h = h)`, which "returns a
//! partially evaluated function ... with the remaining not bound formal
//! parameters still present in the same order". The executable IR has no
//! function values, so every call that passes one is resolved here, once, at
//! construction: the callee is specialized per distinct function argument,
//! its calls of the formal function become direct calls of the argument's
//! function, and each bound formal of a partial application becomes an extra
//! input of the specialization that the call site passes as an ordinary value.
//! Nothing is dispatched at run time.

use super::*;
use rumoca_core::{FallibleExpressionRewriter, FallibleStatementRewriter, Span};

const MAX_SPECIALIZATION_ROUNDS: usize = 16;

/// Specialize every call that passes a function argument, to a fixed point,
/// then drop the generic callees no call reaches any more.
pub(crate) fn specialize_function_arguments(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let mut next_instance = flat
        .functions
        .values()
        .filter_map(|function| function.instance_id)
        .map(|id| id.index())
        .max()
        .map_or(0, |max| max + 1);
    // A function with a functional input runs only through its
    // specializations, so its own body (which passes its formal on) is never
    // rewritten, and every remaining call must pass a function.
    let mut generics = IndexMap::default();
    flat.functions.retain(|name, function| {
        let generic = has_function_input(function);
        if generic {
            generics.insert(name.clone(), function.clone());
        }
        !generic
    });
    for _ in 0..MAX_SPECIALIZATION_ROUNDS {
        let mut functions = flat.functions.clone();
        functions.extend(
            generics
                .iter()
                .map(|(name, function)| (name.clone(), function.clone())),
        );
        let mut specializer = Specializer {
            functions,
            created: IndexMap::default(),
            next_instance,
        };
        super::call_args::rewrite_model_expressions(flat, &mut specializer)?;
        next_instance = specializer.next_instance;
        if specializer.created.is_empty() {
            // Specialized bodies call the argument functions with named
            // bound formals; give those calls their declaration-order slots.
            return super::call_args::materialize_flat_function_call_args(flat);
        }
        for (name, function) in specializer.created {
            flat.functions.insert(name, function);
        }
    }
    Err(FlattenError::internal(format!(
        "function-argument specialization did not converge after {MAX_SPECIALIZATION_ROUNDS} \
         rounds (function arguments nested too deeply or recursively)"
    )))
}

fn has_function_input(function: &rumoca_core::Function) -> bool {
    function
        .inputs
        .iter()
        .any(|input| input.type_class == Some(rumoca_core::ClassType::Function))
}

/// A function argument: the function it names and the formals a partial
/// application binds, in written order.
struct FunctionArgument {
    target: rumoca_core::Reference,
    bound: Vec<(String, rumoca_core::Expression)>,
}

struct Specializer {
    functions: IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    created: IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    next_instance: u32,
}

impl Specializer {
    fn function_argument(
        &self,
        callee: &rumoca_core::Function,
        input: &rumoca_core::FunctionParam,
        argument: &rumoca_core::Expression,
        span: Span,
    ) -> Result<FunctionArgument, FlattenError> {
        let refusal = || {
            FlattenError::invalid_function_call_args(
                callee.name.as_str(),
                format!(
                    "function argument `{}` must be a function name or a partial application \
                     `function f(...)` (MLS 3.7 §12.4.2.1)",
                    input.name
                ),
                span,
            )
        };
        match argument {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Ok(FunctionArgument {
                target: self.named_function(name).ok_or_else(refusal)?,
                bound: Vec::new(),
            }),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                ..
            } if self.functions.contains_key(name.var_name()) => {
                let bound = args
                    .iter()
                    .map(|arg| named_association(arg).ok_or_else(refusal))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(FunctionArgument {
                    target: name.clone(),
                    bound,
                })
            }
            _ => Err(refusal()),
        }
    }

    /// The function a bare name argument denotes: the Flat function of that
    /// name, or the one function whose declaration the name resolved to.
    fn named_function(&self, name: &rumoca_core::Reference) -> Option<rumoca_core::Reference> {
        let function = self.functions.get(name.var_name()).or_else(|| {
            let declaration = name.target_def_id()?;
            let mut matches = self
                .functions
                .values()
                .filter(|function| function.def_id == Some(declaration));
            let function = matches.next()?;
            matches.next().is_none().then_some(function)
        })?;
        let instance_id = function.instance_id?;
        Some(
            rumoca_core::Reference::from_var_name(function.name.clone()).with_resolved_function(
                rumoca_core::ResolvedFunctionReference {
                    instance_id,
                    base_part_count: 0,
                    transitively_non_replaceable: function.transitively_non_replaceable,
                },
            ),
        )
    }

    /// The specialization of `callee` for `arguments` (one per functional
    /// input, in input order), created on first use.
    fn specialization(
        &mut self,
        callee: &rumoca_core::Function,
        arguments: &[(usize, FunctionArgument)],
    ) -> Result<rumoca_core::VarName, FlattenError> {
        let mut name = callee.name.as_str().to_string();
        for (index, argument) in arguments {
            name.push_str(&format!(
                "__{}_{}",
                callee.inputs[*index].name,
                argument.target.as_str().replace('.', "_")
            ));
            for (formal, _) in &argument.bound {
                name.push('_');
                name.push_str(formal);
            }
        }
        let name = rumoca_core::VarName::new(name);
        if self.functions.contains_key(&name) || self.created.contains_key(&name) {
            return Ok(name);
        }
        let mut specialized = callee.clone();
        specialized.name = name.clone();
        specialized.instance_id = Some(rumoca_core::FunctionInstanceId::new(self.next_instance));
        self.next_instance += 1;
        let functional = arguments
            .iter()
            .map(|(index, _)| *index)
            .collect::<HashSet<_>>();
        let mut inputs = callee
            .inputs
            .iter()
            .enumerate()
            .filter(|(index, _)| !functional.contains(index))
            .map(|(_, input)| input.clone())
            .collect::<Vec<_>>();
        for (index, argument) in arguments {
            let formal = &callee.inputs[*index];
            let target = self
                .functions
                .get(argument.target.var_name())
                .expect("function argument names a Flat function");
            let captures = capture_inputs(target, formal, argument, &mut inputs)?;
            FormalSubstitution {
                formal: formal.name.clone(),
                target: argument.target.clone(),
                captures,
            }
            .apply(&mut specialized)?;
        }
        specialized.inputs = inputs;
        self.created.insert(name.clone(), specialized);
        Ok(name)
    }
}

impl FallibleExpressionRewriter for Specializer {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        } = expression
        else {
            return self.walk_expression(expression);
        };
        let Some(callee) = self.functions.get(name.var_name()).cloned() else {
            return self.walk_expression(expression);
        };
        if !has_function_input(&callee) || args.len() != callee.inputs.len() {
            return self.walk_expression(expression);
        }
        let mut functional = Vec::new();
        let mut values = Vec::new();
        for (index, (input, argument)) in callee.inputs.iter().zip(args).enumerate() {
            if input.type_class == Some(rumoca_core::ClassType::Function) {
                let argument = self.rewrite_expression(argument)?;
                functional.push((
                    index,
                    self.function_argument(&callee, input, &argument, *span)?,
                ));
            } else {
                values.push(self.rewrite_expression(argument)?);
            }
        }
        for (_, argument) in &functional {
            values.extend(argument.bound.iter().map(|(_, value)| value.clone()));
        }
        let specialized = self.specialization(&callee, &functional)?;
        let instance_id = self
            .created
            .get(&specialized)
            .or_else(|| self.functions.get(&specialized))
            .and_then(|function| function.instance_id)
            .expect("a specialization carries its own instance identity");
        let reference = rumoca_core::Reference::from_var_name(specialized).with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id,
                base_part_count: name
                    .resolved_function()
                    .map_or(0, |resolved| resolved.base_part_count),
                transitively_non_replaceable: name
                    .resolved_function()
                    .is_some_and(|resolved| resolved.transitively_non_replaceable),
            },
        );
        Ok(rumoca_core::Expression::FunctionCall {
            name: reference,
            args: values,
            is_constructor: false,
            span: *span,
        })
    }
}

impl FallibleStatementRewriter for Specializer {}

/// A named formal association `name = value` of a partial application.
fn named_association(
    argument: &rumoca_core::Expression,
) -> Option<(String, rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = argument
    else {
        return None;
    };
    let formal = name
        .as_str()
        .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)?;
    let [value] = args.as_slice() else {
        return None;
    };
    Some((formal.to_string(), value.clone()))
}

/// Replace the formal function `formal` in a specialized body: a call of it
/// calls the argument's function with the captured formals added by name, and
/// a bare use (passing it on) becomes that partial application.
struct FormalSubstitution {
    formal: String,
    target: rumoca_core::Reference,
    captures: Vec<(String, String)>,
}

impl FormalSubstitution {
    fn captured_associations(&self, span: Span) -> Vec<rumoca_core::Expression> {
        self.captures
            .iter()
            .map(|(bound, captured)| rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::generated(format!(
                    "{}{bound}",
                    rumoca_core::NAMED_FUNCTION_ARG_PREFIX
                )),
                args: vec![rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::from_var_name(rumoca_core::VarName::new(
                        captured.clone(),
                    )),
                    subscripts: Vec::new(),
                    span,
                }],
                is_constructor: true,
                span,
            })
            .collect()
    }

    fn names_formal(&self, name: &rumoca_core::Reference) -> bool {
        name.as_str() == self.formal && name.resolved_function().is_none()
    }
}

impl FallibleExpressionRewriter for FormalSubstitution {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        match expression {
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } if self.names_formal(name) => {
                let mut args = args
                    .iter()
                    .map(|arg| self.rewrite_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                args.extend(self.captured_associations(*span));
                Ok(rumoca_core::Expression::FunctionCall {
                    name: self.target.clone(),
                    args,
                    is_constructor: false,
                    span: *span,
                })
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() && self.names_formal(name) => {
                Ok(rumoca_core::Expression::FunctionCall {
                    name: self.target.clone(),
                    args: self.captured_associations(*span),
                    is_constructor: false,
                    span: *span,
                })
            }
            _ => self.walk_expression(expression),
        }
    }
}

impl FallibleStatementRewriter for FormalSubstitution {}

/// Add one input per formal a partial application binds, named after the
/// functional formal, and return the (bound formal, new input) pairs.
fn capture_inputs(
    target: &rumoca_core::Function,
    formal: &rumoca_core::FunctionParam,
    argument: &FunctionArgument,
    inputs: &mut Vec<rumoca_core::FunctionParam>,
) -> Result<Vec<(String, String)>, FlattenError> {
    let mut captures = Vec::new();
    for (bound, _) in &argument.bound {
        let Some(declared) = target.inputs.iter().find(|input| &input.name == bound) else {
            return Err(FlattenError::invalid_function_call_args(
                target.name.as_str(),
                format!("partial application binds unknown formal `{bound}`"),
                formal.span,
            ));
        };
        let captured = format!("{}__{bound}", formal.name);
        let mut input = declared.clone();
        input.name = captured.clone();
        input.default = None;
        inputs.push(input);
        captures.push((bound.clone(), captured));
    }
    Ok(captures)
}

impl FormalSubstitution {
    /// Substitute the formal function throughout a specialized function.
    fn apply(mut self, function: &mut rumoca_core::Function) -> Result<(), FlattenError> {
        for default in function
            .outputs
            .iter_mut()
            .chain(function.locals.iter_mut())
            .filter_map(|value| value.default.as_mut())
        {
            *default = self.rewrite_expression(default)?;
        }
        function.body = self.rewrite_statements(&function.body)?;
        Ok(())
    }
}
