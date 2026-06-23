//! DAE-level lowering passes for code generation.
//!
//! This module contains record function parameter decomposition, array size
//! argument insertion, parameter dependency sorting, and vector equation
//! scalarization that operate on the DAE IR before code generation.

use crate::ToDaeError;
use crate::scalar_size::compute_var_size;
use indexmap::{IndexMap, IndexSet};
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::DaeExpressionRewriter;
use std::collections::{BTreeMap, HashMap, HashSet};

type Dae = dae::Dae;
type RecordArgMap = HashMap<String, Vec<(usize, Vec<String>)>>;

mod record_field_inference;
use record_field_inference::{FieldUseMap, infer_record_fields_by_function};

/// DAE value prepared for code generators that need DAE-level convenience
/// rewrites without mutating the simulation DAE.
#[derive(Debug, Clone)]
pub struct CodegenDae {
    dae: Dae,
}

impl CodegenDae {
    /// Borrow the prepared DAE.
    pub fn as_dae(&self) -> &dae::Dae {
        &self.dae
    }

    /// Consume the wrapper and return the prepared DAE.
    pub fn into_dae(self) -> dae::Dae {
        self.dae
    }
}

/// Prepare a DAE copy for code generators.
///
/// This applies codegen-only rewrites such as record-parameter decomposition
/// and array-size argument insertion to a cloned DAE so the caller cannot
/// accidentally reuse a simulation DAE after backend-specific mutation.
pub fn prepare_dae_for_codegen(dae: &dae::Dae) -> Result<CodegenDae, ToDaeError> {
    let mut prepared = dae.clone();
    lower_record_function_params_dae(&mut prepared)?;
    unwrap_block_constructor_value_wrappers(&mut prepared);
    insert_array_size_args_dae(&mut prepared)?;
    Ok(CodegenDae { dae: prepared })
}

fn unwrap_block_constructor_value_wrappers(dae: &mut Dae) {
    let functions = dae.symbols.functions.clone();
    BlockConstructorValueUnwrapper {
        functions: &functions,
    }
    .rewrite_dae(dae);
    for function in dae.symbols.functions.values_mut() {
        function.body = BlockConstructorValueUnwrapper {
            functions: &functions,
        }
        .rewrite_statements(&function.body);
    }
}

struct BlockConstructorValueUnwrapper<'a> {
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
}

impl ExpressionRewriter for BlockConstructorValueUnwrapper<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            span,
        } = expr
        {
            let args = self.rewrite_expressions(args);
            if args.len() == 1
                && self
                    .functions
                    .get(name.var_name())
                    .is_some_and(is_block_constructor_value_wrapper)
            {
                return args.into_iter().next().expect("checked len");
            }
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: true,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl StatementRewriter for BlockConstructorValueUnwrapper<'_> {}
impl DaeExpressionRewriter for BlockConstructorValueUnwrapper<'_> {}

fn is_block_constructor_value_wrapper(function: &rumoca_core::Function) -> bool {
    function.is_constructor
        && !function.inputs.is_empty()
        && function
            .inputs
            .iter()
            .all(|input| input.type_class == Some(rumoca_core::ClassType::Connector))
}

// =============================================================================
// Record function parameter decomposition (DAE level)
// =============================================================================

pub(crate) fn record_constructor_fields_from_metadata<'a, I>(
    functions: I,
    type_name: &str,
) -> Option<Vec<rumoca_core::FunctionParam>>
where
    I: IntoIterator<Item = (&'a rumoca_core::VarName, &'a rumoca_core::Function)>,
{
    functions
        .into_iter()
        .find(|(name, function)| {
            function.is_constructor
                && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
        })
        .map(|(_, function)| function.inputs.clone())
        .filter(|fields| !fields.is_empty())
}

fn record_fields_from_constructor_metadata(
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    type_name: &str,
) -> Option<Vec<String>> {
    record_constructor_fields_from_metadata(functions.iter(), type_name)
        .map(|fields| fields.into_iter().map(|param| param.name).collect())
}

/// Decompose record-typed function params in the DAE. This rewrites function
/// signatures (replacing `c: Complex` with `c_re, c_im: Real`) and decomposes
/// call-site arguments throughout all DAE equations.
/// Decompose record-typed function params in the DAE for code generators.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn lower_record_function_params_dae(dae: &mut Dae) -> Result<(), ToDaeError> {
    let record_fields_by_type = dae
        .symbols
        .functions
        .values()
        .flat_map(|function| function.inputs.iter())
        .filter(|input| input.type_class == Some(rumoca_core::ClassType::Record))
        .filter_map(|input| {
            record_fields_from_constructor_metadata(&dae.symbols.functions, &input.type_name)
                .map(|fields| (input.type_name.clone(), fields))
        })
        .collect::<HashMap<_, _>>();

    let inferred_fields_by_function =
        infer_record_fields_by_function(&dae.symbols.functions, &record_fields_by_type);

    // Identify functions with record params and rewrite their signatures.
    let mut decomp_map: HashMap<String, Vec<(usize, Vec<String>)>> = HashMap::new();

    for (func_name, func) in dae.symbols.functions.iter_mut() {
        let decomposed = record_inputs_to_decompose(
            func_name.as_str(),
            func,
            &record_fields_by_type,
            &inferred_fields_by_function,
        );
        let inferred_function_fields = inferred_fields_by_function.get(func_name.as_str());

        // Replace record inputs with scalar field inputs
        rewrite_record_function_inputs(func, &decomposed, inferred_function_fields);
        if decomposed.is_empty() {
            continue;
        }

        let entry: Vec<(usize, Vec<String>)> = decomposed
            .iter()
            .map(|(idx, _, fields)| (*idx, fields.clone()))
            .collect();
        decomp_map.insert(func_name.as_str().to_string(), entry);
    }

    if decomp_map.is_empty() {
        return Ok(());
    }

    let mut rewriter = DaeRecordArgDecomposer {
        map: &decomp_map,
        error: None,
    };
    rewriter.rewrite_dae(dae);
    if let Some(error) = rewriter.error {
        return Err(error);
    }
    for func in dae.symbols.functions.values_mut() {
        for stmt in &mut func.body {
            decompose_record_args_dae_stmt(stmt, &decomp_map)?;
        }
    }
    Ok(())
}

fn record_inputs_to_decompose(
    func_name: &str,
    func: &rumoca_core::Function,
    record_fields_by_type: &HashMap<String, Vec<String>>,
    inferred_fields_by_function: &HashMap<String, FieldUseMap>,
) -> Vec<(usize, String, Vec<String>)> {
    func.inputs
        .iter()
        .enumerate()
        .filter_map(|(idx, input)| {
            let fields = record_fields_for_input(
                func_name,
                input,
                record_fields_by_type,
                inferred_fields_by_function,
            );
            (!fields.is_empty()).then(|| (idx, input.name.clone(), fields))
        })
        .collect()
}

fn record_fields_for_input(
    func_name: &str,
    input: &rumoca_core::FunctionParam,
    record_fields_by_type: &HashMap<String, Vec<String>>,
    inferred_fields_by_function: &HashMap<String, FieldUseMap>,
) -> Vec<String> {
    record_fields_by_type
        .get(&input.type_name)
        .into_iter()
        .flatten()
        .cloned()
        .chain(
            inferred_fields_by_function
                .get(func_name)
                .and_then(|fields| fields.get(&input.name))
                .into_iter()
                .flat_map(|fields| fields.keys().cloned()),
        )
        .collect::<IndexSet<_>>()
        .into_iter()
        .collect()
}

fn rewrite_record_function_inputs(
    func: &mut rumoca_core::Function,
    decomposed: &[(usize, String, Vec<String>)],
    inferred_fields: Option<&FieldUseMap>,
) {
    let old_inputs = std::mem::take(&mut func.inputs);
    let mut seen_inputs = HashSet::<String>::new();
    let mut flattened_prefixes = HashSet::<String>::new();
    for (idx, input) in old_inputs.into_iter().enumerate() {
        rewrite_one_record_function_input(
            func,
            input,
            idx,
            decomposed,
            inferred_fields,
            &mut seen_inputs,
            &mut flattened_prefixes,
        );
    }
    append_inferred_flattened_inputs(func, inferred_fields, flattened_prefixes, &mut seen_inputs);
}

fn rewrite_one_record_function_input(
    func: &mut rumoca_core::Function,
    input: rumoca_core::FunctionParam,
    idx: usize,
    decomposed: &[(usize, String, Vec<String>)],
    inferred_fields: Option<&FieldUseMap>,
    seen_inputs: &mut HashSet<String>,
    flattened_prefixes: &mut HashSet<String>,
) {
    let Some((_, param_name, fields)) = decomposed.iter().find(|(i, _, _)| *i == idx) else {
        if let Some((prefix, _)) = input.name.split_once('_') {
            flattened_prefixes.insert(prefix.to_string());
        }
        seen_inputs.insert(input.name.clone());
        func.inputs.push(input);
        return;
    };
    for field in fields {
        let dims = inferred_fields
            .and_then(|by_prefix| by_prefix.get(param_name))
            .and_then(|by_field| by_field.get(field))
            .cloned()
            .unwrap_or_default();
        push_flat_record_input(func, format!("{param_name}_{field}"), input.span, dims);
        seen_inputs.insert(format!("{param_name}_{field}"));
    }
}

fn append_inferred_flattened_inputs(
    func: &mut rumoca_core::Function,
    inferred_fields: Option<&FieldUseMap>,
    flattened_prefixes: HashSet<String>,
    seen_inputs: &mut HashSet<String>,
) {
    let Some(inferred) = inferred_fields else {
        return;
    };
    for prefix in flattened_prefixes {
        append_inferred_flattened_prefix(func, inferred, &prefix, seen_inputs);
    }
}

fn append_inferred_flattened_prefix(
    func: &mut rumoca_core::Function,
    inferred: &FieldUseMap,
    prefix: &str,
    seen_inputs: &mut HashSet<String>,
) {
    let Some(fields) = inferred.get(prefix) else {
        return;
    };
    for (field, dims) in fields {
        let name = format!("{prefix}_{field}");
        if seen_inputs.insert(name.clone()) {
            push_flat_record_input(func, name, rumoca_core::Span::DUMMY, dims.clone());
        }
    }
}

fn push_flat_record_input(
    func: &mut rumoca_core::Function,
    name: String,
    span: rumoca_core::Span,
    dims: Vec<i64>,
) {
    func.inputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name,
        span,
        type_name: "Real".to_string(),
        type_class: None,
        dims,
        shape_expr: Vec::new(),
        default: None,
        description: None,
    });
}

pub(crate) fn lower_enum_literal_refs_to_ordinals(dae: &mut Dae) {
    if dae.symbols.enum_literal_ordinals.is_empty() {
        return;
    }
    let ordinals = dae.symbols.enum_literal_ordinals.clone();
    EnumLiteralOrdinalLowerer {
        ordinals: &ordinals,
    }
    .rewrite_dae(dae);
    for function in dae.symbols.functions.values_mut() {
        function.body = EnumLiteralOrdinalLowerer {
            ordinals: &ordinals,
        }
        .rewrite_statements(&function.body);
    }
}

struct EnumLiteralOrdinalLowerer<'a> {
    ordinals: &'a IndexMap<String, i64>,
}

impl ExpressionRewriter for EnumLiteralOrdinalLowerer<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(ordinal) = self.ordinals.get(name.as_str())
        {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*ordinal),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

impl StatementRewriter for EnumLiteralOrdinalLowerer<'_> {}

impl DaeExpressionRewriter for EnumLiteralOrdinalLowerer<'_> {}

fn decompose_record_args_dae_stmt(
    stmt: &mut rumoca_core::Statement,
    map: &RecordArgMap,
) -> Result<(), ToDaeError> {
    let mut rewriter = DaeRecordArgDecomposer { map, error: None };
    *stmt = rewriter.rewrite_statement(stmt);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct DaeRecordArgDecomposer<'a> {
    map: &'a RecordArgMap,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for DaeRecordArgDecomposer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let rewritten_args = self.rewrite_expressions(args);
            let Some(decomposed) = self.map.get(name.as_str()) else {
                return rumoca_core::Expression::FunctionCall {
                    name: name.clone(),
                    args: rewritten_args,
                    is_constructor: *is_constructor,
                    span: *span,
                };
            };
            let args = match decompose_dae_record_args(&rewritten_args, decomposed, *span) {
                Ok(args) => args,
                Err(error) => return self.record_error(error, expr),
            };
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl DaeRecordArgDecomposer<'_> {
    fn record_error(
        &mut self,
        error: ToDaeError,
        expr: &rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        self.error = Some(error);
        expr.clone()
    }
}

impl StatementRewriter for DaeRecordArgDecomposer<'_> {}

impl DaeExpressionRewriter for DaeRecordArgDecomposer<'_> {}

fn decompose_dae_record_args(
    old_args: &[rumoca_core::Expression],
    decomposed: &[(usize, Vec<String>)],
    call_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    let mut args = Vec::new();
    let mut old_idx = 0;
    for (param_idx, fields) in decomposed {
        while old_idx < *param_idx && old_idx < old_args.len() {
            args.push(old_args[old_idx].clone());
            old_idx += 1;
        }
        if old_idx < old_args.len() {
            expand_dae_record_arg(&old_args[old_idx], fields, &mut args, call_span)?;
            old_idx += 1;
        }
    }
    while old_idx < old_args.len() {
        args.push(old_args[old_idx].clone());
        old_idx += 1;
    }
    Ok(args)
}

fn named_constructor_arg_dae<'a>(
    ctor_args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    for arg in ctor_args {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            span: _,
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn expand_dae_record_arg(
    arg: &rumoca_core::Expression,
    fields: &[String],
    out: &mut Vec<rumoca_core::Expression>,
    call_span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    let owner_span = required_arg_owner_span(arg, call_span, "DAE record argument expansion")?;
    // Constructor call → extract positional/named args
    if let rumoca_core::Expression::FunctionCall {
        args: ctor_args,
        is_constructor: true,
        ..
    } = arg
    {
        let positional: Vec<&rumoca_core::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, rumoca_core::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg_dae(ctor_args, field.as_str());
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: owner_span,
                });
            }
        }
        return Ok(());
    }

    // Variable reference → emit field VarRefs
    if let rumoca_core::Expression::VarRef { name, .. } = arg {
        for field in fields {
            out.push(rumoca_core::Expression::VarRef {
                name: name.with_appended_field(field),
                subscripts: vec![],
                span: owner_span,
            });
        }
        return Ok(());
    }

    // Check for FieldAccess on the record variable (e.g. `c.re` passed directly)
    if let rumoca_core::Expression::FieldAccess { .. } = arg {
        // Single field access on a record — just push the base.field expression
        // This handles cases like passing `c.re` where `c` was the original record param.
        out.push(arg.clone());
        // Pad remaining fields with 0.0
        for _ in 1..fields.len() {
            out.push(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: owner_span,
            });
        }
        return Ok(());
    }

    // Scalar expression passed to a record-typed parameter (e.g. Real expr passed
    // where Complex is expected) — treat as the first field, zero-fill the rest.
    // This avoids generating invalid C like `(expr).re`.
    if is_obviously_scalar(arg) {
        out.push(arg.clone());
        for _ in 1..fields.len() {
            out.push(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: owner_span,
            });
        }
        return Ok(());
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(rumoca_core::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.clone(),
            span: owner_span,
        });
    }
    Ok(())
}

/// Returns true if the expression is obviously a scalar (not a record type).
fn is_obviously_scalar(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Literal { value: _, .. } => true,
        rumoca_core::Expression::Binary { .. } | rumoca_core::Expression::Unary { .. } => true,
        rumoca_core::Expression::BuiltinCall { function, .. } => {
            // Math builtins return scalars
            matches!(
                function,
                rumoca_core::BuiltinFunction::Abs
                    | rumoca_core::BuiltinFunction::Sign
                    | rumoca_core::BuiltinFunction::Sqrt
                    | rumoca_core::BuiltinFunction::Sin
                    | rumoca_core::BuiltinFunction::Cos
                    | rumoca_core::BuiltinFunction::Tan
                    | rumoca_core::BuiltinFunction::Asin
                    | rumoca_core::BuiltinFunction::Acos
                    | rumoca_core::BuiltinFunction::Atan
                    | rumoca_core::BuiltinFunction::Atan2
                    | rumoca_core::BuiltinFunction::Sinh
                    | rumoca_core::BuiltinFunction::Cosh
                    | rumoca_core::BuiltinFunction::Tanh
                    | rumoca_core::BuiltinFunction::Exp
                    | rumoca_core::BuiltinFunction::Log
                    | rumoca_core::BuiltinFunction::Log10
                    | rumoca_core::BuiltinFunction::Floor
                    | rumoca_core::BuiltinFunction::Ceil
                    | rumoca_core::BuiltinFunction::Min
                    | rumoca_core::BuiltinFunction::Max
                    | rumoca_core::BuiltinFunction::Sum
                    | rumoca_core::BuiltinFunction::Size
                    | rumoca_core::BuiltinFunction::Der
                    | rumoca_core::BuiltinFunction::Pre
                    | rumoca_core::BuiltinFunction::Mod
                    | rumoca_core::BuiltinFunction::Rem
                    | rumoca_core::BuiltinFunction::Div
            )
        }
        rumoca_core::Expression::If { else_branch, .. } => is_obviously_scalar(else_branch),
        // User-defined function calls return scalar double in generated C
        rumoca_core::Expression::FunctionCall { .. } => true,
        _ => false,
    }
}

/// Insert size arguments for variable-size array params at DAE call sites.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn insert_array_size_args_dae(dae: &mut Dae) -> Result<(), ToDaeError> {
    let array_param_map: HashMap<String, Vec<usize>> = dae
        .symbols
        .functions
        .iter()
        .filter_map(|(name, func)| {
            let indices: Vec<usize> = func
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.dims.is_empty())
                .map(|(i, _)| i)
                .collect();
            if indices.is_empty() {
                None
            } else {
                Some((name.as_str().to_string(), indices))
            }
        })
        .collect();

    if array_param_map.is_empty() {
        return Ok(());
    }

    let mut rewriter = DaeSizeArgInserter {
        map: &array_param_map,
        error: None,
    };
    rewriter.rewrite_dae(dae);
    if let Some(error) = rewriter.error {
        return Err(error);
    }
    for func in dae.symbols.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_dae_stmt(stmt, &array_param_map)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod record_lowering_tests;

fn insert_size_args_dae_stmt(
    stmt: &mut rumoca_core::Statement,
    map: &HashMap<String, Vec<usize>>,
) -> Result<(), ToDaeError> {
    let mut rewriter = DaeSizeArgInserter { map, error: None };
    *stmt = rewriter.rewrite_statement(stmt);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct DaeSizeArgInserter<'a> {
    map: &'a HashMap<String, Vec<usize>>,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for DaeSizeArgInserter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let mut args = self.rewrite_expressions(args);
            if let Some(array_indices) = self.map.get(name.as_str())
                && let Err(error) = insert_dae_size_args(&mut args, array_indices, *span)
            {
                return self.record_error(error, expr);
            }
            return rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }
}

impl DaeSizeArgInserter<'_> {
    fn record_error(
        &mut self,
        error: ToDaeError,
        expr: &rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        self.error = Some(error);
        expr.clone()
    }
}

impl StatementRewriter for DaeSizeArgInserter<'_> {}

impl DaeExpressionRewriter for DaeSizeArgInserter<'_> {}

fn insert_dae_size_args(
    args: &mut Vec<rumoca_core::Expression>,
    array_indices: &[usize],
    call_span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    for &param_idx in array_indices.iter().rev() {
        if param_idx >= args.len() {
            continue;
        }
        // If the argument is an Array literal with known element count, use the
        // literal count directly instead of size(), which some backends cannot
        // render for compound literals.
        let size_expr = array_size_expr_for_arg(&args[param_idx], call_span)?;
        args.insert(param_idx + 1, size_expr);
    }
    Ok(())
}

fn array_size_expr_for_arg(
    arg: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let owner_span = required_arg_owner_span(arg, call_span, "DAE array size argument")?;
    if let rumoca_core::Expression::Array { elements, .. } = arg {
        let size = i64::try_from(elements.len()).map_err(|_| {
            ToDaeError::runtime_contract_violation_at(
                "DAE array literal size exceeds i64".to_string(),
                owner_span,
            )
        })?;
        return Ok(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(size),
            span: owner_span,
        });
    }

    Ok(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            arg.clone(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: owner_span,
            },
        ],
        span: owner_span,
    })
}

fn required_arg_owner_span(
    arg: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, ToDaeError> {
    let Some(span) = arg
        .span()
        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
    else {
        return Err(ToDaeError::runtime_metadata_violation(format!(
            "missing source provenance for {context}"
        )));
    };
    span.require_provenance(context)
        .map(Into::into)
        .map_err(|err| ToDaeError::runtime_metadata_violation(err.to_string()))
}

// =============================================================================

/// Topologically sort parameters so that start-value dependencies are ordered.
///
/// If parameter A's `start` expression references parameter B, then B must
/// appear before A in the parameter map. This ensures code generators that
/// evaluate start values sequentially produce correct numeric results.
///
/// Falls back to the original order if cycles are detected.
pub(crate) fn sort_parameters_by_start_dependency(dae: &mut Dae) {
    let param_names: IndexSet<rumoca_core::VarName> =
        dae.variables.parameters.keys().cloned().collect();
    if param_names.len() <= 1 {
        return;
    }

    // Build adjacency: param → set of params its start expression depends on
    let mut deps: IndexMap<usize, IndexSet<usize>> = IndexMap::new();
    for (idx, (_, var)) in dae.variables.parameters.iter().enumerate() {
        let Some(ref start_expr) = var.start else {
            continue;
        };
        for ref_name in collect_expression_var_refs(start_expr) {
            let Some(dep_idx) = param_names.get_index_of(&ref_name) else {
                continue;
            };
            if dep_idx != idx {
                deps.entry(idx).or_default().insert(dep_idx);
            }
        }
    }

    // If no dependencies exist, nothing to reorder.
    if deps.is_empty() {
        return;
    }

    // Kahn's algorithm for topological sort
    let n = param_names.len();
    let mut in_degree = vec![0usize; n];
    // Build forward edges: if node depends on dep, then dep → node
    let mut forward: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (&node, predecessors) in &deps {
        for &pred in predecessors {
            forward[pred].push(node);
            in_degree[node] += 1;
        }
    }

    let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut sorted_indices = Vec::with_capacity(n);
    while let Some(node) = queue.pop_front() {
        sorted_indices.push(node);
        for &next in &forward[node] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    if sorted_indices.len() != n {
        // Cycle detected — keep original order (conservative fallback)
        return;
    }

    // Rebuild the parameters IndexMap in sorted order
    let old_params: Vec<(rumoca_core::VarName, dae::Variable)> =
        dae.variables.parameters.drain(..).collect();
    for &idx in &sorted_indices {
        let (name, var) = old_params[idx].clone();
        dae.variables.parameters.insert(name, var);
    }
}

/// Collect all VarRef names from an expression tree.
fn collect_expression_var_refs(expr: &rumoca_core::Expression) -> Vec<rumoca_core::VarName> {
    let mut collector = VarRefListCollector { refs: Vec::new() };
    collector.visit_expression(expr);
    collector.refs
}

struct VarRefListCollector {
    refs: Vec<rumoca_core::VarName>,
}

impl ExpressionVisitor for VarRefListCollector {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.refs.push(name.var_name().clone());
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

// =============================================================================
// Vector equation scalarization
// =============================================================================

/// Scalarize vector equations that reference "phantom" base names.
///
/// In Modelica, connector arrays like `plug_p.pin[3]` produce scalarized
/// variables (`sineVoltage.plug_p.pin[1].v`, `…pin[2].v`, `…pin[3].v`)
/// but some component-level equations reference the unsubscripted base name
/// (`sineVoltage.plug_p.pin.v`) as a vector.  These phantom base names do
/// not appear in any DAE variable map, so backends that render equations
/// directly (CasADi, SymPy, JAX) produce undefined identifiers.
///
/// This pass detects equations with `scalar_count > 1` whose expressions
/// contain such phantom VarRefs, and expands each into `scalar_count`
/// scalar equations — one per element — with every phantom VarRef replaced
/// by its indexed variant and every declared-array VarRef subscripted.
pub fn scalarize_phantom_vector_equations(dae: &mut Dae) -> Result<(), ToDaeError> {
    let known_names = build_known_var_name_set(dae);
    let phantom_map = build_phantom_expansion_map(dae, &known_names);
    let array_dims = build_array_dims_map(dae);

    canonicalize_embedded_subscript_equation_list(&mut dae.continuous.equations, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.initialization.equations, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.real_updates, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.valued_updates, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.conditions.equations, &array_dims)?;

    if phantom_map.is_empty() {
        return Ok(());
    }

    scalarize_equation_list(
        &mut dae.continuous.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.initialization.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.discrete.real_updates,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.discrete.valued_updates,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    scalarize_equation_list(
        &mut dae.conditions.equations,
        &phantom_map,
        &array_dims,
        &dae.symbols.functions,
    )?;
    Ok(())
}

/// Build the set of all variable names known to the DAE.
fn build_known_var_name_set(dae: &Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for map in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for name in map.keys() {
            names.insert(name.as_str().to_string());
        }
    }
    names
}

/// Build a map from stripped base name → sorted list of actual indexed names.
///
/// For example, if the DAE has `sineVoltage.plug_p.pin[1].v`,
/// `sineVoltage.plug_p.pin[2].v`, `sineVoltage.plug_p.pin[3].v`, the map
/// will contain `"sineVoltage.plug_p.pin.v" → ["…pin[1].v", "…pin[2].v", "…pin[3].v"]`.
///
/// Only base names that do NOT themselves appear in `known_names` are included
/// (i.e., only phantom base names that need expansion).
fn build_phantom_expansion_map(
    dae: &Dae,
    known_names: &HashSet<String>,
) -> HashMap<String, Vec<rumoca_core::Reference>> {
    // Group all known names by their stripped base
    let mut base_to_indexed: HashMap<String, BTreeMap<String, ()>> = HashMap::new();
    for name in known_names {
        let base = super::path_utils::strip_all_subscripts(name);
        if base != *name {
            // This name has subscripts — record it under the base
            base_to_indexed
                .entry(base)
                .or_default()
                .insert(name.clone(), ());
        }
    }

    // Only keep entries where the base name itself is NOT a known variable.
    // Singleton connector arrays still need this mapping: a scalar equation may
    // refer to `ports.C_outflow` while the declared variable is
    // `ports[1].C_outflow`.
    let mut result = HashMap::new();
    for (base, indexed) in base_to_indexed {
        if !known_names.contains(&base) && !indexed.is_empty() {
            let variants = indexed
                .into_keys()
                .map(|variant| phantom_variant_reference(dae, variant))
                .collect();
            result.insert(base, variants);
        }
    }
    result
}

/// Structured reference for a phantom expansion variant. Variants name
/// existing scalarized DAE variables, so the variable's own structured
/// component reference is the source of truth.
fn phantom_variant_reference(dae: &Dae, variant: String) -> rumoca_core::Reference {
    let key = rumoca_core::VarName::new(&variant);
    let variable = [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ]
    .into_iter()
    .find_map(|map| map.get(&key));
    match variable.and_then(|var| var.component_ref.clone()) {
        Some(reference) => rumoca_core::Reference::with_component_reference(variant, reference),
        None => rumoca_core::Reference::generated(variant),
    }
}

/// Build a map from variable name → dims for declared array variables.
fn build_array_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut dims_map = HashMap::new();
    for map in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for (name, var) in map {
            if !var.dims.is_empty() {
                dims_map.insert(name.as_str().to_string(), var.dims.clone());
            }
        }
    }
    dims_map
}

fn canonicalize_embedded_subscript_equation_list(
    equations: &mut [dae::Equation],
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<(), ToDaeError> {
    let mut canonicalizer = EmbeddedSubscriptCanonicalizer {
        array_dims,
        error: None,
    };
    for equation in equations {
        if canonicalizer.error.is_some() {
            break;
        }
        equation.rhs = canonicalizer.rewrite_expression(&equation.rhs);
    }
    match canonicalizer.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct EmbeddedSubscriptCanonicalizer<'a> {
    array_dims: &'a HashMap<String, Vec<i64>>,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for EmbeddedSubscriptCanonicalizer<'_> {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if self.error.is_some() {
            return rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: subscripts.to_vec(),
                span,
            };
        }
        if subscripts.is_empty()
            && let Some(scalar_name) = rumoca_core::parse_scalar_name(name.as_str())
            && self.array_dims.contains_key(scalar_name.base)
        {
            let base_name = if let Some(component_ref) = name.component_ref() {
                rumoca_core::Reference::with_component_reference(
                    scalar_name.base,
                    component_ref.clone(),
                )
            } else if name.is_generated() {
                rumoca_core::Reference::generated(scalar_name.base)
            } else {
                return rumoca_core::Expression::VarRef {
                    name: name.clone(),
                    subscripts: self.rewrite_subscripts(subscripts),
                    span,
                };
            };
            let generated_subscripts = match scalar_name
                .indices
                .into_iter()
                .map(|index| {
                    generated_index_subscript(
                        index,
                        span,
                        "DAE embedded scalar reference subscript",
                    )
                })
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(subscripts) => subscripts,
                Err(error) => {
                    return self.record_error(error, name, subscripts, span);
                }
            };
            return rumoca_core::Expression::VarRef {
                name: base_name,
                subscripts: generated_subscripts,
                span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl EmbeddedSubscriptCanonicalizer<'_> {
    fn record_error(
        &mut self,
        error: ToDaeError,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        self.error = Some(error);
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: subscripts.to_vec(),
            span,
        }
    }
}

fn expr_phantom_ref_width(
    expr: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
) -> Option<usize> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => phantom_map.get(name.as_str()).map(Vec::len),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => merge_phantom_widths(
            expr_phantom_ref_width(lhs, phantom_map),
            expr_phantom_ref_width(rhs, phantom_map),
        ),
        rumoca_core::Expression::Unary { rhs, .. } => expr_phantom_ref_width(rhs, phantom_map),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => args
            .iter()
            .filter_map(|arg| expr_phantom_ref_width(arg, phantom_map))
            .max(),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .flat_map(|(condition, value)| {
                [
                    expr_phantom_ref_width(condition, phantom_map),
                    expr_phantom_ref_width(value, phantom_map),
                ]
            })
            .chain(std::iter::once(expr_phantom_ref_width(
                else_branch,
                phantom_map,
            )))
            .flatten()
            .max(),
        rumoca_core::Expression::ArrayComprehension { expr, filter, .. } => merge_phantom_widths(
            expr_phantom_ref_width(expr, phantom_map),
            filter
                .as_ref()
                .and_then(|filter| expr_phantom_ref_width(filter, phantom_map)),
        ),
        _ => None,
    }
}

fn merge_phantom_widths(left: Option<usize>, right: Option<usize>) -> Option<usize> {
    left.into_iter().chain(right).max()
}

fn expr_has_array_comprehension(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::ArrayComprehension { .. } => true,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_array_comprehension(lhs) || expr_has_array_comprehension(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_array_comprehension(rhs),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_array_comprehension)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_array_comprehension(condition) || expr_has_array_comprehension(value)
            }) || expr_has_array_comprehension(else_branch)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(expr_has_array_comprehension)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_has_array_comprehension(start)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_has_array_comprehension(step))
                || expr_has_array_comprehension(end)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_has_array_comprehension(base)
                || subscripts.iter().any(subscript_has_array_comprehension)
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_has_array_comprehension(base),
        _ => false,
    }
}

fn subscript_has_array_comprehension(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expr_has_array_comprehension(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

/// Scalarize an expression at index `k` (0-based).
///
/// - Phantom VarRefs are replaced by the k-th indexed variant from `phantom_map`.
/// - Declared array VarRefs (with no subscripts) get subscript `[k+1]` (1-based)
///   only while expanding an equation that already contains a phantom reference.
///   MLS §10.6: ordinary declared-array equations must remain array equations so
///   later matrix-aware scalarization can preserve linear algebra semantics.
/// - All other expressions are recursively processed.
fn scalarize_expr_at(
    expr: &rumoca_core::Expression,
    k: usize,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let ctx = ScalarizeExprContext {
        k,
        phantom_map,
        array_dims,
        functions,
    };
    scalarize_expr_with_context(expr, &ctx)
}

struct ScalarizeExprContext<'a> {
    k: usize,
    phantom_map: &'a HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &'a HashMap<String, Vec<i64>>,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
}

fn scalarize_expr_with_context(
    expr: &rumoca_core::Expression,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => scalarize_var_ref_at(
            name,
            subscripts,
            reference_or_wrapper_span(name, *span),
            ctx.k,
            ctx.phantom_map,
            ctx.array_dims,
        )
        .and_then(|projected| projected.map_or_else(|| Ok(expr.clone()), Ok)),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
            Ok(rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(scalarize_expr_with_context(lhs, ctx)?),
                rhs: Box::new(scalarize_expr_with_context(rhs, ctx)?),
                span: *span,
            })
        }
        rumoca_core::Expression::Unary { op, rhs, span } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(scalarize_expr_with_context(rhs, ctx)?),
            span: *span,
        }),
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } => {
            if let Some(expr) = scalarize_builtin_array_constructor_at(
                *function,
                args,
                *span,
                ctx.k,
                ctx.phantom_map,
                ctx.array_dims,
                ctx.functions,
            )? {
                return Ok(expr);
            }
            Ok(rumoca_core::Expression::BuiltinCall {
                function: *function,
                args: args
                    .iter()
                    .map(|a| scalarize_expr_with_context(a, ctx))
                    .collect::<Result<Vec<_>, _>>()?,
                span: *span,
            })
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } => scalarize_function_call_at(name, args, *is_constructor, *span, ctx),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => scalarize_index_expr_at(base, subscripts, *span, ctx),
        rumoca_core::Expression::FieldAccess { base, field, span } => {
            scalarize_field_access_at(base, field, *span, ctx)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => scalarize_if_expr_at(branches, else_branch, *span, ctx),
        rumoca_core::Expression::Array { elements, .. } => {
            // An array literal in a vector equation context: extract element k
            if ctx.k < elements.len() {
                scalarize_expr_with_context(&elements[ctx.k], ctx)
            } else {
                Ok(expr.clone())
            }
        }
        rumoca_core::Expression::ArrayComprehension {
            expr: inner,
            indices,
            filter,
            span,
        } => scalarize_array_comprehension_at(expr, inner, indices, filter.as_deref(), *span, ctx),
        _ => Ok(expr.clone()),
    }
}

fn scalarize_index_expr_at(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if subscripts
        .iter()
        .all(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
        && let rumoca_core::Expression::VarRef {
            name,
            subscripts: base_subscripts,
            ..
        } = base
        && base_subscripts.is_empty()
        && let Some(expr) = scalarize_var_ref_at(
            name,
            base_subscripts,
            span,
            ctx.k,
            ctx.phantom_map,
            ctx.array_dims,
        )?
    {
        return Ok(expr);
    }
    Ok(rumoca_core::Expression::Index {
        base: Box::new(scalarize_expr_with_context(base, ctx)?),
        subscripts: subscripts.to_vec(),
        span,
    })
}

fn scalarize_field_access_at(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let base = scalarize_expr_with_context(base, ctx)?;
    if let rumoca_core::Expression::VarRef { name, span, .. } = &base
        && let rumoca_core::Expression::VarRef { subscripts, .. } = &base
        && subscripts.is_empty()
    {
        return Ok(rumoca_core::Expression::VarRef {
            name: name.with_appended_field(field),
            subscripts: Vec::new(),
            span: *span,
        });
    }
    Ok(rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span,
    })
}

fn scalarize_array_comprehension_at(
    original: &rumoca_core::Expression,
    inner: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if filter.is_some() || indices.len() != 1 {
        return Ok(original.clone());
    }
    let Some(value) = scalarized_comprehension_index_value(&indices[0].range, ctx.k) else {
        return Ok(original.clone());
    };
    let mut substitution = ComprehensionIndexSubstitution {
        name: indices[0].name.clone(),
        value,
        span: indices[0].range.span().unwrap_or(span),
    };
    let selected = substitution.rewrite_expression(inner);
    scalarize_expr_with_context(&selected, ctx)
}

fn scalarized_comprehension_index_value(range: &rumoca_core::Expression, k: usize) -> Option<i64> {
    let rumoca_core::Expression::Range {
        start,
        step,
        end: _,
        ..
    } = range
    else {
        return None;
    };
    let start = integer_literal_value(start)?;
    let step = match step.as_deref() {
        Some(step) => integer_literal_value(step)?,
        None => 1,
    };
    Some(start + (k as i64) * step)
}

fn integer_literal_value(expr: &rumoca_core::Expression) -> Option<i64> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    Some(*value)
}

struct ComprehensionIndexSubstitution {
    name: String,
    value: i64,
    span: rumoca_core::Span,
}

impl ExpressionRewriter for ComprehensionIndexSubstitution {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if name.as_str() == self.name && subscripts.is_empty() {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(self.value),
                span: self.span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if indices.iter().any(|index| index.name == self.name) {
            return rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(expr.clone()),
                indices: indices.to_vec(),
                filter: filter.cloned().map(Box::new),
                span,
            };
        }
        ExpressionRewriter::walk_array_comprehension_expression(self, expr, indices, filter, span)
    }
}

fn scalarize_var_ref_at(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    k: usize,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let n = name.as_str();
    if !subscripts.is_empty() {
        return Ok(None);
    }
    if let Some(variants) = phantom_map.get(n)
        && k < variants.len()
    {
        return Ok(Some(rumoca_core::Expression::VarRef {
            name: variants[k].clone(),
            subscripts: vec![],
            span,
        }));
    }
    if !array_dims.contains_key(n) {
        return Ok(None);
    }
    let index = one_based_scalar_index(k, span, "DAE phantom scalarized variable subscript")?;
    Ok(Some(rumoca_core::Expression::VarRef {
        name: name.clone(),
        subscripts: vec![generated_index_subscript(
            index,
            span,
            "DAE phantom scalarized variable subscript",
        )?],
        span,
    }))
}

fn reference_or_wrapper_span(
    reference: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> rumoca_core::Span {
    if span.is_dummy() {
        reference.span().unwrap_or(span)
    } else {
        span
    }
}

fn scalarize_function_call_at(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    is_constructor: bool,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let first_output_size =
        first_function_output_size(name.as_str(), ctx.functions).ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!("missing function output metadata for `{name}`"),
                span,
            )
        })?;
    if first_output_size > 1 {
        let index =
            one_based_scalar_index(ctx.k, span, "DAE scalarized function output subscript")?;
        return Ok(rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|arg| vectorize_phantom_expr(arg, ctx.phantom_map))
                    .collect(),
                is_constructor,
                span,
            }),
            subscripts: vec![generated_index_subscript(
                index,
                span,
                "DAE scalarized function output subscript",
            )?],
            span,
        });
    }
    Ok(rumoca_core::Expression::FunctionCall {
        name: name.clone(),
        args: args
            .iter()
            .map(|a| scalarize_expr_with_context(a, ctx))
            .collect::<Result<Vec<_>, _>>()?,
        is_constructor,
        span,
    })
}

fn one_based_scalar_index(
    zero_based: usize,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<i64, ToDaeError> {
    zero_based
        .checked_add(1)
        .and_then(|index| i64::try_from(index).ok())
        .ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!("{context} {zero_based} exceeds i64 range"),
                span,
            )
        })
}

fn generated_index_subscript(
    index: i64,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, ToDaeError> {
    rumoca_core::Subscript::try_generated_index(index, span, context).map_err(|err| {
        if span.is_dummy() {
            ToDaeError::runtime_metadata_violation(err.to_string())
        } else {
            ToDaeError::runtime_metadata_violation_at(err.to_string(), span)
        }
    })
}

fn scalarize_if_expr_at(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    Ok(rumoca_core::Expression::If {
        branches: branches
            .iter()
            .map(|(condition, value)| {
                Ok((
                    scalarize_expr_with_context(condition, ctx)?,
                    scalarize_expr_with_context(value, ctx)?,
                ))
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?,
        else_branch: Box::new(scalarize_expr_with_context(else_branch, ctx)?),
        span,
    })
}

fn scalarize_builtin_array_constructor_at(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    k: usize,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    match function {
        rumoca_core::BuiltinFunction::Zeros => Ok(Some(real_literal(0.0, span))),
        rumoca_core::BuiltinFunction::Ones => Ok(Some(real_literal(1.0, span))),
        rumoca_core::BuiltinFunction::Fill => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            scalarize_expr_at(value, k, phantom_map, array_dims, functions).map(Some)
        }
        rumoca_core::BuiltinFunction::Identity => {
            let Some(n) = args.first().and_then(literal_positive_usize) else {
                return Ok(None);
            };
            let row = k / n;
            let col = k % n;
            Ok(Some(real_literal(if row == col { 1.0 } else { 0.0 }, span)))
        }
        _ => Ok(None),
    }
}

fn literal_positive_usize(expr: &rumoca_core::Expression) -> Option<usize> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => usize::try_from(*value).ok().filter(|value| *value > 0),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if *value > 0.0 && value.fract() == 0.0 => Some(*value as usize),
        _ => None,
    }
}

fn real_literal(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span,
    }
}

fn first_function_output_size(
    name: &str,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Option<usize> {
    let lookup_name = rumoca_core::VarName::new(name);
    let function = functions.get(&lookup_name)?;
    let output = function.outputs.first()?;
    Some(compute_var_size(&output.dims))
}

fn vectorize_phantom_expr(
    expr: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
) -> rumoca_core::Expression {
    PhantomVectorizer { phantom_map }.rewrite_expression(expr)
}

struct PhantomVectorizer<'a> {
    phantom_map: &'a HashMap<String, Vec<rumoca_core::Reference>>,
}

impl ExpressionRewriter for PhantomVectorizer<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(variants) = self.phantom_map.get(name.as_str())
        {
            return rumoca_core::Expression::Array {
                elements: variants
                    .iter()
                    .map(|variant| rumoca_core::Expression::VarRef {
                        name: variant.clone(),
                        subscripts: Vec::new(),
                        span,
                    })
                    .collect(),
                is_matrix: false,
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn walk_binary_expression(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let lhs = self.rewrite_expression(lhs);
        let rhs = self.rewrite_expression(rhs);
        vectorized_binary_expr(op.clone(), lhs, rhs, span)
    }

    fn walk_unary_expression(
        &mut self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rhs = self.rewrite_expression(rhs);
        if let rumoca_core::Expression::Array { elements, .. } = rhs {
            return rumoca_core::Expression::Array {
                elements: elements
                    .into_iter()
                    .map(|element| rumoca_core::Expression::Unary {
                        op: op.clone(),
                        rhs: Box::new(element),
                        span,
                    })
                    .collect(),
                is_matrix: false,
                span,
            };
        }
        rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(rhs),
            span,
        }
    }
}

fn vectorized_binary_expr(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    match (lhs, rhs) {
        (
            rumoca_core::Expression::Array {
                elements: lhs_values,
                ..
            },
            rumoca_core::Expression::Array {
                elements: rhs_values,
                ..
            },
        ) if lhs_values.len() == rhs_values.len() => {
            array_from_binary_elements(op, lhs_values.into_iter().zip(rhs_values).collect(), span)
        }
        (
            rumoca_core::Expression::Array {
                elements: lhs_values,
                ..
            },
            rhs,
        ) => array_from_binary_elements(
            op,
            lhs_values
                .into_iter()
                .map(|lhs| (lhs, rhs.clone()))
                .collect(),
            span,
        ),
        (
            lhs,
            rumoca_core::Expression::Array {
                elements: rhs_values,
                ..
            },
        ) => array_from_binary_elements(
            op,
            rhs_values
                .into_iter()
                .map(|rhs| (lhs.clone(), rhs))
                .collect(),
            span,
        ),
        (lhs, rhs) => rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        },
    }
}

fn array_from_binary_elements(
    op: rumoca_core::OpBinary,
    pairs: Vec<(rumoca_core::Expression, rumoca_core::Expression)>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements: pairs
            .into_iter()
            .map(|(lhs, rhs)| rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            })
            .collect(),
        is_matrix: false,
        span,
    }
}

/// Process an equation list, expanding vector equations with phantom refs.
fn scalarize_equation_list(
    equations: &mut Vec<dae::Equation>,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<(), ToDaeError> {
    let mut new_equations = Vec::with_capacity(equations.len());
    for eq in equations.drain(..) {
        let phantom_width = expr_phantom_ref_width(&eq.rhs, phantom_map);
        if eq.scalar_count > 1 && (phantom_width.is_some() || expr_has_array_comprehension(&eq.rhs))
        {
            // Expand into scalar_count individual equations
            for k in 0..eq.scalar_count {
                let scalar_rhs = scalarize_expr_at(&eq.rhs, k, phantom_map, array_dims, functions)?;
                let origin = format!("{} [scalarized {}]", eq.origin, k + 1);
                new_equations.push(scalarized_equation_at(
                    &eq,
                    scalar_rhs,
                    k,
                    origin,
                    phantom_map,
                    array_dims,
                )?);
            }
        } else if phantom_width == Some(1) {
            let scalar_rhs = scalarize_expr_at(&eq.rhs, 0, phantom_map, array_dims, functions)?;
            new_equations.push(dae::Equation {
                rhs: scalar_rhs,
                ..eq
            });
        } else {
            new_equations.push(eq);
        }
    }
    *equations = new_equations;
    Ok(())
}

fn scalarized_equation_at(
    eq: &dae::Equation,
    scalar_rhs: rumoca_core::Expression,
    k: usize,
    origin: String,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<dae::Equation, ToDaeError> {
    let Some(lhs) = &eq.lhs else {
        return Ok(dae::Equation::residual(scalar_rhs, eq.span, origin));
    };
    Ok(dae::Equation::explicit(
        scalarize_lhs_name_at(lhs.var_name(), k, phantom_map, array_dims, eq.span)?,
        scalar_rhs,
        eq.span,
        origin,
    ))
}

fn scalarize_lhs_name_at(
    name: &rumoca_core::VarName,
    k: usize,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, ToDaeError> {
    if let Some(variants) = phantom_map.get(name.as_str())
        && let Some(variant) = variants.get(k)
    {
        return Ok(variant.clone());
    }
    let scalar_name = if let Some(dims) = array_dims.get(name.as_str()) {
        dae::scalar_name_for_flat_index(name, dims, k)
    } else {
        rumoca_core::VarName::new(format!("{}[{}]", name.as_str(), k + 1))
    };
    super::convert::structured_target_reference(&scalar_name, span)
}

#[cfg(test)]
mod tests;
