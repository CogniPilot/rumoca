//! DAE-level lowering passes for code generation.
//!
//! SPEC_0021 file-size exception: DAE pre-codegen lowering still hosts record,
//! array-size, dependency, and vector-equation passes together. split plan:
//! move each pass family into focused dae_lowering submodules.
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
type RecordArgMap = HashMap<String, Vec<RecordArgDecomposition>>;
type RecordArrayFieldMap = HashMap<String, RecordArrayFieldVariants>;

#[derive(Debug, Clone)]
struct RecordArrayFieldVariants {
    variants: Vec<rumoca_core::Reference>,
    field_dims: Vec<i64>,
}

struct RecordArgDecomposition {
    original_index: usize,
    param_name: String,
    fields: Vec<String>,
}

mod record_field_inference;
use record_field_inference::{FieldUseMap, infer_record_fields_by_function};

#[derive(Default)]
struct ArrayParamMap {
    by_def_id: HashMap<rumoca_core::DefId, Vec<(usize, String)>>,
    by_name: HashMap<String, Vec<(usize, String)>>,
}

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

/// Prepare a DAE copy for FMI modelDescription XML metadata.
///
/// FMI XML start attributes carry concrete values, not Modelica expressions.
/// This preparation is intentionally separate from runtime codegen prep so FMU
/// implementation code can still evaluate start expressions after importer
/// parameter overrides.
pub fn prepare_dae_for_fmi_model_description(dae: &dae::Dae) -> Result<CodegenDae, ToDaeError> {
    let mut prepared = dae.clone();
    crate::fmi_metadata_values::fold_fmi_model_description_values_to_literals(&mut prepared)?;
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
    let mut decomp_map = RecordArgMap::new();

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

        let entry: Vec<RecordArgDecomposition> = decomposed
            .iter()
            .map(|(idx, param_name, fields)| RecordArgDecomposition {
                original_index: *idx,
                param_name: param_name.clone(),
                fields: fields.clone(),
            })
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
    if input.type_class == Some(rumoca_core::ClassType::Class) {
        return Vec::new();
    }
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
            push_flat_record_input(func, name, func.span, dims.clone());
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
    lower_enum_literal_refs_in_structured_families(
        &mut dae.continuous.structured_equations,
        &ordinals,
    );
    lower_enum_literal_refs_in_structured_families(
        &mut dae.initialization.structured_equations,
        &ordinals,
    );
    for function in dae.symbols.functions.values_mut() {
        function.body = EnumLiteralOrdinalLowerer {
            ordinals: &ordinals,
        }
        .rewrite_statements(&function.body);
    }
}

fn lower_enum_literal_refs_in_structured_families(
    families: &mut [dae::StructuredEquationFamily],
    ordinals: &IndexMap<String, i64>,
) {
    for family in families {
        let Some(template) = &mut family.template else {
            continue;
        };
        let mut lowerer = EnumLiteralOrdinalLowerer { ordinals };
        template.body = lowerer.rewrite_expressions(&template.body);
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
    decomposed: &[RecordArgDecomposition],
    call_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    let mut args = Vec::new();
    let positional_args = old_args
        .iter()
        .filter(|arg| named_function_arg_value(arg).is_none())
        .collect::<Vec<_>>();
    let mut consumed_named_args = HashSet::new();
    let mut positional_idx = 0;
    for entry in decomposed {
        while positional_idx < entry.original_index && positional_idx < positional_args.len() {
            args.push(positional_args[positional_idx].clone());
            positional_idx += 1;
        }
        if let Some(named_arg) = named_record_arg_value(old_args, &entry.param_name) {
            consumed_named_args.insert(entry.param_name.as_str());
            expand_dae_record_arg(named_arg, &entry.fields, &mut args, call_span)?;
        } else if positional_idx < positional_args.len() {
            expand_dae_record_arg(
                positional_args[positional_idx],
                &entry.fields,
                &mut args,
                call_span,
            )?;
            positional_idx += 1;
        }
    }
    while positional_idx < positional_args.len() {
        args.push(positional_args[positional_idx].clone());
        positional_idx += 1;
    }
    args.extend(old_args.iter().filter_map(|arg| {
        let (name, _) = named_function_arg_value(arg)?;
        (!consumed_named_args.contains(name)).then(|| arg.clone())
    }));
    Ok(args)
}

fn named_record_arg_value<'a>(
    args: &'a [rumoca_core::Expression],
    param_name: &str,
) -> Option<&'a rumoca_core::Expression> {
    args.iter().find_map(|arg| {
        let (name, value) = named_function_arg_value(arg)?;
        (name == param_name).then_some(value)
    })
}

fn named_function_arg_value(
    arg: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = arg
    else {
        return None;
    };
    let param_name = name.as_str().strip_prefix("__rumoca_named_arg__.")?;
    Some((param_name, args.first()?))
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
        let base_name =
            record_arg_projection_base_name(name, fields).unwrap_or_else(|| name.clone());
        for field in fields {
            out.push(rumoca_core::Expression::VarRef {
                name: base_name.with_appended_field(field),
                subscripts: vec![],
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

fn record_arg_projection_base_name(
    name: &rumoca_core::Reference,
    fields: &[String],
) -> Option<rumoca_core::Reference> {
    let component_ref = name.component_ref()?;
    let terminal = component_ref.last_ident()?;
    if !fields.iter().any(|field| field == terminal) {
        return None;
    }
    if component_ref.parts.len() > 1 {
        let mut base_ref = component_ref.clone();
        base_ref.parts.pop();
        base_ref.def_id = None;
        return Some(rumoca_core::Reference::from_component_reference(base_ref));
    }
    None
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
    let mut array_param_map = ArrayParamMap::default();
    for (name, func) in &dae.symbols.functions {
        let indices: Vec<(usize, String)> = func
            .inputs
            .iter()
            .enumerate()
            .filter(|(_, p)| !p.dims.is_empty())
            .map(|(index, parameter)| (index, parameter.name.clone()))
            .collect();
        if indices.is_empty() {
            continue;
        }
        if let Some(def_id) = func.def_id {
            array_param_map.by_def_id.insert(def_id, indices.clone());
        }
        array_param_map
            .by_name
            .insert(name.as_str().to_string(), indices);
    }

    if array_param_map.by_name.is_empty() {
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
    map: &ArrayParamMap,
) -> Result<(), ToDaeError> {
    let mut rewriter = DaeSizeArgInserter { map, error: None };
    *stmt = rewriter.rewrite_statement(stmt);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct DaeSizeArgInserter<'a> {
    map: &'a ArrayParamMap,
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
            if let Some(array_indices) = array_param_indices_for_call(self.map, name)
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

fn array_param_indices_for_call<'a>(
    map: &'a ArrayParamMap,
    call_name: &rumoca_core::Reference,
) -> Option<&'a Vec<(usize, String)>> {
    call_name
        .component_ref()
        .and_then(|reference| reference.def_id)
        .and_then(|def_id| map.by_def_id.get(&def_id))
        .or_else(|| map.by_name.get(call_name.as_str()))
}

fn insert_dae_size_args(
    args: &mut Vec<rumoca_core::Expression>,
    array_params: &[(usize, String)],
    call_span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    for (param_idx, param_name) in array_params.iter().rev() {
        let Some(arg_idx) = argument_index_for_param(args, *param_idx, param_name) else {
            continue;
        };
        // If the argument is an Array literal with known element count, use the
        // literal count directly instead of size(), which some backends cannot
        // render for compound literals.
        let size_expr =
            array_size_expr_for_arg(argument_value_for_size(&args[arg_idx]), call_span)?;
        args.insert(arg_idx + 1, size_expr);
    }
    Ok(())
}

fn argument_index_for_param(
    args: &[rumoca_core::Expression],
    param_idx: usize,
    param_name: &str,
) -> Option<usize> {
    args.iter()
        .position(|arg| named_function_arg_value(arg).is_some_and(|(name, _)| name == param_name))
        .or((param_idx < args.len()).then_some(param_idx))
}

fn argument_value_for_size(arg: &rumoca_core::Expression) -> &rumoca_core::Expression {
    if let Some((_, value)) = named_function_arg_value(arg) {
        return value;
    }
    arg
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
    let record_array_fields = build_record_array_field_map(dae);

    canonicalize_embedded_subscript_equation_list(&mut dae.continuous.equations, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.initialization.equations, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.real_updates, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.discrete.valued_updates, &array_dims)?;
    canonicalize_embedded_subscript_equation_list(&mut dae.conditions.equations, &array_dims)?;

    // Expanding an array equation into scalar rows shifts every later row, so the
    // partitions that carry structured families (continuous, initialization) must
    // re-point those families at their new row blocks. Without this a family after
    // an expanded phantom equation indexes the wrong rows downstream.
    let continuous_spans = scalarize_equation_list(
        &mut dae.continuous.equations,
        &phantom_map,
        &array_dims,
        &record_array_fields,
        &dae.symbols.functions,
        false,
    )?;
    rumoca_ir_dae::remap_structured_families_after_expansion(
        &mut dae.continuous.structured_equations,
        &continuous_spans,
    );
    let initialization_spans = scalarize_equation_list(
        &mut dae.initialization.equations,
        &phantom_map,
        &array_dims,
        &record_array_fields,
        &dae.symbols.functions,
        false,
    )?;
    rumoca_ir_dae::remap_structured_families_after_expansion(
        &mut dae.initialization.structured_equations,
        &initialization_spans,
    );
    // The discrete and condition partitions carry no structured families.
    scalarize_equation_list(
        &mut dae.discrete.real_updates,
        &phantom_map,
        &array_dims,
        &record_array_fields,
        &dae.symbols.functions,
        true,
    )?;
    scalarize_equation_list(
        &mut dae.discrete.valued_updates,
        &phantom_map,
        &array_dims,
        &record_array_fields,
        &dae.symbols.functions,
        true,
    )?;
    scalarize_equation_list(
        &mut dae.conditions.equations,
        &phantom_map,
        &array_dims,
        &record_array_fields,
        &dae.symbols.functions,
        false,
    )?;
    Ok(())
}

pub(crate) fn sync_materialized_structured_equation_templates(
    dae: &mut Dae,
) -> Result<(), ToDaeError> {
    let var_dims = build_dae_var_dims_map(dae);
    sync_structured_partition_templates(
        &dae.continuous.structured_equations,
        &mut dae.continuous.equations,
        &var_dims,
    )?;
    sync_structured_partition_templates(
        &dae.initialization.structured_equations,
        &mut dae.initialization.equations,
        &var_dims,
    )?;
    Ok(())
}

pub(crate) fn repair_external_table_event_handles(dae: &mut Dae) {
    let table_ids = dae
        .metadata
        .nonnumeric_variable_names
        .iter()
        .filter(|name| name.ends_with(".tableID"))
        .cloned()
        .collect::<HashSet<_>>();
    repair_external_table_event_handles_in_equations(&mut dae.discrete.real_updates, &table_ids);
    repair_external_table_event_handles_in_equations(&mut dae.discrete.valued_updates, &table_ids);
    repair_external_table_event_handles_in_equations(&mut dae.conditions.equations, &table_ids);
}

fn repair_external_table_event_handles_in_equations(
    equations: &mut [dae::Equation],
    table_ids: &HashSet<String>,
) {
    for equation in equations {
        let Some(prefix) = equation
            .lhs
            .as_ref()
            .and_then(|lhs| external_table_event_prefix(lhs.as_str()))
        else {
            continue;
        };
        let table_id_name = format!("{prefix}.tableID");
        if !table_ids.contains(&table_id_name) {
            continue;
        }
        equation.rhs = ExternalTableEventHandleRepair {
            table_id_name: &table_id_name,
            span: equation.span,
        }
        .rewrite_expression(&equation.rhs);
    }
}

fn external_table_event_prefix(name: &str) -> Option<&str> {
    name.strip_suffix(".nextTimeEventScaled")
        .or_else(|| name.strip_suffix(".nextTimeEvent"))
}

struct ExternalTableEventHandleRepair<'a> {
    table_id_name: &'a str,
    span: rumoca_core::Span,
}

impl ExpressionRewriter for ExternalTableEventHandleRepair<'_> {
    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let mut args = self.rewrite_expressions(args);
        if name.last_segment() == "getNextTimeEvent"
            && args
                .first()
                .is_some_and(external_table_constructor_call_dae)
        {
            let arg_span = args
                .first()
                .and_then(rumoca_core::Expression::span)
                .unwrap_or(self.span);
            args[0] = rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated(self.table_id_name.to_string()),
                subscripts: Vec::new(),
                span: arg_span,
            };
        }
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor,
            span,
        }
    }
}

fn external_table_constructor_call_dae(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if matches!(
            name.last_segment(),
            "ExternalCombiTimeTable" | "ExternalCombiTable1D"
        )
    )
}

fn sync_structured_partition_templates(
    families: &[dae::StructuredEquationFamily],
    equations: &mut [dae::Equation],
    var_dims: &HashMap<String, Vec<i64>>,
) -> Result<(), ToDaeError> {
    for family in families {
        if !family.interiors_materialized {
            continue;
        }
        let Some(template) = family.template.as_ref() else {
            continue;
        };
        if template.body.is_empty()
            || !family
                .equation_counts
                .iter()
                .all(|count| *count == template.body.len())
        {
            continue;
        }
        sync_structured_template_family(family, template, equations, var_dims)?;
    }
    Ok(())
}

fn sync_structured_template_family(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
    equations: &mut [dae::Equation],
    var_dims: &HashMap<String, Vec<i64>>,
) -> Result<(), ToDaeError> {
    let tuples = family.domain.index_tuples().map_err(|err| {
        ToDaeError::runtime_metadata_violation_at(
            format!("invalid structured equation domain: {err}"),
            family.span,
        )
    })?;
    for (iteration, tuple) in tuples.iter().enumerate() {
        let binder_values: HashMap<_, _> = family
            .domain
            .binders
            .iter()
            .zip(tuple.iter().copied())
            .map(|(binder, value)| (binder.display_name.clone(), value))
            .collect();
        let row_base = structured_template_row_base(family, template, iteration)?;
        for (position, body) in template.body.iter().enumerate() {
            let Some(equation) = equations.get_mut(row_base + position) else {
                return Err(ToDaeError::runtime_metadata_violation_at(
                    "structured equation template points past materialized equations".to_string(),
                    family.span,
                ));
            };
            equation.rhs = StructuredBinderSubstitution {
                values: &binder_values,
                span: family.span,
            }
            .rewrite_expression(body);
            if let Some(scalar_count) =
                structured_template_residual_scalar_count(&equation.rhs, var_dims)
            {
                equation.scalar_count = scalar_count;
            }
        }
    }
    Ok(())
}

fn build_dae_var_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut dims = HashMap::new();
    for partition in [
        &dae.variables.states,
        &dae.variables.algebraics,
        &dae.variables.inputs,
        &dae.variables.outputs,
        &dae.variables.parameters,
        &dae.variables.constants,
        &dae.variables.discrete_reals,
        &dae.variables.discrete_valued,
    ] {
        for (name, variable) in partition {
            dims.insert(name.as_str().to_string(), variable.dims.clone());
        }
    }
    dims
}

fn structured_template_residual_scalar_count(
    expr: &rumoca_core::Expression,
    var_dims: &HashMap<String, Vec<i64>>,
) -> Option<usize> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    expression_selected_scalar_count(lhs, var_dims)
}

fn expression_selected_scalar_count(
    expr: &rumoca_core::Expression,
    var_dims: &HashMap<String, Vec<i64>>,
) -> Option<usize> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => var_ref_selected_scalar_count(name.as_str(), subscripts, var_dims),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            var_ref_selected_scalar_count(name.as_str(), subscripts, var_dims)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if matches!(function, rumoca_core::BuiltinFunction::Der) && args.len() == 1 =>
        {
            expression_selected_scalar_count(&args[0], var_dims)
        }
        _ => None,
    }
}

fn var_ref_selected_scalar_count(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
    var_dims: &HashMap<String, Vec<i64>>,
) -> Option<usize> {
    let dims = var_dims.get(name)?;
    if subscripts.is_empty() {
        return Some(compute_var_size(dims));
    }
    projected_dims_for_subscripts(dims, subscripts).map(|dims| compute_var_size(&dims))
}

fn projected_dims_for_subscripts(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
) -> Option<Vec<i64>> {
    let mut remaining = Vec::new();
    let mut dim_idx = 0usize;
    for subscript in subscripts {
        if dim_idx >= dims.len() {
            break;
        }
        match subscript {
            rumoca_core::Subscript::Index { .. } => {
                dim_idx += 1;
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                if subscript_expr_selects_vector(expr)? {
                    return None;
                }
                dim_idx += 1;
            }
            rumoca_core::Subscript::Colon { .. } => {
                remaining.push(dims[dim_idx]);
                dim_idx += 1;
            }
        }
    }
    remaining.extend_from_slice(&dims[dim_idx..]);
    Some(remaining)
}

fn subscript_expr_selects_vector(expr: &rumoca_core::Expression) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value:
                rumoca_core::Literal::Integer(_)
                | rumoca_core::Literal::Real(_)
                | rumoca_core::Literal::Boolean(_),
            ..
        } => Some(false),
        rumoca_core::Expression::Range { .. } | rumoca_core::Expression::Array { .. } => Some(true),
        _ => None,
    }
}

fn structured_template_row_base(
    family: &dae::StructuredEquationFamily,
    template: &rumoca_core::ComprehensionTemplate,
    iteration: usize,
) -> Result<usize, ToDaeError> {
    family
        .first_equation_index
        .checked_add(iteration.checked_mul(template.body.len()).ok_or_else(|| {
            ToDaeError::runtime_metadata_violation_at(
                "structured equation row index overflows".to_string(),
                family.span,
            )
        })?)
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation_at(
                "structured equation row index overflows".to_string(),
                family.span,
            )
        })
}

struct StructuredBinderSubstitution<'a> {
    values: &'a HashMap<String, i64>,
    span: rumoca_core::Span,
}

impl ExpressionRewriter for StructuredBinderSubstitution<'_> {
    fn walk_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if subscripts.is_empty()
            && let Some(value) = self
                .values
                .get(name.as_str())
                .or_else(|| self.values.get(name.last_segment()))
        {
            return rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span: if span.is_dummy() { self.span } else { span },
            };
        }
        let subscripts = self.rewrite_subscripts(subscripts);
        if subscripts
            .iter()
            .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
        {
            return rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: name.clone(),
                    subscripts: Vec::new(),
                    span,
                }),
                subscripts,
                span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts,
            span,
        }
    }
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

fn build_record_array_field_map(dae: &Dae) -> RecordArrayFieldMap {
    let mut fields: HashMap<String, (BTreeMap<usize, rumoca_core::Reference>, Vec<i64>)> =
        HashMap::new();
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
            let Some(component_ref) = var.component_ref.as_ref() else {
                continue;
            };
            let Some((key, index)) = record_array_field_key(component_ref) else {
                continue;
            };
            let (indexed, field_dims) = fields.entry(key).or_default();
            if field_dims.is_empty() {
                *field_dims = var.dims.clone();
            }
            indexed.insert(
                index,
                rumoca_core::Reference::with_component_reference(
                    name.as_str(),
                    component_ref.clone(),
                ),
            );
        }
    }
    fields
        .into_iter()
        .filter_map(|(key, (indexed, field_dims))| {
            let mut variants = Vec::with_capacity(indexed.len());
            for expected in 1..=indexed.len() {
                let value = indexed.get(&expected)?.clone();
                variants.push(value);
            }
            Some((
                key,
                RecordArrayFieldVariants {
                    variants,
                    field_dims,
                },
            ))
        })
        .collect()
}

fn record_array_field_key(
    component_ref: &rumoca_core::ComponentReference,
) -> Option<(String, usize)> {
    if component_ref.parts.len() < 2 {
        return None;
    }
    let container_index = component_ref
        .parts
        .iter()
        .enumerate()
        .take(component_ref.parts.len() - 1)
        .rev()
        .find_map(|(index, part)| {
            single_positive_index_subscript(&part.subs).map(|sub| (index, sub))
        })?;
    let (container_index, _) = container_index;
    let suffix = component_ref.parts.get(container_index + 1..)?;
    if suffix.is_empty() {
        return None;
    }
    let suffix = suffix
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".");
    let container = component_ref.parts.get(container_index)?;
    let index = single_positive_index_subscript(&container.subs)?;
    let mut base_ref = component_ref.clone();
    base_ref.parts.truncate(container_index + 1);
    base_ref.parts.last_mut()?.subs.clear();
    base_ref.def_id = None;
    let base = rumoca_core::ComponentPath::from_component_reference(&base_ref).to_flat_string();
    Some((format!("{base}.{suffix}"), index))
}

fn single_positive_index_subscript(subscripts: &[rumoca_core::Subscript]) -> Option<usize> {
    let [rumoca_core::Subscript::Index { value, .. }] = subscripts else {
        return None;
    };
    usize::try_from(*value).ok().filter(|value| *value > 0)
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
        let subscripts = self.rewrite_subscripts(subscripts);
        if subscripts
            .iter()
            .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
        {
            return rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: name.clone(),
                    subscripts: Vec::new(),
                    span,
                }),
                subscripts,
                span,
            };
        }
        rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts,
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

#[cfg(test)]
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

fn expr_has_record_array_member_slice(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::FieldAccess { base, .. } => {
            matches!(
                base.as_ref(),
                rumoca_core::Expression::Index {
                    subscripts,
                    ..
                } if subscripts.iter().any(subscript_is_record_array_member_slice)
            ) || expr_has_record_array_member_slice(base)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_record_array_member_slice(lhs) || expr_has_record_array_member_slice(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_record_array_member_slice(rhs),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_record_array_member_slice)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_record_array_member_slice(condition)
                    || expr_has_record_array_member_slice(value)
            }) || expr_has_record_array_member_slice(else_branch)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(expr_has_record_array_member_slice)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_has_record_array_member_slice(start)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_has_record_array_member_slice(step))
                || expr_has_record_array_member_slice(end)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_has_record_array_member_slice(base)
                || subscripts
                    .iter()
                    .any(subscript_has_record_array_member_slice)
        }
        _ => false,
    }
}

fn expr_has_colon_slice(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            subscripts
                .iter()
                .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
                || expr_has_colon_slice(base)
                || subscripts.iter().any(subscript_has_colon_slice)
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_has_colon_slice(base),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_colon_slice(lhs) || expr_has_colon_slice(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_has_colon_slice(rhs),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_colon_slice)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_colon_slice(condition) || expr_has_colon_slice(value)
            }) || expr_has_colon_slice(else_branch)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(expr_has_colon_slice)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_has_colon_slice(start)
                || step.as_ref().is_some_and(|step| expr_has_colon_slice(step))
                || expr_has_colon_slice(end)
        }
        _ => false,
    }
}

#[cfg(test)]
fn subscript_has_array_comprehension(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expr_has_array_comprehension(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

fn expr_has_vectorized_scalar_function_call(
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> bool {
    match expr {
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => {
            let function = functions.get(name.var_name());
            if scalar_output_function(function) {
                let mut positional_idx = 0usize;
                if args.iter().any(|arg| {
                    let formal_rank =
                        scalarized_function_arg_formal_rank(function, arg, &mut positional_idx);
                    expr_has_vectorized_scalar_actual(arg, formal_rank, array_dims)
                }) {
                    return true;
                }
            }
            args.iter()
                .any(|arg| expr_has_vectorized_scalar_function_call(arg, array_dims, functions))
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_has_vectorized_scalar_function_call(lhs, array_dims, functions)
                || expr_has_vectorized_scalar_function_call(rhs, array_dims, functions)
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            expr_has_vectorized_scalar_function_call(rhs, array_dims, functions)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. }
        | rumoca_core::Expression::Array { elements: args, .. } => args
            .iter()
            .any(|arg| expr_has_vectorized_scalar_function_call(arg, array_dims, functions)),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_vectorized_scalar_function_call(condition, array_dims, functions)
                    || expr_has_vectorized_scalar_function_call(value, array_dims, functions)
            }) || expr_has_vectorized_scalar_function_call(else_branch, array_dims, functions)
        }
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_has_vectorized_scalar_function_call(base, array_dims, functions)
        }
        _ => false,
    }
}

fn expr_has_vectorized_scalar_actual(
    expr: &rumoca_core::Expression,
    formal_rank: usize,
    array_dims: &HashMap<String, Vec<i64>>,
) -> bool {
    if let Some((_, value)) = named_function_arg_value(expr) {
        return expr_has_vectorized_scalar_actual(value, formal_rank, array_dims);
    }
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if subscripts.is_empty()
            && array_dims
                .get(name.as_str())
                .is_some_and(|dims| dims.len() == formal_rank + 1)
    )
}

fn subscript_has_record_array_member_slice(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expr_has_record_array_member_slice(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

fn subscript_has_colon_slice(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expr_has_colon_slice(expr),
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
    record_array_fields: &RecordArrayFieldMap,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let ctx = ScalarizeExprContext {
        k,
        phantom_map,
        array_dims,
        record_array_fields,
        functions,
    };
    scalarize_expr_with_context(expr, &ctx)
}

struct ScalarizeExprContext<'a> {
    k: usize,
    phantom_map: &'a HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &'a HashMap<String, Vec<i64>>,
    record_array_fields: &'a RecordArrayFieldMap,
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
            ctx.record_array_fields,
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
            if let Some(expr) = scalarize_builtin_vector_output_at(*function, args, *span, ctx)? {
                return Ok(expr);
            }
            if let Some(expr) = scalarize_builtin_array_constructor_at(*function, args, *span, ctx)?
            {
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
            if let Some((element_index, element_lane)) =
                scalarized_array_literal_lane(elements, ctx.k, ctx.array_dims)
            {
                let element_ctx = ScalarizeExprContext {
                    k: element_lane,
                    phantom_map: ctx.phantom_map,
                    array_dims: ctx.array_dims,
                    record_array_fields: ctx.record_array_fields,
                    functions: ctx.functions,
                };
                scalarize_expr_with_context(&elements[element_index], &element_ctx)
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

fn scalarize_builtin_vector_output_at(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let Some(output_width) = builtin_fixed_vector_output_width(function) else {
        return Ok(None);
    };
    if ctx.k >= output_width {
        return Ok(None);
    }
    let index = one_based_scalar_index(ctx.k, span, "DAE scalarized builtin output subscript")?;
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::BuiltinCall {
            function,
            args: args
                .iter()
                .map(|arg| vectorize_builtin_vector_arg(arg, ctx))
                .collect::<Result<Vec<_>, _>>()?,
            span,
        }),
        subscripts: vec![generated_index_subscript(
            index,
            span,
            "DAE scalarized builtin output subscript",
        )?],
        span,
    }))
}

fn vectorize_builtin_vector_arg(
    arg: &rumoca_core::Expression,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let Some(vector) = try_project_colon_slice_var_ref(arg, ctx.array_dims)? {
        return Ok(vector);
    }
    Ok(vectorize_phantom_expr(arg, ctx.phantom_map))
}

fn builtin_fixed_vector_output_width(function: rumoca_core::BuiltinFunction) -> Option<usize> {
    match function {
        rumoca_core::BuiltinFunction::Cross => Some(3),
        _ => None,
    }
}

fn scalarize_index_expr_at(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let rumoca_core::Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base
        && base_subscripts.is_empty()
    {
        if subscripts
            .iter()
            .all(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
            && let Some(expr) = scalarize_var_ref_at(
                name,
                base_subscripts,
                span,
                ctx.k,
                ctx.phantom_map,
                ctx.array_dims,
                ctx.record_array_fields,
            )?
        {
            return Ok(expr);
        }
        if let Some(dims) = ctx.array_dims.get(name.as_str())
            && let Some(projected_subscripts) =
                project_slice_subscripts_for_lane(dims, subscripts, ctx.k, span)?
        {
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
                span,
            });
        }
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
    if let Some(projected) =
        scalarize_record_array_member_slice_at(base, field, span, ctx.k, ctx.record_array_fields)?
    {
        return Ok(projected);
    }
    if let Some(projected) = scalarize_record_array_field_at(base, field, span, ctx)? {
        return Ok(projected);
    }
    let base = scalarize_expr_with_context(base, ctx)?;
    if let rumoca_core::Expression::VarRef { name, span, .. } = &base
        && let rumoca_core::Expression::VarRef { subscripts, .. } = &base
        && subscripts.is_empty()
    {
        let base_name = record_arg_projection_base_name(name, &[field.to_string()])
            .unwrap_or_else(|| name.clone());
        return Ok(rumoca_core::Expression::VarRef {
            name: base_name.with_appended_field(field),
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

fn scalarize_record_array_field_at(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    project_record_array_field_rhs_at(base, field, span, ctx.k, ctx.record_array_fields)
}

fn project_record_array_field_rhs_at(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    k: usize,
    record_array_fields: &RecordArrayFieldMap,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = base
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }
    let key = format!("{}.{}", name.as_str(), field);
    project_record_array_field_entry_at(record_array_fields.get(&key), k, span)
}

fn project_record_array_field_entry_at(
    entry: Option<&RecordArrayFieldVariants>,
    k: usize,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let Some(entry) = entry else {
        return Ok(None);
    };
    let field_width = record_array_field_width(&entry.field_dims);
    if field_width == 0 {
        return Ok(None);
    }
    let record_index = k / field_width;
    let field_index = k % field_width;
    let Some(projected) = entry.variants.get(record_index).cloned() else {
        return Ok(None);
    };
    let subscripts = if field_width == 1 {
        Vec::new()
    } else {
        let index = one_based_scalar_index(
            field_index,
            span,
            "DAE record-array field scalarized subscript",
        )?;
        vec![generated_index_subscript(
            index,
            span,
            "DAE record-array field scalarized subscript",
        )?]
    };
    Ok(Some(rumoca_core::Expression::VarRef {
        name: projected,
        subscripts,
        span,
    }))
}

fn record_array_field_width(dims: &[i64]) -> usize {
    if dims.is_empty() {
        return 1;
    }
    dims.iter()
        .copied()
        .try_fold(1usize, |acc, dim| {
            let dim = usize::try_from(dim).ok()?;
            (dim > 0).then_some(acc.saturating_mul(dim))
        })
        .unwrap_or(0)
}

fn record_array_field_scalar_count(entry: &RecordArrayFieldVariants) -> usize {
    entry
        .variants
        .len()
        .saturating_mul(record_array_field_width(&entry.field_dims))
}

fn scalarize_record_array_member_slice_at(
    base: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    k: usize,
    record_array_fields: &RecordArrayFieldMap,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    if let Some((name, subscript, suffix)) = record_array_member_slice_parts(base, field)
        && let Some(component_ref) = name.component_ref()
        && let Some(key) = record_array_member_slice_field_key(component_ref, &suffix)
        && let Some(entry) = record_array_fields.get(&key)
        && let Some(projected) =
            project_record_array_field_slice_entry_at(entry, subscript, k, span)?
    {
        return Ok(Some(projected));
    }

    let rumoca_core::Expression::Index {
        base: inner,
        subscripts,
        ..
    } = base
    else {
        return Ok(None);
    };
    let [subscript] = subscripts.as_slice() else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name,
        subscripts: ref_subscripts,
        ..
    } = inner.as_ref()
    else {
        return Ok(None);
    };
    if !ref_subscripts.is_empty() {
        return Ok(None);
    }
    let Some(component_ref) = name.component_ref() else {
        return Ok(None);
    };
    let mut element_ref = component_ref.clone();
    let Some(part) = element_ref.parts.last_mut() else {
        return Ok(None);
    };
    let index = record_array_member_slice_index(subscript, k, span)?;
    part.subs = vec![generated_index_subscript(
        index,
        span,
        "DAE record-array member slice subscript",
    )?];
    element_ref.parts.push(rumoca_core::ComponentRefPart {
        ident: field.to_string(),
        span,
        subs: Vec::new(),
    });
    Ok(Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(element_ref),
        subscripts: Vec::new(),
        span,
    }))
}

fn record_array_member_slice_parts<'a>(
    base: &'a rumoca_core::Expression,
    field: &str,
) -> Option<(
    &'a rumoca_core::Reference,
    &'a rumoca_core::Subscript,
    Vec<String>,
)> {
    let (name, subscript, mut suffix) = collect_record_array_member_slice_base(base)?;
    suffix.push(field.to_string());
    Some((name, subscript, suffix))
}

fn collect_record_array_member_slice_base(
    expr: &rumoca_core::Expression,
) -> Option<(
    &rumoca_core::Reference,
    &rumoca_core::Subscript,
    Vec<String>,
)> {
    match expr {
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let [subscript] = subscripts.as_slice() else {
                return None;
            };
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: ref_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            ref_subscripts
                .is_empty()
                .then_some((name, subscript, Vec::new()))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let (name, subscript, mut suffix) = collect_record_array_member_slice_base(base)?;
            suffix.push(field.clone());
            Some((name, subscript, suffix))
        }
        _ => None,
    }
}

fn record_array_member_slice_field_key(
    component_ref: &rumoca_core::ComponentReference,
    suffix: &[String],
) -> Option<String> {
    if suffix.is_empty() {
        return None;
    }
    let mut base_ref = component_ref.clone();
    base_ref.parts.last_mut()?.subs.clear();
    base_ref.def_id = None;
    let base = rumoca_core::ComponentPath::from_component_reference(&base_ref).to_flat_string();
    Some(format!("{base}.{}", suffix.join(".")))
}

fn project_record_array_field_slice_entry_at(
    entry: &RecordArrayFieldVariants,
    subscript: &rumoca_core::Subscript,
    k: usize,
    span: rumoca_core::Span,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let field_width = record_array_field_width(&entry.field_dims);
    if field_width == 0 {
        return Ok(None);
    }
    let record_lane = k / field_width;
    let field_index = k % field_width;
    let element_index = record_array_member_slice_index(subscript, record_lane, span)?;
    let variant_index = usize::try_from(element_index.checked_sub(1).ok_or_else(|| {
        ToDaeError::runtime_metadata_violation(
            "record-array member slice produced a non-positive index".to_string(),
        )
    })?)
    .map_err(|_| {
        ToDaeError::runtime_metadata_violation(
            "record-array member slice index is outside supported range".to_string(),
        )
    })?;
    let Some(projected) = entry.variants.get(variant_index).cloned() else {
        return Ok(None);
    };
    let subscripts = if field_width == 1 {
        Vec::new()
    } else {
        let index = one_based_scalar_index(
            field_index,
            span,
            "DAE record-array member field scalarized subscript",
        )?;
        vec![generated_index_subscript(
            index,
            span,
            "DAE record-array member field scalarized subscript",
        )?]
    };
    Ok(Some(rumoca_core::Expression::VarRef {
        name: projected,
        subscripts,
        span,
    }))
}

fn subscript_is_record_array_member_slice(subscript: &rumoca_core::Subscript) -> bool {
    matches!(subscript, rumoca_core::Subscript::Colon { .. })
        || matches!(
            subscript,
            rumoca_core::Subscript::Expr { expr, .. }
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
        )
}

fn record_array_member_slice_index(
    subscript: &rumoca_core::Subscript,
    k: usize,
    span: rumoca_core::Span,
) -> Result<i64, ToDaeError> {
    match subscript {
        rumoca_core::Subscript::Colon { .. } => {
            one_based_scalar_index(k, span, "DAE record-array member slice subscript")
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            scalarized_record_array_subscript_index(expr, k).ok_or_else(|| {
                ToDaeError::runtime_metadata_violation(
                    "record-array member slice requires a compile-time integer or range subscript"
                        .to_string(),
                )
            })
        }
        rumoca_core::Subscript::Index { value, .. } => Ok(*value),
    }
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
    let start = integer_constant_value(start)?;
    let step = match step.as_deref() {
        Some(step) => integer_constant_value(step)?,
        None => 1,
    };
    Some(start + (k as i64) * step)
}

fn scalarized_record_array_subscript_index(
    expr: &rumoca_core::Expression,
    k: usize,
) -> Option<i64> {
    integer_constant_value(expr).or_else(|| scalarized_comprehension_index_value(expr, k))
}

fn integer_constant_value(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let value = integer_constant_value(rhs)?;
            match op {
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(value),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => value.checked_neg(),
                _ => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = integer_constant_value(lhs)?;
            let rhs = integer_constant_value(rhs)?;
            rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
        }
        _ => None,
    }
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
    record_array_fields: &RecordArrayFieldMap,
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
    if let Some(projected) =
        project_record_array_field_entry_at(record_array_fields.get(n), k, span)?
    {
        return Ok(Some(projected));
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
    let function = ctx.functions.get(&rumoca_core::VarName::new(name.as_str()));
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
        args: scalarize_function_call_args(args, function, ctx)?,
        is_constructor,
        span,
    })
}

fn scalarize_function_call_args(
    args: &[rumoca_core::Expression],
    function: Option<&rumoca_core::Function>,
    ctx: &ScalarizeExprContext<'_>,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    let scalar_output = scalar_output_function(function);
    let mut positional_idx = 0usize;
    args.iter()
        .enumerate()
        .map(|(index, arg)| {
            let formal_rank =
                scalarized_function_arg_formal_rank(function, arg, &mut positional_idx);
            if scalar_output
                && function_input_expects_array(function, arg, index)
                && expr_phantom_ref_width(arg, ctx.phantom_map).is_some()
            {
                return Ok(vectorize_phantom_expr(arg, ctx.phantom_map));
            }
            if scalar_output {
                return project_scalarized_function_arg_at(
                    arg,
                    formal_rank,
                    ctx.k,
                    ctx.array_dims,
                    ctx.record_array_fields,
                    ctx.functions,
                );
            }
            if function_input_expects_array(function, arg, index) {
                return Ok(vectorize_phantom_expr(arg, ctx.phantom_map));
            }
            scalarize_expr_with_context(arg, ctx)
        })
        .collect()
}

fn function_input_expects_array(
    function: Option<&rumoca_core::Function>,
    arg: &rumoca_core::Expression,
    index: usize,
) -> bool {
    let Some(function) = function else {
        return false;
    };
    let input_name = named_argument_input_name(arg);
    let param = input_name
        .and_then(|name| function.inputs.iter().find(|param| param.name == name))
        .or_else(|| function.inputs.get(index));
    param.is_some_and(|param| !param.dims.is_empty() || !param.shape_expr.is_empty())
}

fn named_argument_input_name(arg: &rumoca_core::Expression) -> Option<&str> {
    let rumoca_core::Expression::FunctionCall {
        name,
        is_constructor: true,
        ..
    } = arg
    else {
        return None;
    };
    name.as_str().strip_prefix("__rumoca_named_arg__.")
}

fn vectorize_phantom_array_formal_args(
    expr: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> rumoca_core::Expression {
    PhantomArrayFormalArgVectorizer {
        phantom_map,
        functions,
    }
    .rewrite_expression(expr)
}

struct PhantomArrayFormalArgVectorizer<'a> {
    phantom_map: &'a HashMap<String, Vec<rumoca_core::Reference>>,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
}

impl ExpressionRewriter for PhantomArrayFormalArgVectorizer<'_> {
    fn walk_function_call_expression(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let function = self
            .functions
            .get(&rumoca_core::VarName::new(name.as_str()));
        rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    if function_input_expects_array(function, arg, index) {
                        vectorize_phantom_expr(arg, self.phantom_map)
                    } else {
                        self.rewrite_expression(arg)
                    }
                })
                .collect(),
            is_constructor,
            span,
        }
    }
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

fn generated_colon_subscript(
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, ToDaeError> {
    rumoca_core::Subscript::try_generated_colon(span, context).map_err(|err| {
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
    ctx: &ScalarizeExprContext<'_>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    match function {
        rumoca_core::BuiltinFunction::Zeros => Ok(Some(real_literal(0.0, span))),
        rumoca_core::BuiltinFunction::Ones => Ok(Some(real_literal(1.0, span))),
        rumoca_core::BuiltinFunction::Fill => {
            let Some(value) = args.first() else {
                return Ok(None);
            };
            let value_lane = scalarized_fill_value_lane(
                value,
                ctx.k,
                ctx.phantom_map,
                ctx.array_dims,
                ctx.record_array_fields,
            );
            scalarize_expr_at(
                value,
                value_lane,
                ctx.phantom_map,
                ctx.array_dims,
                ctx.record_array_fields,
                ctx.functions,
            )
            .map(Some)
        }
        rumoca_core::BuiltinFunction::Identity => {
            let Some(n) = args.first().and_then(literal_positive_usize) else {
                return Ok(None);
            };
            let row = ctx.k / n;
            let col = ctx.k % n;
            Ok(Some(real_literal(if row == col { 1.0 } else { 0.0 }, span)))
        }
        _ => Ok(None),
    }
}

fn scalarized_fill_value_lane(
    value: &rumoca_core::Expression,
    k: usize,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
) -> usize {
    let Some(width) =
        scalarized_fill_value_width(value, phantom_map, array_dims, record_array_fields)
    else {
        return k;
    };
    if width <= 1 { 0 } else { k % width }
}

fn scalarized_fill_value_width(
    value: &rumoca_core::Expression,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
) -> Option<usize> {
    match value {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => phantom_map
            .get(name.as_str())
            .map(Vec::len)
            .or_else(|| {
                record_array_fields
                    .get(name.as_str())
                    .map(record_array_field_scalar_count)
            })
            .or_else(|| {
                array_dims
                    .get(name.as_str())
                    .map(|dims| compute_var_size(dims))
            }),
        rumoca_core::Expression::Array { elements, .. } => Some(elements.len()),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            if !base_subscripts.is_empty() {
                return None;
            }
            array_dims
                .get(name.as_str())
                .and_then(|dims| projected_dims_for_subscripts(dims, subscripts))
                .map(|dims| compute_var_size(&dims))
        }
        _ => None,
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
/// Scalarize phantom/comprehension array equations in place, returning one
/// `(new_start, new_len)` span per input equation (indexed by pre-expansion
/// position). An array equation that expands into `scalar_count` rows reports
/// `new_len == scalar_count`; every other equation reports `new_len == 1`. The
/// spans feed [`rumoca_ir_dae::remap_structured_families_after_expansion`] so
/// structured families stay pointed at their post-expansion row blocks.
fn scalarize_equation_list(
    equations: &mut Vec<dae::Equation>,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    recover_discrete_assignments: bool,
) -> Result<Vec<(usize, usize)>, ToDaeError> {
    let mut new_equations = Vec::with_capacity(equations.len());
    let mut spans = Vec::with_capacity(equations.len());
    for eq in equations.drain(..) {
        let new_start = new_equations.len();
        let phantom_width = expr_phantom_ref_width(&eq.rhs, phantom_map);
        let effective_scalar_count = if eq.scalar_count > 1 {
            eq.scalar_count
        } else {
            structured_template_residual_scalar_count(&eq.rhs, array_dims)
                .unwrap_or(eq.scalar_count)
        };
        if effective_scalar_count > 1
            && (phantom_width.is_some()
                || expr_has_record_array_member_slice(&eq.rhs)
                || expr_has_colon_slice(&eq.rhs)
                || expr_has_vectorized_scalar_function_call(&eq.rhs, array_dims, functions))
        {
            // Expand into scalar_count individual equations
            for k in 0..effective_scalar_count {
                let scalar_rhs = scalarize_expr_at(
                    &eq.rhs,
                    k,
                    phantom_map,
                    array_dims,
                    record_array_fields,
                    functions,
                )?;
                let origin = format!("{} [scalarized {}]", eq.origin, k + 1);
                new_equations.push(scalarized_equation_at(
                    &eq,
                    scalar_rhs,
                    k,
                    origin,
                    phantom_map,
                    array_dims,
                    recover_discrete_assignments,
                )?);
            }
        } else if phantom_width == Some(1) {
            let scalar_rhs = scalarize_expr_at(
                &eq.rhs,
                0,
                phantom_map,
                array_dims,
                record_array_fields,
                functions,
            )?;
            new_equations.push(dae::Equation {
                rhs: scalar_rhs,
                ..eq
            });
        } else if phantom_width.is_some() {
            let rhs = vectorize_phantom_array_formal_args(&eq.rhs, phantom_map, functions);
            new_equations.push(dae::Equation { rhs, ..eq });
        } else {
            new_equations.push(project_scalarized_residual_rhs(
                eq,
                phantom_map,
                array_dims,
                record_array_fields,
                functions,
            )?);
        }
        spans.push((new_start, new_equations.len() - new_start));
    }
    *equations = new_equations;
    Ok(spans)
}

fn project_scalarized_residual_rhs(
    eq: dae::Equation,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<dae::Equation, ToDaeError> {
    if eq.scalar_count != 1 {
        return Ok(eq);
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &eq.rhs
    else {
        let rhs = lower_colon_slice_dot_products(&eq.rhs, array_dims)?;
        return Ok(dae::Equation { rhs, ..eq });
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        let rhs = lower_colon_slice_dot_products(&eq.rhs, array_dims)?;
        return Ok(dae::Equation { rhs, ..eq });
    };
    let Some(k) = scalarized_lhs_zero_based_index_or_singleton(name, subscripts, array_dims) else {
        let rhs = lower_colon_slice_dot_products(&eq.rhs, array_dims)?;
        return Ok(dae::Equation { rhs, ..eq });
    };
    let _ = phantom_map;
    let scalar_lhs =
        project_scalarized_rhs_expr_at(lhs, k, array_dims, record_array_fields, functions)?;
    let scalar_rhs =
        project_scalarized_rhs_expr_at(rhs, k, array_dims, record_array_fields, functions)?;
    let scalar_lhs = lower_colon_slice_dot_products(&scalar_lhs, array_dims)?;
    let scalar_rhs = lower_colon_slice_dot_products(&scalar_rhs, array_dims)?;
    Ok(dae::Equation {
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(scalar_lhs),
            rhs: Box::new(scalar_rhs),
            span: *span,
        },
        ..eq
    })
}

fn project_scalarized_rhs_expr_at(
    expr: &rumoca_core::Expression,
    k: usize,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    RhsProjectionCtx {
        k,
        array_dims,
        record_array_fields,
        functions,
    }
    .project(expr)
}

fn lower_colon_slice_dot_products(
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    match expr {
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => Ok(rumoca_core::Expression::Index {
            base: Box::new(lower_colon_slice_dot_products(base, array_dims)?),
            subscripts: subscripts.clone(),
            span: *span,
        }),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
            lower_colon_slice_binary_expr(op, lhs, rhs, *span, array_dims)
        }
        rumoca_core::Expression::Unary { op, rhs, span } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(lower_colon_slice_dot_products(rhs, array_dims)?),
            span: *span,
        }),
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span,
        } => Ok(rumoca_core::Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|arg| lower_colon_slice_dot_products(arg, array_dims))
                .collect::<Result<Vec<_>, _>>()?,
            span: *span,
        }),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => Ok(rumoca_core::Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    Ok((
                        lower_colon_slice_dot_products(condition, array_dims)?,
                        lower_colon_slice_dot_products(value, array_dims)?,
                    ))
                })
                .collect::<Result<Vec<_>, ToDaeError>>()?,
            else_branch: Box::new(lower_colon_slice_dot_products(else_branch, array_dims)?),
            span: *span,
        }),
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span,
        } => Ok(rumoca_core::Expression::Array {
            elements: elements
                .iter()
                .map(|element| lower_colon_slice_dot_products(element, array_dims))
                .collect::<Result<Vec<_>, _>>()?,
            is_matrix: *is_matrix,
            span: *span,
        }),
        _ => Ok(expr.clone()),
    }
}

fn try_project_colon_slice_var_ref(
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
    let rumoca_core::Expression::Index {
        base,
        subscripts,
        span,
    } = expr
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base.as_ref()
    else {
        return Ok(None);
    };
    if !base_subscripts.is_empty() || !subscripts_have_colon(subscripts) {
        return Ok(None);
    }
    let Some(dims) = array_dims.get(name.as_str()) else {
        return Ok(None);
    };
    let Some(projected_dims) = projected_dims_for_subscripts(dims, subscripts) else {
        return Ok(None);
    };
    let scalar_count = compute_var_size(&projected_dims);
    if scalar_count <= 1 {
        return Ok(None);
    }
    let Some(elements) = project_colon_slice_elements(name, dims, subscripts, scalar_count, *span)?
    else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::Expression::Array {
        elements,
        is_matrix: false,
        span: *span,
    }))
}

fn subscripts_have_colon(subscripts: &[rumoca_core::Subscript]) -> bool {
    subscripts
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
}

fn project_colon_slice_elements(
    name: &rumoca_core::Reference,
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Expression>>, ToDaeError> {
    let mut elements = Vec::with_capacity(scalar_count);
    for idx in 0..scalar_count {
        let Some(projected_subscripts) =
            project_slice_subscripts_for_lane(dims, subscripts, idx, span)?
        else {
            return Ok(None);
        };
        elements.push(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: projected_subscripts,
            span,
        });
    }
    Ok(Some(elements))
}

fn lower_colon_slice_binary_expr(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if !matches!(op, rumoca_core::OpBinary::Mul) {
        return Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(lower_colon_slice_dot_products(lhs, array_dims)?),
            rhs: Box::new(lower_colon_slice_dot_products(rhs, array_dims)?),
            span,
        });
    }
    let lhs = lower_colon_slice_dot_operand(op, lhs, array_dims)?;
    let rhs = lower_colon_slice_dot_operand(op, rhs, array_dims)?;
    if let Some(dot) = dot_product_if_matching_vectors(op, &lhs, &rhs, span) {
        return Ok(dot);
    }
    Ok(vectorized_binary_expr(op.clone(), lhs, rhs, span))
}

fn lower_colon_slice_dot_operand(
    op: &rumoca_core::OpBinary,
    expr: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if matches!(op, rumoca_core::OpBinary::Mul)
        && let Some(projected) = try_project_colon_slice_var_ref(expr, array_dims)?
    {
        return Ok(projected);
    }
    lower_colon_slice_dot_products(expr, array_dims)
}

fn dot_product_if_matching_vectors(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    if !matches!(op, rumoca_core::OpBinary::Mul) {
        return None;
    }
    let (
        rumoca_core::Expression::Array {
            elements: lhs_values,
            ..
        },
        rumoca_core::Expression::Array {
            elements: rhs_values,
            ..
        },
    ) = (lhs, rhs)
    else {
        return None;
    };
    if lhs_values.len() != rhs_values.len() {
        return None;
    }
    dot_product_expr(lhs_values, rhs_values, span)
}

fn dot_product_expr(
    lhs_values: &[rumoca_core::Expression],
    rhs_values: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    let mut terms = lhs_values
        .iter()
        .cloned()
        .zip(rhs_values.iter().cloned())
        .map(|(lhs, rhs)| rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        });
    let first = terms.next()?;
    Some(
        terms.fold(first, |acc, term| rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(acc),
            rhs: Box::new(term),
            span,
        }),
    )
}

fn project_slice_subscripts_for_lane(
    dims: &[i64],
    subscripts: &[rumoca_core::Subscript],
    k: usize,
    span: rumoca_core::Span,
) -> Result<Option<Vec<rumoca_core::Subscript>>, ToDaeError> {
    let mut selected_dims = Vec::new();
    let mut dim_idx = 0usize;
    for subscript in subscripts {
        if dim_idx >= dims.len() {
            break;
        }
        match subscript {
            rumoca_core::Subscript::Index { .. } => {
                dim_idx += 1;
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                if subscript_expr_selects_vector(expr).unwrap_or(true) {
                    return Ok(None);
                }
                dim_idx += 1;
            }
            rumoca_core::Subscript::Colon { .. } => {
                selected_dims.push(dims[dim_idx]);
                dim_idx += 1;
            }
        }
    }
    selected_dims.extend_from_slice(&dims[dim_idx..]);
    if selected_dims.is_empty() {
        return Ok(None);
    }
    let Some(lane_indices) = lane_indices_for_dims(k, &selected_dims) else {
        return Ok(None);
    };
    let mut lane_iter = lane_indices.into_iter();
    let mut projected = Vec::with_capacity(subscripts.len() + dims.len().saturating_sub(dim_idx));
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Colon { .. } => {
                let Some(index) = lane_iter.next() else {
                    return Ok(None);
                };
                projected.push(generated_index_subscript(
                    index,
                    span,
                    "DAE scalarized colon slice projection",
                )?);
            }
            rumoca_core::Subscript::Index { .. } => projected.push(subscript.clone()),
            rumoca_core::Subscript::Expr { expr, .. } => {
                if subscript_expr_selects_vector(expr).unwrap_or(true) {
                    return Ok(None);
                }
                projected.push(subscript.clone());
            }
        }
    }
    for _ in dim_idx..dims.len() {
        let Some(index) = lane_iter.next() else {
            return Ok(None);
        };
        projected.push(generated_index_subscript(
            index,
            span,
            "DAE scalarized trailing array projection",
        )?);
    }
    Ok(Some(projected))
}

fn lane_indices_for_dims(k: usize, dims: &[i64]) -> Option<Vec<i64>> {
    let mut remaining = k;
    let mut indices = Vec::with_capacity(dims.len());
    for (idx, dim) in dims.iter().enumerate() {
        let dim = usize::try_from(*dim).ok()?;
        if dim == 0 {
            return None;
        }
        let stride =
            dims.get(idx + 1..)
                .unwrap_or_default()
                .iter()
                .try_fold(1usize, |acc, next_dim| {
                    let next_dim = usize::try_from(*next_dim).ok()?;
                    acc.checked_mul(next_dim)
                })?;
        let zero_based = remaining / stride;
        if zero_based >= dim {
            return None;
        }
        remaining %= stride;
        indices.push(i64::try_from(zero_based.checked_add(1)?).ok()?);
    }
    (remaining == 0).then_some(indices)
}

struct RhsProjectionCtx<'a> {
    k: usize,
    array_dims: &'a HashMap<String, Vec<i64>>,
    record_array_fields: &'a RecordArrayFieldMap,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
}

impl RhsProjectionCtx<'_> {
    fn project(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        match expr {
            rumoca_core::Expression::VarRef { .. } => self.project_var_ref(expr),
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => self.project_index(base, subscripts, *span, expr),
            rumoca_core::Expression::Array { elements, .. } => self.project_array(elements, expr),
            rumoca_core::Expression::ArrayComprehension {
                expr: inner,
                indices,
                filter,
                span,
            } => self.project_array_comprehension(inner, indices, filter.as_deref(), *span, expr),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.project_function_call(name, args, *is_constructor, *span),
            rumoca_core::Expression::FieldAccess { base, field, span } => {
                self.project_field_access(base, field, *span)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                self.project_binary(op, lhs, rhs, *span)
            }
            rumoca_core::Expression::Unary { op, rhs, span } => self.project_unary(op, rhs, *span),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => self.project_if(branches, else_branch, *span),
            _ => Ok(expr.clone()),
        }
    }

    fn project_var_ref(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return Ok(expr.clone());
        };
        if subscripts.is_empty() && self.array_dims.contains_key(name.as_str()) {
            let index = one_based_scalar_index(self.k, *span, "DAE scalar lhs RHS projection")?;
            return Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![generated_index_subscript(
                    index,
                    *span,
                    "DAE scalar lhs RHS projection",
                )?],
                span: *span,
            });
        }
        if subscripts.is_empty()
            && let Some(projected) = project_record_array_field_entry_at(
                self.record_array_fields.get(name.as_str()),
                self.k,
                *span,
            )?
        {
            return Ok(projected);
        }
        Ok(expr.clone())
    }

    fn project_index(
        &self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        let rumoca_core::Expression::VarRef {
            name,
            subscripts: base_subscripts,
            ..
        } = base
        else {
            return Ok(expr.clone());
        };
        if !base_subscripts.is_empty() {
            return Ok(expr.clone());
        }
        let Some(dims) = self.array_dims.get(name.as_str()) else {
            return Ok(expr.clone());
        };
        let Some(projected_subscripts) =
            project_slice_subscripts_for_lane(dims, subscripts, self.k, span)?
        else {
            return Ok(expr.clone());
        };
        Ok(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: projected_subscripts,
            span,
        })
    }

    fn project_array(
        &self,
        elements: &[rumoca_core::Expression],
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        if let Some((element_index, element_lane)) =
            scalarized_array_literal_lane(elements, self.k, self.array_dims)
        {
            return RhsProjectionCtx {
                k: element_lane,
                ..*self
            }
            .project(&elements[element_index]);
        }
        Ok(expr.clone())
    }

    fn project_array_comprehension(
        &self,
        inner: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        span: rumoca_core::Span,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        if filter.is_some() || indices.len() != 1 {
            return Ok(expr.clone());
        }
        let Some(value) = scalarized_comprehension_index_value(&indices[0].range, self.k) else {
            return Ok(expr.clone());
        };
        let mut substitution = ComprehensionIndexSubstitution {
            name: indices[0].name.clone(),
            value,
            span: indices[0].range.span().unwrap_or(span),
        };
        let selected = substitution.rewrite_expression(inner);
        self.project(&selected)
    }

    fn project_function_call(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        let function = self.functions.get(name.var_name());
        if scalar_output_function(function) {
            return self.project_function_call_with_formals(name, args, false, span, function);
        }
        if let Some(function) = function {
            return self.project_function_call_with_formals(
                name,
                args,
                is_constructor,
                span,
                Some(function),
            );
        }
        let projected_args = args
            .iter()
            .map(|arg| self.project(arg))
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: projected_args,
            is_constructor,
            span,
        })
    }

    fn project_function_call_with_formals(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
        function: Option<&rumoca_core::Function>,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        let mut positional_idx = 0usize;
        let projected_args = args
            .iter()
            .map(|arg| {
                let formal_rank =
                    scalarized_function_arg_formal_rank(function, arg, &mut positional_idx);
                project_scalarized_function_arg_at(
                    arg,
                    formal_rank,
                    self.k,
                    self.array_dims,
                    self.record_array_fields,
                    self.functions,
                )
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: projected_args,
            is_constructor,
            span,
        })
    }

    fn project_field_access(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        if let Some(projected) = scalarize_record_array_member_slice_at(
            base,
            field,
            span,
            self.k,
            self.record_array_fields,
        )? {
            return Ok(projected);
        }
        if let Some(projected) =
            project_record_array_field_rhs_at(base, field, span, self.k, self.record_array_fields)?
        {
            return Ok(projected);
        }
        Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(self.project(base)?),
            field: field.to_string(),
            span,
        })
    }

    fn project_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(self.project(lhs)?),
            rhs: Box::new(self.project(rhs)?),
            span,
        })
    }

    fn project_unary(
        &self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(self.project(rhs)?),
            span,
        })
    }

    fn project_if(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        let branches = branches
            .iter()
            .map(|(condition, value)| Ok((condition.clone(), self.project(value)?)))
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        Ok(rumoca_core::Expression::If {
            branches,
            else_branch: Box::new(self.project(else_branch)?),
            span,
        })
    }
}

fn scalarized_array_literal_lane(
    elements: &[rumoca_core::Expression],
    k: usize,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Option<(usize, usize)> {
    if elements.is_empty() {
        return None;
    }
    let widths = elements
        .iter()
        .map(|element| scalarized_array_literal_element_width(element, array_dims))
        .collect::<Option<Vec<_>>>()?;
    let width = *widths.first()?;
    if width == 0 || widths.iter().any(|candidate| *candidate != width) {
        return None;
    }
    let element_index = k / width;
    if element_index >= elements.len() {
        return None;
    }
    Some((element_index, k % width))
}

fn scalarized_array_literal_element_width(
    element: &rumoca_core::Expression,
    array_dims: &HashMap<String, Vec<i64>>,
) -> Option<usize> {
    match element {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => array_dims
            .get(name.as_str())
            .map(|dims| compute_var_size(dims))
            .or(Some(1)),
        _ => Some(1),
    }
}

fn scalar_output_function(function: Option<&rumoca_core::Function>) -> bool {
    matches!(function.and_then(|function| function.outputs.first()), Some(output) if output.dims.is_empty())
}

fn scalarized_function_arg_formal_rank(
    function: Option<&rumoca_core::Function>,
    arg: &rumoca_core::Expression,
    positional_idx: &mut usize,
) -> usize {
    let Some(function) = function else {
        return 0;
    };
    if let Some(input_name) = named_argument_input_name(arg) {
        return function
            .inputs
            .iter()
            .find(|input| input.name == input_name)
            .map_or(0, |input| input.dims.len());
    }
    let rank = function
        .inputs
        .get(*positional_idx)
        .map_or(0, |input| input.dims.len());
    *positional_idx += 1;
    rank
}

fn project_scalarized_function_arg_at(
    arg: &rumoca_core::Expression,
    formal_rank: usize,
    k: usize,
    array_dims: &HashMap<String, Vec<i64>>,
    record_array_fields: &RecordArrayFieldMap,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if let Some((_, value)) = named_function_arg_value(arg)
        && let rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            span,
            ..
        } = arg
    {
        return Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: vec![project_scalarized_function_arg_at(
                value,
                formal_rank,
                k,
                array_dims,
                record_array_fields,
                functions,
            )?],
            is_constructor: *is_constructor,
            span: *span,
        });
    }
    if let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = arg
        && is_stream_passthrough_intrinsic_dae(name.as_str())
    {
        let projected_args = match args.as_slice() {
            [inner] => vec![project_scalarized_function_arg_at(
                inner,
                formal_rank,
                k,
                array_dims,
                record_array_fields,
                functions,
            )?],
            _ => args.clone(),
        };
        return Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: projected_args,
            is_constructor: false,
            span: *span,
        });
    }
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = arg
    else {
        return project_scalarized_rhs_expr_at(arg, k, array_dims, record_array_fields, functions);
    };
    if !subscripts.is_empty() {
        return Ok(arg.clone());
    }
    if let Some(projected) =
        project_record_array_field_entry_at(record_array_fields.get(name.as_str()), k, *span)?
    {
        return Ok(projected);
    }
    let Some(dims) = array_dims.get(name.as_str()) else {
        return Ok(arg.clone());
    };
    if dims.len() == formal_rank {
        return Ok(arg.clone());
    }
    if dims.len() != formal_rank + 1 {
        return project_scalarized_rhs_expr_at(arg, k, array_dims, record_array_fields, functions);
    }
    let index = one_based_scalar_index(k, *span, "DAE scalarized function argument projection")?;
    let index_subscript =
        generated_index_subscript(index, *span, "DAE scalarized function argument projection")?;
    if formal_rank == 0 {
        return Ok(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: vec![index_subscript],
            span: *span,
        });
    }
    let mut projected_subscripts = Vec::with_capacity(dims.len());
    projected_subscripts.push(index_subscript);
    for _ in 0..formal_rank {
        projected_subscripts.push(generated_colon_subscript(
            *span,
            "DAE scalarized function argument projection",
        )?);
    }
    Ok(rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: Vec::new(),
            span: *span,
        }),
        subscripts: projected_subscripts,
        span: *span,
    })
}

fn is_stream_passthrough_intrinsic_dae(name: &str) -> bool {
    rumoca_core::qualified_type_name_matches(name, "actualStream")
        || rumoca_core::qualified_type_name_matches(name, "inStream")
}

fn scalarized_lhs_zero_based_index(subscripts: &[rumoca_core::Subscript]) -> Option<usize> {
    match subscripts {
        [subscript] => {
            let index = match subscript {
                rumoca_core::Subscript::Index { value, .. } => *value,
                rumoca_core::Subscript::Expr { expr, .. } => integer_literal_value(expr)?,
                rumoca_core::Subscript::Colon { .. } => return None,
            };
            usize::try_from(index.checked_sub(1)?).ok()
        }
        _ => None,
    }
}

fn scalarized_lhs_zero_based_index_or_singleton(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    array_dims: &HashMap<String, Vec<i64>>,
) -> Option<usize> {
    if let Some(index) = scalarized_lhs_zero_based_index(subscripts) {
        return Some(index);
    }
    if !subscripts.is_empty() {
        return None;
    }
    let dims = array_dims.get(name.as_str())?;
    (dims.iter().copied().product::<i64>() == 1).then_some(0)
}

fn scalarized_equation_at(
    eq: &dae::Equation,
    scalar_rhs: rumoca_core::Expression,
    k: usize,
    origin: String,
    phantom_map: &HashMap<String, Vec<rumoca_core::Reference>>,
    array_dims: &HashMap<String, Vec<i64>>,
    recover_discrete_assignments: bool,
) -> Result<dae::Equation, ToDaeError> {
    let Some(lhs) = &eq.lhs else {
        if recover_discrete_assignments
            && let Some((lhs, rhs)) = residual_assignment_parts(scalar_rhs.clone())?
        {
            return Ok(dae::Equation::explicit(lhs, rhs, eq.span, origin));
        }
        return Ok(dae::Equation::residual(scalar_rhs, eq.span, origin));
    };
    Ok(dae::Equation::explicit(
        scalarize_lhs_name_at(lhs.var_name(), k, phantom_map, array_dims, eq.span)?,
        scalar_rhs,
        eq.span,
        origin,
    ))
}

fn residual_assignment_parts(
    expr: rumoca_core::Expression,
) -> Result<Option<(rumoca_core::Reference, rumoca_core::Expression)>, ToDaeError> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return Ok(None);
    };
    let Some(lhs) = assignment_target_reference(*lhs)? else {
        return Ok(None);
    };
    Ok(Some((lhs, *rhs)))
}

fn assignment_target_reference(
    expr: rumoca_core::Expression,
) -> Result<Option<rumoca_core::Reference>, ToDaeError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name));
            }
            if let Some(mut component_ref) = name.component_ref().cloned() {
                let Some(part) = component_ref.parts.last_mut() else {
                    return Ok(None);
                };
                part.subs.extend(subscripts);
                return Ok(Some(rumoca_core::Reference::from_component_reference(
                    component_ref,
                )));
            }
            let Some(rendered) = render_literal_subscripted_reference(name.as_str(), &subscripts)
            else {
                return Ok(None);
            };
            if name.is_generated() {
                Ok(Some(rumoca_core::Reference::generated(rendered)))
            } else {
                Ok(Some(rumoca_core::Reference::new(rendered)))
            }
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            let Some(base) = assignment_target_reference(*base)? else {
                return Ok(None);
            };
            Ok(Some(base.with_appended_field(&field)))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some(mut base) = assignment_target_reference(*base)? else {
                return Ok(None);
            };
            for subscript in subscripts {
                let rumoca_core::Subscript::Index { value, span } = subscript else {
                    return Ok(None);
                };
                base = base.with_appended_index(
                    value,
                    span.require_provenance("DAE assignment target index")
                        .map_err(|error| {
                            ToDaeError::runtime_metadata_violation(error.to_string())
                        })?,
                );
            }
            Ok(Some(base))
        }
        _ => Ok(None),
    }
}

fn render_literal_subscripted_reference(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    let mut rendered = base.to_string();
    for subscript in subscripts {
        let rumoca_core::Subscript::Index { value, .. } = subscript else {
            return None;
        };
        rendered.push('[');
        rendered.push_str(&value.to_string());
        rendered.push(']');
    }
    Some(rendered)
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
