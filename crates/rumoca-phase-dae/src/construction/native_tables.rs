//! Recognition and constant folding of the native Modelica Standard Library
//! table ExternalObject family.
//!
//! An MLS §12.9.7 ExternalObject is an opaque C handle produced only by its own
//! constructor. The standard-library table blocks
//! (`Modelica.Blocks.Tables.CombiTable1D`/`CombiTable1Ds` and
//! `Modelica.Blocks.Sources.CombiTimeTable`) back their `tableID` handle with
//! the `ModelicaStandardTables` C runtime. This module identifies exactly those
//! handle types by the declared C symbol of their constructor (MLS §12.9), so
//! the DAE can model the handle as an opaque integer table id and fold each
//! parameter-constant constructor into a loaded table descriptor the solver's
//! native table operators interpolate, instead of executing foreign C code.
//!
//! Identity is taken from the constructor's external C symbol, never from the
//! Modelica display name: a user type spelled the same but bound to different C
//! code is not recognized and keeps failing at the value-type boundary.

use rumoca_core::{DefId, Expression, ExternalTableData, Reference, Span, VarName};
use rumoca_eval_flat::constant::{EvalContext, Value, eval_expr};
use rumoca_ir_dae::NativeTableOperator;
use rumoca_ir_flat as flat;

use crate::ToDaeError;

const COMBI_TABLE_1D_PREFIX: &str = "ModelicaStandardTables_CombiTable1D_";
const COMBI_TIME_TABLE_PREFIX: &str = "ModelicaStandardTables_CombiTimeTable_";
const NO_FILE_SENTINEL: &str = "NoName";

/// One recognized native table ExternalObject family. Only the families the
/// native table runtime interpolates are recognized; a two-dimensional table
/// (`ModelicaStandardTables_CombiTable2D_*`) has no runtime operator and so is
/// deliberately absent, keeping such handles at the unsupported boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TableFamily {
    CombiTable1D,
    CombiTimeTable,
}

/// The native table family a type identity backs, if any.
///
/// A type is a native table handle exactly when it is an ExternalObject
/// (MLS §12.9.7) whose constructor's external C symbol (MLS §12.9) is one of
/// the recognized `ModelicaStandardTables` entry points.
pub(crate) fn native_table_family(flat: &flat::Model, type_def_id: DefId) -> Option<TableFamily> {
    let constructor = external_object_constructor(flat, type_def_id)?;
    classify_symbol(constructor.external.as_ref()?.function_name.as_deref()?)
}

/// The native table family backing one flat variable's declared type.
pub(crate) fn native_table_family_of_variable(
    flat: &flat::Model,
    variable: &flat::Variable,
) -> Option<TableFamily> {
    native_table_family(flat, variable_type_def_id(flat, variable)?)
}

fn classify_symbol(symbol: &str) -> Option<TableFamily> {
    let family = if symbol.starts_with(COMBI_TABLE_1D_PREFIX) {
        TableFamily::CombiTable1D
    } else if symbol.starts_with(COMBI_TIME_TABLE_PREFIX) {
        TableFamily::CombiTimeTable
    } else {
        return None;
    };
    // Identify the handle type by its constructor, whose ModelicaStandardTables
    // entry point initializes a table (`_init*`). A value, bounds, or next-event
    // accessor shares the family prefix but merely returns a Real, and must not
    // be mistaken for a handle constructor.
    let entry_point = symbol.rsplit('_').next()?;
    entry_point.starts_with("init").then_some(family)
}

/// The resolved declaration identity of one flat variable's nominal type.
fn variable_type_def_id(flat: &flat::Model, variable: &flat::Variable) -> Option<DefId> {
    let nominal = flat.effective_types.get(&variable.type_id)?.nominal_type();
    flat.type_ids_by_def_id
        .iter()
        .find_map(|(def_id, type_id)| (*type_id == nominal).then_some(*def_id))
}

/// The ExternalObject constructor for a type identity, if the Flat IR retained
/// it.
///
/// MLS §12.9.7 lets an ExternalObject value be produced only by its own
/// constructor, an external function that returns the external-object type. So
/// a Flat function with an external body whose single output declares
/// `type_def_id` is that type's constructor, and its presence proves the type
/// is an ExternalObject rather than a record or a predefined type.
fn external_object_constructor(
    flat: &flat::Model,
    type_def_id: DefId,
) -> Option<&rumoca_core::Function> {
    flat.functions.values().find(|function| {
        function.external.is_some()
            && matches!(
                function.outputs.as_slice(),
                [output] if output.type_def_id == Some(type_def_id)
            )
    })
}

/// One flat variable that declares a native table handle.
struct NativeTableHandle<'flat> {
    name: &'flat VarName,
    variable: &'flat flat::Variable,
    type_def_id: DefId,
}

/// The native table handle variables of a flat model, in a deterministic
/// declaration order that assigns each its stable one-based table id.
fn native_table_handles(flat: &flat::Model) -> Vec<NativeTableHandle<'_>> {
    let mut handles = Vec::new();
    for (name, variable) in &flat.variables {
        let Some(type_def_id) = variable_type_def_id(flat, variable) else {
            continue;
        };
        if native_table_family(flat, type_def_id).is_some() {
            handles.push(NativeTableHandle {
                name,
                variable,
                type_def_id,
            });
        }
    }
    handles
}

/// The stable one-based table id of one native table handle variable.
///
/// Ids are assigned by declaration order so the DAE binding rewrite and the
/// [`build_external_tables`] descriptor list agree without shared mutable
/// state. The runtime requires a finite positive integer id.
pub(crate) fn native_table_id(flat: &flat::Model, name: &VarName) -> Option<u64> {
    native_table_handles(flat)
        .iter()
        .position(|handle| handle.name == name)
        .map(|index| index as u64 + 1)
}

/// Fold every native table handle constructor into a loaded table descriptor.
///
/// Each handle's constructor call (MLS §12.9.7) is evaluated from its
/// parameter-constant arguments: the table matrix, selected columns, and the
/// smoothness and extrapolation enumerations. A table read from a file
/// (`tableOnFile`, signalled by a non-`"NoName"` file argument) and a
/// non-constant argument are both rejected with a precise diagnostic, since the
/// solver interpolates only in-memory parameter tables.
pub(crate) fn build_external_tables(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<Vec<ExternalTableData>, ToDaeError> {
    let mut tables = Vec::new();
    for (index, handle) in native_table_handles(flat).into_iter().enumerate() {
        tables.push(build_one_table(flat, constants, &handle, index as u64 + 1)?);
    }
    Ok(tables)
}

fn build_one_table(
    flat: &flat::Model,
    constants: &EvalContext,
    handle: &NativeTableHandle<'_>,
    id: u64,
) -> Result<ExternalTableData, ToDaeError> {
    let span = handle.variable.source_span;
    let binding = handle.variable.binding.as_ref().ok_or_else(|| {
        unsupported(
            handle.name,
            "a native table handle must be bound to its constructor call",
            span,
        )
    })?;
    let Expression::FunctionCall { args, .. } = binding else {
        return Err(unsupported(
            handle.name,
            "a native table handle binding must be a constructor call",
            span,
        ));
    };
    let constructor = external_object_constructor(flat, handle.type_def_id).ok_or_else(|| {
        unsupported(
            handle.name,
            "the native table constructor declaration is not retained",
            span,
        )
    })?;
    let call = ConstructorCall {
        constants,
        constructor,
        args,
        name: handle.name,
        span,
    };
    call.reject_file_table()?;
    Ok(ExternalTableData {
        id,
        data: call.matrix()?,
        columns: call.columns()?,
        smoothness: call.enum_code("smoothness", smoothness_code)?,
        extrapolation: call.enum_code("extrapolation", extrapolation_code)?,
    })
}

/// The least or greatest abscissa of a native table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AbscissaBound {
    Least,
    Greatest,
}

impl AbscissaBound {
    /// The native table bounds accessor a C symbol denotes, if it is one.
    ///
    /// A `ModelicaStandardTables` bounds accessor (`minimumTime`/`maximumTime`
    /// for a time table, `minimumAbscissa`/`maximumAbscissa` for a 1D table,
    /// MLS §12.9) reads the least or greatest abscissa of its table. The value
    /// and next-event accessors (`getValue`/`getDerValue`/`nextTimeEvent`)
    /// depend on their abscissa argument and so are not abscissa bounds.
    fn from_symbol(symbol: &str) -> Option<Self> {
        match NativeTableOperator::from_symbol(symbol)? {
            NativeTableOperator::BoundsMin => Some(Self::Least),
            NativeTableOperator::BoundsMax => Some(Self::Greatest),
            NativeTableOperator::Lookup
            | NativeTableOperator::Slope
            | NativeTableOperator::NextEvent => None,
        }
    }

    /// Reduce the abscissa column (the table matrix's first column) of an
    /// in-memory table to this bound.
    fn reduce(self, table: &ExternalTableData) -> Option<f64> {
        let abscissa = table.data.iter().map(|row| row.first().copied());
        let mut abscissa = abscissa.collect::<Option<Vec<_>>>()?.into_iter();
        let first = abscissa.next()?;
        Some(abscissa.fold(first, |bound, value| match self {
            Self::Least => bound.min(value),
            Self::Greatest => bound.max(value),
        }))
    }
}

/// The constant Real value a native table bounds accessor call denotes, if the
/// call is such an accessor on a recognized in-memory constant table.
///
/// The bounds of a parameter-constant in-memory table are known at translation:
/// the least or greatest abscissa is the minimum or maximum of the table
/// matrix's first column (MLS §12.9). Folding the accessor to that Real literal
/// keeps the opaque table handle off every numeric path that cannot execute the
/// foreign C body, including the structural state-selection trial evaluator that
/// seeds parameter values before selecting an independent basis.
///
/// Returns `None` when the call is not a recognized bounds accessor applied
/// directly to a recognized constant-table handle, so ordinary call lowering
/// proceeds unchanged for every other call, including the value, slope, and
/// next-event accessors whose result depends on their abscissa argument.
pub(crate) fn native_table_bounds_literal(
    flat: &flat::Model,
    constants: &EvalContext,
    name: &Reference,
    args: &[Expression],
) -> Option<f64> {
    let symbol = flat
        .functions
        .get(name.var_name())?
        .external
        .as_ref()?
        .function_name
        .as_deref()?;
    let bound = AbscissaBound::from_symbol(symbol)?;
    let [
        Expression::VarRef {
            name: handle,
            subscripts,
            ..
        },
    ] = args
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let handle_name = handle.var_name();
    let (index, handle) = native_table_handles(flat)
        .into_iter()
        .enumerate()
        .find(|(_, handle)| handle.name == handle_name)?;
    // The identical descriptor is built for every recognized handle by
    // `build_external_tables` before expression lowering begins, so this build
    // cannot fail here; treat any failure as "not foldable" and let the raw
    // call surface its own diagnostic rather than masking it.
    let table = build_one_table(flat, constants, &handle, index as u64 + 1).ok()?;
    bound.reduce(&table)
}

/// The fixed inputs of one native table constructor call, evaluated by name.
struct ConstructorCall<'flat> {
    constants: &'flat EvalContext,
    constructor: &'flat rumoca_core::Function,
    args: &'flat [Expression],
    name: &'flat VarName,
    span: Span,
}

impl ConstructorCall<'_> {
    /// One constructor argument, matched to its declared parameter name and
    /// falling back to that parameter's default when the call omitted it.
    fn argument(&self, parameter: &str) -> Option<&Expression> {
        let ordinal = self
            .constructor
            .inputs
            .iter()
            .position(|input| input.name == parameter)?;
        self.args
            .get(ordinal)
            .or(self.constructor.inputs[ordinal].default.as_ref())
    }

    fn evaluate(&self, parameter: &str) -> Result<Value, ToDaeError> {
        let expression = self.argument(parameter).ok_or_else(|| {
            self.reject(&format!("the native table constructor omits `{parameter}`"))
        })?;
        eval_expr(expression, self.constants).map_err(|_| {
            self.reject(&format!(
                "the native table constructor argument `{parameter}` is not parameter-constant"
            ))
        })
    }

    fn reject_file_table(&self) -> Result<(), ToDaeError> {
        for parameter in ["fileName", "tableName"] {
            if self.argument(parameter).is_none() {
                continue;
            }
            if let Value::String(text) = &self.evaluate(parameter)?
                && text != NO_FILE_SENTINEL
            {
                return Err(self.reject(
                    "a table read from a file (tableOnFile) is unsupported; only in-memory parameter tables are interpolated",
                ));
            }
        }
        Ok(())
    }

    fn matrix(&self) -> Result<Vec<Vec<f64>>, ToDaeError> {
        let value = self.evaluate("table")?;
        let rows = value
            .as_array()
            .ok_or_else(|| self.reject("the native table matrix is not an array"))?;
        let matrix = rows
            .iter()
            .map(|row| self.matrix_row(row))
            .collect::<Result<Vec<_>, _>>()?;
        if matrix.is_empty() || matrix.first().is_some_and(Vec::is_empty) {
            return Err(self.reject(
                "the native table matrix is empty; a file table (tableOnFile) is unsupported",
            ));
        }
        Ok(matrix)
    }

    /// One row of the constructor's table matrix as numeric cells.
    fn matrix_row(&self, row: &Value) -> Result<Vec<f64>, ToDaeError> {
        let cells = row
            .as_array()
            .ok_or_else(|| self.reject("the native table matrix is not two-dimensional"))?;
        cells
            .iter()
            .map(|cell| {
                cell.to_real()
                    .ok_or_else(|| self.reject("a native table matrix entry is not numeric"))
            })
            .collect()
    }

    fn columns(&self) -> Result<Vec<usize>, ToDaeError> {
        let value = self.evaluate("columns")?;
        let entries = value
            .as_array()
            .ok_or_else(|| self.reject("the native table columns are not an array"))?;
        let mut columns = Vec::with_capacity(entries.len());
        for entry in entries {
            let column = entry
                .as_integer()
                .and_then(|column| usize::try_from(column).ok());
            columns.push(column.ok_or_else(|| {
                self.reject("a native table column index is not a positive integer")
            })?);
        }
        Ok(columns)
    }

    /// The runtime code for an enumeration-valued constructor argument.
    ///
    /// A Modelica enumeration value is one-based and identical to the runtime
    /// smoothness/extrapolation code, so an evaluated ordinal is used directly;
    /// an `Enum(type, literal)` value is mapped by its literal name for the same
    /// code (MLS §4.8.5).
    fn enum_code(
        &self,
        parameter: &str,
        classify: fn(&str) -> Option<i64>,
    ) -> Result<i64, ToDaeError> {
        let value = self.evaluate(parameter)?;
        if let Some(code) = value.as_integer() {
            return Ok(code);
        }
        let (_, literal) = value.as_enum().ok_or_else(|| {
            self.reject(&format!(
                "the native table `{parameter}` is not an enumeration value"
            ))
        })?;
        classify(literal).ok_or_else(|| {
            self.reject(&format!(
                "the native table `{parameter}` value `{literal}` is not recognized"
            ))
        })
    }

    fn reject(&self, reason: &str) -> ToDaeError {
        unsupported(self.name, reason, self.span)
    }
}

/// MLS `Modelica.Blocks.Types.Smoothness` enumeration values, as the native
/// table runtime interprets them (constant-segment interpolation is code 3;
/// every other smoothness is interpolated linearly).
fn smoothness_code(literal: &str) -> Option<i64> {
    match literal {
        "LinearSegments" => Some(1),
        "ContinuousDerivative" => Some(2),
        "ConstantSegments" => Some(3),
        "MonotoneContinuousDerivative1" => Some(4),
        "MonotoneContinuousDerivative2" => Some(5),
        "ModifiedContinuousDerivative" => Some(6),
        _ => None,
    }
}

/// MLS `Modelica.Blocks.Types.Extrapolation` enumeration values, matching the
/// runtime extrapolation modes (hold, linear, periodic, none).
fn extrapolation_code(literal: &str) -> Option<i64> {
    match literal {
        "HoldLastPoint" => Some(1),
        "LastTwoPoints" => Some(2),
        "Periodic" => Some(3),
        "NoExtrapolation" => Some(4),
        _ => None,
    }
}

fn unsupported(name: &VarName, reason: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "native table constructor",
        format!("`{name}`: {reason}"),
        span,
    )
}

#[cfg(test)]
mod tests {
    use super::{
        AbscissaBound, ExternalTableData, TableFamily, classify_symbol, extrapolation_code,
        smoothness_code,
    };

    fn abscissa_table(abscissa: &[f64]) -> ExternalTableData {
        // A native table matrix is abscissa-major: the first column is the
        // abscissa and any further columns are ordinate data the bound ignores.
        ExternalTableData {
            id: 1,
            data: abscissa.iter().map(|&t| vec![t, t * 10.0]).collect(),
            columns: vec![2],
            smoothness: 1,
            extrapolation: 1,
        }
    }

    #[test]
    fn only_a_bounds_accessor_symbol_names_an_abscissa_bound() {
        // The least/greatest abscissa accessors of both families name a bound;
        // the value, slope, and next-event accessors depend on their abscissa
        // argument and so are not compile-time abscissa bounds.
        assert_eq!(
            AbscissaBound::from_symbol("ModelicaStandardTables_CombiTimeTable_minimumTime"),
            Some(AbscissaBound::Least)
        );
        assert_eq!(
            AbscissaBound::from_symbol("ModelicaStandardTables_CombiTimeTable_maximumTime"),
            Some(AbscissaBound::Greatest)
        );
        assert_eq!(
            AbscissaBound::from_symbol("ModelicaStandardTables_CombiTable1D_minimumAbscissa"),
            Some(AbscissaBound::Least)
        );
        assert_eq!(
            AbscissaBound::from_symbol("ModelicaStandardTables_CombiTable1D_maximumAbscissa"),
            Some(AbscissaBound::Greatest)
        );
        for value_or_event in [
            "ModelicaStandardTables_CombiTimeTable_getValue",
            "ModelicaStandardTables_CombiTable1D_getDerValue",
            "ModelicaStandardTables_CombiTimeTable_nextTimeEvent",
            "my_foreign_symbol",
        ] {
            assert_eq!(AbscissaBound::from_symbol(value_or_event), None);
        }
    }

    #[test]
    fn a_bound_reduces_the_abscissa_column_to_its_extreme() {
        let table = abscissa_table(&[-1.0, 0.0, 2.5, 7.0]);
        assert_eq!(AbscissaBound::Least.reduce(&table), Some(-1.0));
        assert_eq!(AbscissaBound::Greatest.reduce(&table), Some(7.0));
        // A single-row table has one abscissa that is both its least and its
        // greatest.
        let single = abscissa_table(&[3.0]);
        assert_eq!(AbscissaBound::Least.reduce(&single), Some(3.0));
        assert_eq!(AbscissaBound::Greatest.reduce(&single), Some(3.0));
    }

    #[test]
    fn an_unsorted_abscissa_column_still_reduces_to_its_extremes() {
        // The bound is the column extreme, not the endpoint, so a table whose
        // abscissa is not stored in ascending order still yields the true
        // minimum and maximum.
        let table = abscissa_table(&[4.0, 1.0, 9.0, 2.0]);
        assert_eq!(AbscissaBound::Least.reduce(&table), Some(1.0));
        assert_eq!(AbscissaBound::Greatest.reduce(&table), Some(9.0));
    }

    #[test]
    fn only_a_native_table_constructor_symbol_identifies_a_handle() {
        // The `_init*` entry point constructs a handle, so it names the family.
        assert_eq!(
            classify_symbol("ModelicaStandardTables_CombiTable1D_init3"),
            Some(TableFamily::CombiTable1D)
        );
        assert_eq!(
            classify_symbol("ModelicaStandardTables_CombiTimeTable_init3"),
            Some(TableFamily::CombiTimeTable)
        );
    }

    #[test]
    fn accessors_two_dim_tables_and_foreign_handles_are_not_recognized() {
        // A value, bounds, or next-event accessor shares the family prefix but
        // is not a constructor; a two-dimensional table has no runtime operator;
        // an unrelated ExternalObject constructor is a foreign C symbol. Each
        // stays unrecognized, so it is rejected at the value-type boundary.
        for symbol in [
            "ModelicaStandardTables_CombiTable1D_getValue",
            "ModelicaStandardTables_CombiTimeTable_nextTimeEvent",
            "ModelicaStandardTables_CombiTable1D_close",
            "ModelicaStandardTables_CombiTable2D_init3",
            "my_make_handle",
        ] {
            assert_eq!(classify_symbol(symbol), None);
        }
    }

    #[test]
    fn smoothness_and_extrapolation_codes_match_the_runtime() {
        assert_eq!(smoothness_code("LinearSegments"), Some(1));
        assert_eq!(smoothness_code("ConstantSegments"), Some(3));
        assert_eq!(smoothness_code("ContinuousDerivative"), Some(2));
        assert_eq!(smoothness_code("Unrecognized"), None);
        assert_eq!(extrapolation_code("HoldLastPoint"), Some(1));
        assert_eq!(extrapolation_code("Periodic"), Some(3));
        assert_eq!(extrapolation_code("NoExtrapolation"), Some(4));
        assert_eq!(extrapolation_code("Unrecognized"), None);
    }
}
