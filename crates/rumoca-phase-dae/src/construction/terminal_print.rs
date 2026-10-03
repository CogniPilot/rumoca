//! Calls of the foreign terminal printer (MLS 3.7 §12.9).
//!
//! `Modelica.Utilities.Streams.print(string, fileName)` is an impure external
//! function whose foreign body, `ModelicaInternal_print`, writes `string` and
//! a line end to the terminal when `fileName` is empty and appends it to the
//! file otherwise. The call is identified by that entry point and the
//! declared interface `(string, fileName)` over two String inputs, never by
//! the Modelica name. A call whose file name is the empty String literal is a
//! terminal print, which DAE construction owns as one event action; a call
//! that writes a file stays an unsupported result-less call.

use rumoca_core::{Expression, Literal, Reference};
use rumoca_ir_flat as flat;

const ENTRY_POINT: &str = "ModelicaInternal_print";

/// The message of a terminal print call of `callee` with `arguments`.
pub(in crate::construction) fn terminal_print_message<'a>(
    flat: &flat::Model,
    callee: &Reference,
    arguments: &'a [Expression],
) -> Option<&'a Expression> {
    let function = flat.functions.get(callee.var_name())?;
    let external = function.external.as_ref()?;
    let [string, file_name] = function.inputs.as_slice() else {
        return None;
    };
    let declared = external.language == "C"
        && external.function_name.as_deref() == Some(ENTRY_POINT)
        && external.output_name.is_none()
        && function.outputs.is_empty()
        && [string, file_name]
            .iter()
            .all(|input| input.dimensions().is_empty() && function_input_is_string(flat, input))
        && matches!(
            external.args.as_slice(),
            [Expression::VarRef { name: first, subscripts: first_subscripts, .. },
             Expression::VarRef { name: second, subscripts: second_subscripts, .. }]
                if first.as_str() == string.name
                    && second.as_str() == file_name.name
                    && first_subscripts.is_empty()
                    && second_subscripts.is_empty()
        );
    if !declared {
        return None;
    }
    let file_name_value = match arguments {
        [_, file_name] => file_name,
        [_] => file_name.default.as_ref()?,
        _ => return None,
    };
    matches!(
        file_name_value,
        Expression::Literal { value: Literal::String(name), .. } if name.is_empty()
    )
    .then(|| &arguments[0])
}

fn function_input_is_string(flat: &flat::Model, input: &rumoca_core::FunctionParam) -> bool {
    input.effective_type.canonical_type() == flat.predefined_types.string
}
