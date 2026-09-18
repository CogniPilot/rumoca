//! Guard classification for MLS §3.6.5 conditional folding in attribute and
//! binding values.

use super::*;

/// Whether `condition` reads a tunable parameter coordinate's value.
///
/// A Modelica `constant` and a structural quantity (`size`/`ndims`, an
/// enumeration extent) fold to a literal and own no runtime coordinate, so only
/// a tunable `parameter` appears here as [`Coordinate::Parameter`]. A guard over
/// one's value must not be folded in an attribute or binding value, where the
/// parameter can change after translation.
///
/// A parameter that appears only as the array operand of `size`/`ndims`
/// contributes its shape, not its value, and a parameter's shape is fixed at
/// translation. Such a guard (`size(offset, 1) == 1`) is structural: it proves
/// the same Boolean however the parameter is later recalibrated, so it still
/// folds. The array operand of `size`/`ndims` is therefore not scanned; a
/// dimension index or any other position that reads the value still is.
pub(super) fn guard_reads_tunable_parameter<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    condition: &Expression,
) -> bool {
    struct GuardScan<'a, 'dae> {
        coordinates: &'a HashMap<VarName, Coordinate<'dae>>,
        reads_parameter: bool,
    }

    impl rumoca_core::ExpressionVisitor for GuardScan<'_, '_> {
        fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
            if matches!(
                self.coordinates.get(name.var_name()),
                Some(Coordinate::Parameter(_))
            ) {
                self.reads_parameter = true;
            }
            self.walk_var_ref(name, subscripts);
        }

        fn visit_builtin_call(
            &mut self,
            function: &rumoca_core::BuiltinFunction,
            args: &[Expression],
        ) {
            // `size(A, ...)` and `ndims(A)` read `A`'s shape, never its value, so
            // the array operand `A` (the first argument) is skipped. A remaining
            // argument (a `size` dimension index) is scanned like any other read.
            let structural = matches!(
                function,
                rumoca_core::BuiltinFunction::Size | rumoca_core::BuiltinFunction::Ndims
            );
            for argument in args.iter().skip(usize::from(structural)) {
                self.visit_expression(argument);
            }
        }
    }

    let mut scan = GuardScan {
        coordinates,
        reads_parameter: false,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut scan, condition);
    scan.reads_parameter
}

/// Whether any guard or value of a conditional calls a user function.
///
/// Shape discovery prunes the calls of a statically dead arm, so a call that
/// discovery removed carries no call-shape certificate. Preserving such an arm
/// would try to build that uncertified call, so a conditional with a user-
/// function call in any arm keeps folding rather than being preserved.
pub(super) fn conditional_calls_a_user_function(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
) -> bool {
    struct CallScan {
        calls_user_function: bool,
    }

    impl rumoca_core::ExpressionVisitor for CallScan {
        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[Expression],
            is_constructor: bool,
        ) {
            self.calls_user_function = true;
            self.walk_function_call(name, args, is_constructor);
        }
    }

    let mut scan = CallScan {
        calls_user_function: false,
    };
    for (condition, value) in branches {
        rumoca_core::ExpressionVisitor::visit_expression(&mut scan, condition);
        rumoca_core::ExpressionVisitor::visit_expression(&mut scan, value);
    }
    rumoca_core::ExpressionVisitor::visit_expression(&mut scan, else_branch);
    scan.calls_user_function
}
