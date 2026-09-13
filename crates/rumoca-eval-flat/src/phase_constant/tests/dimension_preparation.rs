use super::*;
use crate::constant::{EvalContext, EvalEnvironment};

#[test]
fn borrowed_values_match_owned_values_across_scopes_and_unknowns() {
    let integers = FxHashMap::from_iter([
        ("n".into(), 99),
        ("left.n".into(), 2),
        ("right.n".into(), 5),
    ]);
    let reals = FxHashMap::from_iter([("left.Config.gain".into(), 0.5)]);
    let booleans = FxHashMap::from_iter([("left.enabled".into(), true)]);
    let enums = FxHashMap::from_iter([("left.mode".into(), "Pkg.Mode.On".into())]);
    let dimensions = FxHashMap::from_iter([
        ("left.a".into(), vec![2, 3]),
        ("right.a".into(), vec![5, 4]),
    ]);
    let mut function = rumoca_core::Function::new("Pkg.bump", test_span());
    function.add_input(function_param("n", TestScalar::Integer, vec![]));
    function.add_output(function_param("y", TestScalar::Integer, vec![]));
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: comp_ref("y"),
        value: binary(rumoca_core::OpBinary::Add, var("n"), int(1)),
        span: test_span(),
    }];
    let functions = FxHashMap::from_iter([("Pkg.bump".into(), function)]);
    for scope in [None, Some("left.result"), Some("right.result")] {
        let parameters = ParamEvalContext::new(
            &integers,
            &reals,
            &booleans,
            &enums,
            &dimensions,
            &functions,
            scope,
        );
        let owned = build_param_value_context(&parameters, &EnumCanonicalizer::new(&enums));
        let mut borrowed = BorrowedContext::new(&parameters, &EnumCanonicalizer::new(&enums));
        borrowed.literals.set_lookup_scope(
            scope
                .map(ComponentPath::from_flat_path)
                .and_then(|path| path.parent()),
        );
        for name in ["Pkg.bump", "bump"] {
            assert!(
                std::ptr::eq(borrowed.get_function(name).unwrap(), &functions["Pkg.bump"],),
                "function aliases must borrow the original definition"
            );
        }
        assert!(
            std::ptr::eq(
                borrowed.get_array_dimensions("left.a").unwrap().as_ptr(),
                dimensions["left.a"].as_ptr(),
            ),
            "shape queries must borrow the original dimension vector"
        );
        let expressions = [
            var("n"),
            var("enabled"),
            var("mode"),
            var("unknown.value"),
            call(rumoca_core::BuiltinFunction::Size, vec![var("a"), int(2)]),
            call(rumoca_core::BuiltinFunction::Ndims, vec![var("a")]),
            function_call("Pkg.bump", vec![var("n")]),
            function_call("bump", vec![var("n")]),
            binary(rumoca_core::OpBinary::Eq, var("mode"), var("Pkg.Mode.On")),
            binary(rumoca_core::OpBinary::Eq, var("Config.gain"), real(0.5)),
        ];
        for expression in expressions {
            assert_eq!(
                crate::constant::eval_expr(&expression, &borrowed).ok(),
                crate::constant::eval_expr(&expression, &owned).ok(),
                "scope {scope:?}, expression {expression:?}",
            );
        }
        let mut evaluator = ParamEvaluator::new(&parameters);
        assert_eq!(
            evaluator.eval_integer(&var("n"), Some("left.result")),
            Some(2)
        );
        assert_eq!(
            evaluator.eval_integer(&var("n"), Some("right.result")),
            Some(5)
        );
        assert_eq!(evaluator.eval_integer(&var("n"), None), Some(99));
    }
}

fn assert_no_inventory_copy(expression: &rumoca_core::Expression, expected: Vec<i64>) {
    let integers = (0..128).map(|n| (format!("p{n}"), n)).collect();
    let before = EvalContext::parameter_insertions();
    let dimensions = infer_array_dimensions_full_with_conds(
        expression,
        &integers,
        &FxHashMap::default(),
        &FxHashMap::default(),
        &FxHashMap::default(),
    );
    assert_eq!(dimensions, Some(expected));
    assert_eq!(
        EvalContext::parameter_insertions() - before,
        0,
        "one immutable shape walk must borrow its parameter inventory",
    );
}

#[test]
fn matrix_shape_walk_borrows_parameter_inventory() {
    let matrix = rumoca_core::Expression::Array {
        elements: (0..32).map(|n| var(&format!("p{n}"))).collect(),
        kind: rumoca_core::ArrayConstructor::Horizontal,
        span: test_span(),
    };
    assert_no_inventory_copy(&matrix, vec![1, 32]);
}

#[test]
fn comprehension_shape_walks_share_only_the_immutable_parameter_inventory() {
    let comprehension = |index: &str| rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(binary(rumoca_core::OpBinary::Add, var("p5"), var(index))),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: index.to_owned(),
            range: rumoca_core::Expression::Range {
                start: Box::new(int(1)),
                step: None,
                end: Box::new(int(3)),
                span: test_span(),
            },
        }],
        filter: None,
        span: test_span(),
    };
    let array = rumoca_core::Expression::Array {
        elements: vec![comprehension("p6"), comprehension("p7")],
        kind: rumoca_core::ArrayConstructor::Array,
        span: test_span(),
    };
    assert_no_inventory_copy(&array, vec![2, 3]);
}

#[test]
fn prepared_dimension_values_preserve_scope_and_index_shadowing() {
    let integers = FxHashMap::from_iter([("left.n".into(), 2), ("right.n".into(), 5)]);
    let reals = FxHashMap::default();
    let booleans = FxHashMap::default();
    let enums = FxHashMap::default();
    let dimensions = FxHashMap::default();
    let functions = FxHashMap::default();
    for (name, expected) in [("left.result", 2), ("right.result", 5)] {
        let context = ParamEvalContext::new(
            &integers,
            &reals,
            &booleans,
            &enums,
            &dimensions,
            &functions,
            Some(name),
        );
        let scope = DimensionScope::new(&context);
        assert_eq!(scope.integer(&var("n")), Some(expected));
        let mut nested = scope.clone();
        nested.bind_index("n");
        assert_eq!(nested.integer(&var("n")), None);
        assert_eq!(scope.integer(&var("n")), Some(expected));
    }
}

#[test]
fn independent_shape_queries_do_not_materialize_the_parameter_inventory() {
    let integers = (0..128).map(|n| (format!("p{n}"), n)).collect();
    let empty = FxHashMap::default();
    let booleans = FxHashMap::default();
    let dimensions = FxHashMap::default();
    let before = EvalContext::parameter_insertions();
    for _ in 0..8 {
        let expression = rumoca_core::Expression::Array {
            elements: vec![var("p2"), var("p5")],
            kind: rumoca_core::ArrayConstructor::Horizontal,
            span: test_span(),
        };
        assert_eq!(
            infer_array_dimensions_full_with_conds(
                &expression,
                &integers,
                &booleans,
                &empty,
                &dimensions,
            ),
            Some(vec![1, 2]),
        );
    }
    assert_eq!(
        EvalContext::parameter_insertions() - before,
        0,
        "read-only queries must borrow the inventory instead of copying all unrelated bindings",
    );
}

/// Build an EvalContext from known parameter values and functions.
fn build_eval_context(
    known_ints: &FxHashMap<String, i64>,
    known_reals: &FxHashMap<String, f64>,
    known_bools: &FxHashMap<String, bool>,
    array_dims: &FxHashMap<String, Vec<i64>>,
    functions: &FxHashMap<String, rumoca_core::Function>,
) -> EvalContext {
    let parameter_capacity =
        known_ints.len() + known_reals.len() + known_bools.len() + array_dims.len();
    let mut eval_ctx = EvalContext::with_capacity(parameter_capacity, 0, functions.len() * 2);
    for (k, v) in known_ints {
        eval_ctx.add_parameter(k.clone(), Value::Integer(*v));
    }
    for (k, v) in known_reals {
        eval_ctx.add_parameter(k.clone(), Value::Real(*v));
    }
    for (k, v) in known_bools {
        eval_ctx.add_parameter(k.clone(), Value::Bool(*v));
    }
    for (k, v) in array_dims {
        eval_ctx.add_array_dimensions(k.clone(), v.clone());
    }
    for func in functions.values() {
        eval_ctx.add_function(func.clone());
    }
    eval_ctx
}

fn build_param_value_context(
    ctx: &ParamEvalContext<'_>,
    enum_canonicalizer: &EnumCanonicalizer,
) -> EvalContext {
    let mut eval_ctx = build_eval_context(
        ctx.known_ints,
        ctx.known_reals,
        ctx.known_bools,
        ctx.array_dims,
        ctx.functions,
    );
    for (name, literal) in ctx.known_enums {
        // An empty rendered name carries no enumeration identity and cannot key
        // a lookup, so there is nothing to register for it.
        let Some(identity) = enum_canonicalizer.canonicalize(literal) else {
            continue;
        };
        let value = identity.to_value();
        eval_ctx.add_parameter(name.clone(), value.clone());
        eval_ctx.add_parameter(identity.to_flat_string(), value);
    }
    eval_ctx.set_lookup_scope(
        ctx.var_context
            .map(ComponentPath::from_flat_path)
            .and_then(|path| path.parent()),
    );
    eval_ctx
}
