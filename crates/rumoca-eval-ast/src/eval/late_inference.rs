use super::*;

/// Check that all if-branches have dimensions consistent with expected dims (with scope).
pub(crate) fn all_branches_consistent_with_scope(
    branches: &[(Expression, Expression)],
    expected: &[usize],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> bool {
    for (_, then_expr) in branches {
        if let Some(branch_dims) = infer_dimensions_from_binding_with_scope(then_expr, ctx, scope)
            && branch_dims != expected
        {
            return false;
        }
    }
    true
}

/// Get binding expression or start value for a component.
fn get_binding_or_start(comp: &rumoca_ir_ast::Component) -> Option<&Expression> {
    comp.binding
        .as_ref()
        .or_else(|| (!matches!(comp.start, Expression::Empty { .. })).then_some(&comp.start))
}

/// Check if shape_expr contains colon dimensions.
fn has_colon_dimensions(comp: &rumoca_ir_ast::Component) -> bool {
    comp.shape_expr
        .iter()
        .any(|s| matches!(s, Subscript::Range { .. }))
}

/// Try to evaluate a single component's constants, returns true if progress was made.
fn eval_component_constants(
    full_name: &str,
    comp: &rumoca_ir_ast::Component,
    ctx: &mut TypeCheckEvalContext,
) -> bool {
    let mut progress = false;
    let type_name = comp.type_name.to_string();

    // Try integer parameters
    if type_name == "Integer"
        && !ctx.integers.contains_key(full_name)
        && let Some(val) = get_binding_or_start(comp).and_then(|e| eval_integer(e, ctx))
    {
        ctx.add_integer(full_name, val);
        progress = true;
    }

    // Try boolean parameters
    if type_name == "Boolean"
        && !ctx.booleans.contains_key(full_name)
        && let Some(val) = get_binding_or_start(comp).and_then(|e| eval_boolean(e, ctx))
    {
        ctx.booleans.insert(full_name.to_string(), val);
        progress = true;
    }

    // Try real parameters
    if type_name == "Real"
        && !ctx.reals.contains_key(full_name)
        && let Some(val) = get_binding_or_start(comp).and_then(|e| eval_real(e, ctx))
    {
        ctx.reals.insert(full_name.to_string(), val);
        progress = true;
    }

    // Try shape_expr dimensions
    let has_shape = !comp.shape_expr.is_empty() && !ctx.dimensions.contains_key(full_name);
    if has_shape
        && let Some(d) = comp
            .shape_expr
            .iter()
            .map(|sub| eval_dimension(sub, ctx))
            .collect()
    {
        ctx.add_dimensions(full_name, d);
        progress = true;
    }

    // Try colon dimension inference from binding
    let needs_inference = has_colon_dimensions(comp) && !ctx.dimensions.contains_key(full_name);
    if needs_inference
        && let Some(dims) = comp
            .binding
            .as_ref()
            .and_then(|b| infer_dimensions_from_binding(b, ctx))
    {
        ctx.add_dimensions(full_name, dims);
        progress = true;
    }

    progress
}

/// Collect all constant parameter values from a class for evaluation.
///
/// This performs a multi-pass evaluation to handle dependencies between parameters.
pub fn collect_constants(class: &ClassDef, prefix: &str) -> TypeCheckEvalContext {
    let mut ctx = TypeCheckEvalContext::new();

    const MAX_PASSES: usize = 10;
    for _pass in 0..MAX_PASSES {
        let mut progress = false;

        for (name, comp) in &class.components {
            let full_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            progress |= eval_component_constants(&full_name, comp, &mut ctx);
        }

        if !progress {
            break;
        }
    }

    ctx
}

/// Collect variable references from an expression.
///
/// Returns a set of variable names referenced in the expression.
/// Used to identify structural parameters (those used in dimension expressions).
pub fn collect_variable_refs(expr: &Expression) -> std::collections::HashSet<String> {
    rumoca_ir_ast::visitor::collect_component_refs(expr)
        .into_iter()
        .filter_map(|component_ref| {
            component_ref
                .parts
                .first()
                .map(|part| part.ident.text.to_string())
        })
        .collect()
}

/// Collect variable references from a subscript.
///
/// For explicit expressions like `x[n]`, returns the variables referenced.
/// For colon subscripts `x[:]`, returns empty (no variable references).
pub fn collect_subscript_refs(sub: &Subscript) -> std::collections::HashSet<String> {
    match sub {
        Subscript::Expression(expr) => collect_variable_refs(expr),
        // Colon (:) subscripts have no variable references - they mean "all indices"
        Subscript::Range { .. } => std::collections::HashSet::new(),
        Subscript::Empty => std::collections::HashSet::new(),
    }
}

/// Variability level for MLS §4.5 compliance.
///
/// Ordered from most restrictive to least restrictive:
/// Constant < Parameter < Discrete < Continuous
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VariabilityLevel {
    /// Constant - value known at compile time
    Constant = 0,
    /// Parameter - value fixed at initialization
    Parameter = 1,
    /// Discrete - value changes only at events
    Discrete = 2,
    /// Continuous - value can change continuously (default)
    Continuous = 3,
}

impl VariabilityLevel {
    /// Convert from AST Variability to VariabilityLevel.
    pub fn from_variability(v: &rumoca_core::Variability) -> Self {
        match v {
            rumoca_core::Variability::Constant(_) => Self::Constant,
            rumoca_core::Variability::Parameter(_) => Self::Parameter,
            rumoca_core::Variability::Discrete(_) => Self::Discrete,
            rumoca_core::Variability::Continuous(_) => Self::Continuous,
            rumoca_core::Variability::Empty => Self::Continuous,
        }
    }

    /// Get a human-readable name for this variability level.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Constant => "constant",
            Self::Parameter => "parameter",
            Self::Discrete => "discrete",
            Self::Continuous => "continuous",
        }
    }
}

/// Compute the maximum variability level of variables referenced in an expression.
///
/// Returns the highest variability among all referenced variables.
/// If no variables are referenced (pure literal), returns Constant.
pub fn max_variability_in_expr(
    expr: &Expression,
    class: &rumoca_ir_ast::ClassDef,
) -> VariabilityLevel {
    let refs = collect_variable_refs(expr);
    if refs.is_empty() {
        // Pure literal expression - constant variability
        return VariabilityLevel::Constant;
    }

    refs.iter()
        .filter_map(|name| class.components.get(name))
        .map(|comp| VariabilityLevel::from_variability(&comp.variability))
        .max()
        .unwrap_or(VariabilityLevel::Continuous)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{OpBinary, Token};
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference};
    use std::sync::Arc;

    fn make_token(text: &str) -> Token {
        Token {
            text: Arc::from(text),
            location: Default::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_int_literal(n: i64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: make_token(&n.to_string()),

            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_real_literal(x: f64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token: make_token(&x.to_string()),

            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_comp_ref(name: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: make_token(name),
                subs: None,
            }],
            def_id: None,

            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_dotted_comp_ref(path: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: crate::path_utils::split_path_with_indices(path)
                .into_iter()
                .map(|part| ComponentRefPart {
                    ident: make_token(part),
                    subs: None,
                })
                .collect(),
            def_id: None,

            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_comp_ref_with_sub(name: &str, idx: i64) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: make_token(name),
                subs: Some(vec![Subscript::Expression(make_int_literal(idx))]),
            }],
            def_id: None,

            span: rumoca_core::Span::DUMMY,
        })
    }

    fn make_call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::FunctionCall {
            comp: ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                }],
                def_id: None,

                span: rumoca_core::Span::DUMMY,
            },
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_eval_integer_literal() {
        let ctx = TypeCheckEvalContext::new();
        let expr = make_int_literal(42);
        assert_eq!(eval_integer(&expr, &ctx), Some(42));
    }

    #[test]
    fn test_eval_integer_variable() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("n", 10);
        let expr = make_comp_ref("n");
        assert_eq!(eval_integer(&expr, &ctx), Some(10));
    }

    #[test]
    fn test_lookup_with_scope_uses_structured_scope() {
        let mut map = FxHashMap::default();
        map.insert("sys.Medium.nX".to_string(), 4_i64);
        assert_eq!(lookup_with_scope("Medium.nX", "sys", &map), Some(&4_i64));
    }

    #[test]
    fn test_lookup_with_scope_dotted_name_does_not_leaf_fallback() {
        let mut map = FxHashMap::default();
        map.insert("sys.nX".to_string(), 7_i64);
        assert_eq!(lookup_with_scope("Medium.nX", "", &map), None);
    }

    #[test]
    fn test_lookup_with_scope_dotted_name_requires_matching_scoped_name() {
        let mut map = FxHashMap::default();
        map.insert("a.nX".to_string(), 7_i64);
        map.insert("b.nX".to_string(), 7_i64);
        assert_eq!(lookup_with_scope("Medium.nX", "", &map), None);
    }

    #[test]
    fn test_lookup_with_scope_dotted_name_ignores_ambiguous_suffixes() {
        let mut map = FxHashMap::default();
        map.insert("a.Medium.nX".to_string(), 1_i64);
        map.insert("b.Medium.nX".to_string(), 2_i64);
        assert_eq!(lookup_with_scope("Medium.nX", "", &map), None);
    }

    #[test]
    fn test_lookup_with_scope_simple_name_does_not_suffix_fallback() {
        let mut map = FxHashMap::default();
        map.insert("sys.nX".to_string(), 7_i64);
        assert_eq!(lookup_with_scope("nX", "", &map), None);
    }

    #[test]
    fn test_lookup_with_scope_resolves_indexed_name_with_scope() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("sys.arr[data.medium]", 7_i64);

        assert_eq!(
            lookup_with_scope("arr[data.medium]", "sys", &ctx.integers),
            Some(&7_i64)
        );
    }

    #[test]
    fn test_lookup_with_scope_does_not_index_fake_suffix_from_subscript_dot() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("sys.arr[data.medium]", 7_i64);

        assert_eq!(lookup_with_scope("medium]", "", &ctx.integers), None);
    }

    #[test]
    fn test_lookup_with_scope_does_not_scan_suffixes() {
        let mut map = FxHashMap::default();
        map.insert("sys.arr[data.medium].x".to_string(), 7_i64);
        map.insert("other.scope.x".to_string(), 9_i64);

        assert_eq!(lookup_with_scope("medium].x", "", &map), None);
    }

    #[test]
    fn test_lookup_with_scope_no_cross_map_suffix_fallback() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("a.nX", 7_i64);
        ctx.add_real("b.nX", 3.0);

        assert_eq!(lookup_with_scope("Medium.nX", "", &ctx.integers), None);
    }

    #[test]
    fn test_infer_dims_component_ref_dotted_does_not_leaf_fallback() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("sys.arr", vec![7]);

        let expr = make_dotted_comp_ref("Medium.arr");
        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, "battery1"),
            None
        );
    }

    #[test]
    fn test_infer_dims_field_access_uses_full_path_lookup() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("cellData1.OCV_SOC", vec![2, 2]);
        ctx.add_dimensions("cellData2.OCV_SOC", vec![17, 2]);
        let expr = Expression::FieldAccess {
            base: Arc::new(make_comp_ref("cellData1")),
            field: "OCV_SOC".to_string(),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, "battery1.cellData"),
            Some(vec![2, 2])
        );
    }

    #[test]
    fn test_infer_dims_field_access_does_not_leaf_fallback_for_dotted_path() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("scope.OCV_SOC_internal", vec![17, 2]);

        let expr = Expression::FieldAccess {
            base: Arc::new(make_comp_ref("cellData1")),
            field: "OCV_SOC_internal".to_string(),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, "battery1.cellData"),
            None
        );
    }

    #[test]
    fn test_infer_dims_field_access_dotted_base_uses_full_path_lookup() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("cellData1.OCV_SOC_internal", vec![2, 2]);
        ctx.add_dimensions("cellData2.OCV_SOC_internal", vec![17, 2]);
        let expr = Expression::FieldAccess {
            base: Arc::new(make_dotted_comp_ref("cellData1")),
            field: "OCV_SOC_internal".to_string(),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, "battery1.cellData"),
            Some(vec![2, 2])
        );
    }

    #[test]
    fn test_infer_dims_field_access_with_indexed_base_uses_exact_lookup() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("stackData.cellData[1,1].OCV_SOC", vec![29, 2]);

        let expr = Expression::FieldAccess {
            base: Arc::new(Expression::ArrayIndex {
                base: Arc::new(Expression::FieldAccess {
                    base: Arc::new(make_comp_ref("stackData")),
                    field: "cellData".to_string(),

                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![
                    Subscript::Expression(make_int_literal(1)),
                    Subscript::Expression(make_int_literal(1)),
                ],

                span: rumoca_core::Span::DUMMY,
            }),
            field: "OCV_SOC".to_string(),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, ""),
            Some(vec![29, 2])
        );
    }

    #[test]
    fn test_infer_dims_field_access_with_indexed_base_respects_scope() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("stack.stackData.cellData[1,1].OCV_SOC", vec![29, 2]);

        let expr = Expression::FieldAccess {
            base: Arc::new(Expression::ArrayIndex {
                base: Arc::new(Expression::FieldAccess {
                    base: Arc::new(make_comp_ref("stackData")),
                    field: "cellData".to_string(),

                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![
                    Subscript::Expression(make_int_literal(1)),
                    Subscript::Expression(make_int_literal(1)),
                ],

                span: rumoca_core::Span::DUMMY,
            }),
            field: "OCV_SOC".to_string(),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, "stack"),
            Some(vec![29, 2])
        );
    }

    #[test]
    fn test_eval_binary_add() {
        let ctx = TypeCheckEvalContext::new();
        let expr = Expression::Binary {
            op: OpBinary::Add,
            lhs: Arc::new(make_int_literal(3)),
            rhs: Arc::new(make_int_literal(4)),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(eval_integer(&expr, &ctx), Some(7));
    }

    #[test]
    fn test_eval_size() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("arr", vec![10, 20]);

        let expr = Expression::FunctionCall {
            comp: ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: make_token("size"),
                    subs: None,
                }],
                def_id: None,

                span: rumoca_core::Span::DUMMY,
            },
            args: vec![make_comp_ref("arr"), make_int_literal(1)],
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(eval_integer(&expr, &ctx), Some(10));
    }

    #[test]
    fn test_infer_dims_single_row_matrix_literal() {
        let ctx = TypeCheckEvalContext::new();
        let expr = Expression::Array {
            elements: vec![
                Expression::Empty {
                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Empty {
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_dimensions_from_binding(&expr, &ctx), Some(vec![1, 2]));
    }

    #[test]
    fn test_infer_dims_multi_row_matrix_literal() {
        let ctx = TypeCheckEvalContext::new();
        let expr = Expression::Array {
            elements: vec![
                Expression::Array {
                    elements: vec![
                        Expression::Empty {
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Empty {
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: true,

                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Array {
                    elements: vec![
                        Expression::Empty {
                            span: rumoca_core::Span::DUMMY,
                        },
                        Expression::Empty {
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: true,

                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_dimensions_from_binding(&expr, &ctx), Some(vec![2, 2]));
    }

    #[test]
    fn test_infer_dims_vector_of_vertical_matrix_concat_uses_operand_shapes() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_integer("na", 3);
        ctx.add_integer("nb", 2);
        ctx.add_dimensions("b", vec![2]);

        let zero_rows = make_call(
            "zeros",
            vec![
                make_call(
                    "max",
                    vec![
                        make_int_literal(0),
                        Expression::Binary {
                            op: OpBinary::Sub,
                            lhs: Arc::new(make_comp_ref("na")),
                            rhs: Arc::new(make_comp_ref("nb")),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                ),
                make_int_literal(1),
            ],
        );
        let matrix = Expression::Array {
            elements: vec![
                Expression::Array {
                    elements: vec![zero_rows],
                    is_matrix: true,

                    span: rumoca_core::Span::DUMMY,
                },
                Expression::Array {
                    elements: vec![make_comp_ref("b")],
                    is_matrix: true,

                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: true,
            span: rumoca_core::Span::DUMMY,
        };
        let expr = make_call("vector", vec![matrix]);

        assert_eq!(
            infer_dimensions_from_binding(&expr, &ctx),
            Some(vec![3]),
            "MLS §10.4.2 matrix constructors concatenate array operands before vector() flattens them"
        );

        let mut scoped_ctx = TypeCheckEvalContext::new();
        scoped_ctx.add_integer("Hw.na", 3);
        scoped_ctx.add_integer("Hw.nb", 2);
        scoped_ctx.add_dimensions("Hw.b", vec![2]);
        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &scoped_ctx, "Hw"),
            Some(vec![3]),
            "MLS §5.1 scoped lookup applies while inferring matrix-constructor operand shapes"
        );
    }

    #[test]
    fn test_infer_dims_component_ref_scalar_subscript_is_scalar() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("eta", vec![3]);
        let expr = make_comp_ref_with_sub("eta", 1);
        assert_eq!(
            infer_dimensions_from_binding(&expr, &ctx),
            Some(vec![]),
            "eta[1] should be scalar, not vector-shaped"
        );
    }

    #[test]
    fn test_infer_dims_component_ref_with_indexed_prefix_uses_exact_lookup() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("stackData.cellData[1,1].OCV_SOC", vec![29, 2]);

        let expr = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![
                ComponentRefPart {
                    ident: make_token("stackData"),
                    subs: None,
                },
                ComponentRefPart {
                    ident: make_token("cellData"),
                    subs: Some(vec![
                        Subscript::Expression(make_int_literal(1)),
                        Subscript::Expression(make_int_literal(1)),
                    ]),
                },
                ComponentRefPart {
                    ident: make_token("OCV_SOC"),
                    subs: None,
                },
            ],
            def_id: None,

            span: rumoca_core::Span::DUMMY,
        });

        assert_eq!(
            infer_dimensions_from_binding_with_scope(&expr, &ctx, ""),
            Some(vec![29, 2])
        );
    }

    #[test]
    fn test_eval_size_with_indexed_component_ref_uses_exact_lookup() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("stackData.cellData[1,1].OCV_SOC", vec![29, 2]);

        let indexed_ref = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![
                ComponentRefPart {
                    ident: make_token("stackData"),
                    subs: None,
                },
                ComponentRefPart {
                    ident: make_token("cellData"),
                    subs: Some(vec![
                        Subscript::Expression(make_int_literal(1)),
                        Subscript::Expression(make_int_literal(1)),
                    ]),
                },
                ComponentRefPart {
                    ident: make_token("OCV_SOC"),
                    subs: None,
                },
            ],
            def_id: None,

            span: rumoca_core::Span::DUMMY,
        });

        let size_expr = Expression::FunctionCall {
            comp: ComponentReference {
                local: false,
                parts: vec![ComponentRefPart {
                    ident: make_token("size"),
                    subs: None,
                }],
                def_id: None,

                span: rumoca_core::Span::DUMMY,
            },
            args: vec![indexed_ref, make_int_literal(1)],
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(eval_integer(&size_expr, &ctx), Some(29));
    }

    #[test]
    fn test_infer_dims_array_literal_with_indexed_elements_stays_1d() {
        let mut ctx = TypeCheckEvalContext::new();
        ctx.add_dimensions("eta", vec![3]);
        let expr = Expression::Array {
            elements: vec![
                make_comp_ref_with_sub("eta", 1),
                make_comp_ref_with_sub("eta", 2),
                make_comp_ref_with_sub("eta", 3),
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(
            infer_dimensions_from_binding(&expr, &ctx),
            Some(vec![3]),
            "array literal of scalar indexed elements should infer as [3], not [3,3]"
        );
    }

    #[test]
    fn test_infer_dims_real_range_binding() {
        let ctx = TypeCheckEvalContext::new();
        let start = make_real_literal(0.0);
        let step = make_real_literal(0.02);
        let end = make_real_literal(1.0);
        assert_eq!(eval_real(&start, &ctx), Some(0.0));
        assert_eq!(eval_real(&step, &ctx), Some(0.02));
        assert_eq!(eval_real(&end, &ctx), Some(1.0));

        let expr = Expression::Range {
            start: Arc::new(start),
            step: Some(Arc::new(step)),
            end: Arc::new(end),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_dimensions_from_binding(&expr, &ctx), Some(vec![51]));
    }
}
