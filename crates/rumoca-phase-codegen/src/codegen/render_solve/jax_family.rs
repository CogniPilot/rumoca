//! Compact branch-free JAX family emission; target integration is separate.
use super::template_partition::SolveNativeFamilyValue;
use super::*;
use std::collections::HashSet;

pub(in crate::codegen) fn render_solve_native_family_jax_function(
    family: Value,
    prefix: String,
    y: String,
) -> RenderResult {
    let Some(family) = family.downcast_object_ref::<SolveNativeFamilyValue>() else {
        return Err(render_err(
            "JAX emitter requires a typed native Solve family",
        ));
    };
    emit(family.family(), &prefix, &y)
}

fn identifier(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .enumerate()
            .all(|(i, b)| b == b'_' || b.is_ascii_alphabetic() || (i > 0 && b.is_ascii_digit()))
}

fn affine_index(
    base: usize,
    terms: &[solve::AffineStencilIndexStrideTerm],
    extents: &[usize],
    prefix: &str,
) -> RenderResult {
    let mut bound = base as u128;
    let mut negative_width = 0u128;
    let mut expression = base.to_string();
    for term in terms {
        let extent = *extents
            .get(term.dimension)
            .ok_or_else(|| render_err("JAX index dimension"))?;
        let width = (term.stride.unsigned_abs() as u128)
            .checked_mul(extent.saturating_sub(1) as u128)
            .ok_or_else(|| render_err("JAX index range overflow"))?;
        if term.stride < 0 {
            negative_width = negative_width
                .checked_add(width)
                .ok_or_else(|| render_err("JAX negative index range overflow"))?;
        }
        bound = bound
            .checked_add(width)
            .ok_or_else(|| render_err("JAX index range overflow"))?;
        expression = format!(
            "({expression} + {prefix}_c{} * ({}))",
            term.dimension, term.stride
        );
    }
    // JAX gathers accept negative indices by wrapping from the end. Solve
    // storage indices do not have that meaning, so reject this compact path
    // before an invalid affine gather can silently read a different cell.
    if negative_width > base as u128 {
        return Err(render_err("JAX affine index can become negative"));
    }
    if bound > i64::MAX as u128 {
        return Err(render_err("JAX index exceeds signed64 domain"));
    }
    Ok(expression)
}

fn register(prefix: &str, index: solve::Reg, defined: &HashSet<solve::Reg>) -> RenderResult {
    if !defined.contains(&index) {
        return Err(render_err("JAX family reads undefined register"));
    }
    Ok(format!("{prefix}_r{index}"))
}

pub(super) fn supports(family: &RenderNativeAffineFamily) -> bool {
    emit(family, "jax_probe", "x").is_ok()
}

// Sufficient mixed-radix separation proof: after sorting dimensions by
// absolute stride, each new axis must move beyond the entire lower-axis span.
// Covers ordinary dense/strided/reversed rectangular interiors without
// enumerating cells. Unproved maps retain checked scalar fallback.
fn output_strides(
    family: &RenderNativeAffineFamily,
    rank: usize,
) -> Result<Vec<i128>, minijinja::Error> {
    let mut strides = vec![0i128; rank];
    for term in &family.output_map.strides {
        let entry = strides
            .get_mut(term.dimension)
            .ok_or_else(|| render_err("JAX output dimension"))?;
        *entry = entry
            .checked_add(term.stride as i128)
            .ok_or_else(|| render_err("JAX output stride overflow"))?;
    }
    Ok(strides)
}

fn validate_injective_output(
    family: &RenderNativeAffineFamily,
    extents: &[usize],
) -> Result<(), minijinja::Error> {
    let strides = output_strides(family, extents.len())?;
    let mut axes: Vec<_> = strides
        .into_iter()
        .zip(extents.iter().copied())
        .filter(|(_, extent)| *extent > 1)
        .map(|(stride, extent)| (stride.unsigned_abs(), extent))
        .collect();
    axes.sort_unstable();
    let mut span = 0u128;
    for (stride, extent) in axes {
        if stride <= span {
            return Err(render_err(
                "JAX output injectivity unproved; use scalar fallback",
            ));
        }
        span = span
            .checked_add(
                stride
                    .checked_mul((extent - 1) as u128)
                    .ok_or_else(|| render_err("JAX output span overflow"))?,
            )
            .ok_or_else(|| render_err("JAX output span overflow"))?;
    }
    Ok(())
}

pub(super) fn coverage_ranges(
    family: &RenderNativeAffineFamily,
) -> Result<Vec<(usize, usize)>, minijinja::Error> {
    let extents = family
        .domain
        .extents()
        .map_err(|e| render_err(e.to_string()))?;
    if family.count == 0 {
        return Ok(Vec::new());
    }
    validate_injective_output(family, &extents)?;
    let strides = output_strides(family, extents.len())?;
    let mut minimum = family.output_map.start as i128;
    let mut axes = Vec::new();
    for (stride, extent) in strides.into_iter().zip(extents) {
        if extent <= 1 {
            continue;
        }
        if stride < 0 {
            minimum = minimum
                .checked_add(
                    stride
                        .checked_mul((extent - 1) as i128)
                        .ok_or_else(|| render_err("JAX coverage offset overflow"))?,
                )
                .ok_or_else(|| render_err("JAX coverage minimum overflow"))?;
        }
        let stride = usize::try_from(stride.unsigned_abs())
            .map_err(|_| render_err("JAX coverage stride overflow"))?;
        axes.push((stride, extent));
    }
    axes.sort_unstable();
    let mut width = 1usize;
    let mut outer = Vec::new();
    for (stride, extent) in axes {
        if outer.is_empty() && stride == width {
            width = width
                .checked_mul(extent)
                .ok_or_else(|| render_err("JAX coverage width overflow"))?;
        } else {
            outer.push((stride, extent));
        }
    }
    let minimum =
        usize::try_from(minimum).map_err(|_| render_err("JAX negative coverage index"))?;
    let end = minimum
        .checked_add(width)
        .ok_or_else(|| render_err("JAX coverage end overflow"))?;
    let mut ranges = vec![(minimum, end)];
    for (stride, extent) in outer {
        let count = ranges
            .len()
            .checked_mul(extent)
            .ok_or_else(|| render_err("JAX coverage range count overflow"))?;
        let mut expanded = render_vec_with_capacity(count, "JAX coverage range count")?;
        for position in 0..extent {
            let offset = stride
                .checked_mul(position)
                .ok_or_else(|| render_err("JAX coverage offset overflow"))?;
            for &(start, end) in &ranges {
                expanded.push((
                    start
                        .checked_add(offset)
                        .ok_or_else(|| render_err("JAX coverage start overflow"))?,
                    end.checked_add(offset)
                        .ok_or_else(|| render_err("JAX coverage end overflow"))?,
                ));
            }
        }
        ranges = expanded;
    }
    ranges.sort_unstable();
    Ok(ranges)
}

fn assignment(
    family: &RenderNativeAffineFamily,
    prefix: &str,
    y: &str,
    extents: &[usize],
    defined: &HashSet<solve::Reg>,
    position: usize,
    op: &solve::LinearOp,
) -> Result<(solve::Reg, String), minijinja::Error> {
    let read = |index| register(prefix, index, defined);
    match op {
        solve::LinearOp::Const { dst, value } => {
            if !value.is_finite() {
                return Err(render_err("JAX family requires finite constants"));
            }
            let mut value = format!("{value:?}");
            for term in family_const_terms(family, position)? {
                if !term.stride.is_finite() {
                    return Err(render_err("JAX finite constant stride"));
                }
                value = format!(
                    "({value} + {prefix}_c{} * ({:?}))",
                    term.dimension, term.stride
                );
            }
            Ok((*dst, value))
        }
        solve::LinearOp::LoadY { dst, index } | solve::LinearOp::LoadP { dst, index } => {
            let index = affine_index(
                *index,
                &family_load_terms(family, position)?,
                extents,
                prefix,
            )?;
            let source = if matches!(op, solve::LinearOp::LoadY { .. }) {
                y
            } else {
                "P"
            };
            Ok((*dst, format!("{source}[{index}]")))
        }
        solve::LinearOp::LoadTime { dst } => Ok((*dst, "t".to_owned())),
        solve::LinearOp::Move { dst, src } => Ok((*dst, read(*src)?)),
        solve::LinearOp::Unary { dst, op, arg } => {
            let arg = read(*arg)?;
            let value = match op {
                solve::UnaryOp::Neg => format!("(-{arg})"),
                solve::UnaryOp::Abs => format!("jnp.abs({arg})"),
                solve::UnaryOp::Sqrt => format!("jnp.sqrt({arg})"),
                _ => {
                    return Err(render_err(
                        "unsupported JAX compact unary; use checked scalar fallback",
                    ));
                }
            };
            Ok((*dst, value))
        }
        solve::LinearOp::Binary { dst, op, lhs, rhs } => {
            let symbol = match op {
                solve::BinaryOp::Add => "+",
                solve::BinaryOp::Sub => "-",
                solve::BinaryOp::Mul => "*",
                solve::BinaryOp::Div => "/",
                solve::BinaryOp::Pow => "**",
                _ => {
                    return Err(render_err(
                        "unsupported JAX compact binary; use checked scalar fallback",
                    ));
                }
            };
            Ok((*dst, format!("({} {symbol} {})", read(*lhs)?, read(*rhs)?)))
        }
        _ => Err(render_err(
            "unsupported JAX compact operation; preserve scalar/lazy fallback",
        )),
    }
}

fn emit(family: &RenderNativeAffineFamily, prefix: &str, y: &str) -> RenderResult {
    if !identifier(prefix) || !identifier(y) {
        return Err(render_err("JAX identifiers required"));
    }
    validate_native_family_metadata(family)?;
    let extents = family
        .domain
        .extents()
        .map_err(|e| render_err(e.to_string()))?;
    validate_injective_output(family, &extents)?;
    let strides = family
        .domain
        .ordinal_strides()
        .map_err(|e| render_err(e.to_string()))?;
    if family.count == 0 {
        return Ok(String::new());
    }
    if family.count > i64::MAX as usize {
        return Err(render_err("JAX family exceeds signed64 domain"));
    }
    let mut lines = vec![format!(
        "\t{prefix}_row = jnp.arange({}, dtype=jnp.int64)",
        family.count
    )];
    for (axis, (&extent, &stride)) in extents.iter().zip(&strides).enumerate() {
        if extent > i64::MAX as usize || stride > i64::MAX as usize {
            return Err(render_err("JAX coordinate exceeds signed64 domain"));
        }
        lines.push(format!(
            "\t{prefix}_c{axis} = ({prefix}_row // {stride}) % {extent}"
        ));
    }
    let mut defined = HashSet::new();
    let mut stored = false;
    for (position, op) in family.base_ops.iter().enumerate() {
        if let solve::LinearOp::StoreOutput { src } = op {
            if stored {
                return Err(render_err(
                    "JAX compact family requires one output per cell",
                ));
            }
            let index = affine_index(
                family.output_map.start,
                &family.output_map.strides,
                &extents,
                prefix,
            )?;
            // Keep scalar singleton maps and vector maps shape-aligned.
            // Constant/time-only bodies broadcast without per-cell operations.
            lines.push(format!(
                "\tout = out.at[jnp.broadcast_to({index}, ({count},))].set(jnp.broadcast_to({}, ({count},)))",
                register(prefix, *src, &defined)?,
                count = family.count
            ));
            stored = true;
        } else {
            let (dst, value) = assignment(family, prefix, y, &extents, &defined, position, op)?;
            lines.push(format!("\t{prefix}_r{dst} = {value}"));
            defined.insert(dst);
        }
    }
    if !stored {
        return Err(render_err("JAX family has no output"));
    }
    Ok(lines.join("\n"))
}

#[cfg(test)]
mod tests {
    use super::*;
    fn family(count: usize) -> RenderNativeAffineFamily {
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".to_owned(),
                lower: 1,
                upper: count as i64,
                step: 1,
            }],
        };
        RenderNativeAffineFamily {
            kind: "stencil",
            output_offset: 0,
            count,
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain,
            base_ops: vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 2.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 0,
                terms: vec![solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: vec![],
        }
    }
    #[test]
    fn compact_jax_singleton_scatter_keeps_index_and_update_vector_shapes() {
        let mut value = family(1);
        assert!(value.output_map.strides.is_empty());
        let text = emit(&value, "single", "x").unwrap();
        assert!(
            text.contains(
                "out.at[jnp.broadcast_to(0, (1,))].set(jnp.broadcast_to(single_r2, (1,)))"
            )
        );
        value.base_ops = vec![
            solve::LinearOp::Const { dst: 0, value: 2.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ];
        value.load_strides.clear();
        let constant = emit(&value, "constant", "x").unwrap();
        assert!(constant.contains(
            "out.at[jnp.broadcast_to(0, (1,))].set(jnp.broadcast_to(constant_r0, (1,)))"
        ));
    }

    #[test]
    fn compact_jax_emission_scales_with_kernel_not_cells() {
        let small = emit(&family(128), "kernel", "x").unwrap();
        let large = emit(&family(2048), "kernel", "x").unwrap();
        assert_eq!(small.lines().count(), large.lines().count());
        assert!(large.len() < 2 * small.len());
        assert!(large.contains("jnp.arange(2048"));
        assert!(large.contains("out.at["));
        assert!(large.contains("x[(0 + kernel_c0 * (1))]"));
    }
    #[test]
    fn compact_jax_two_dimensional_coordinates_preserve_reversed_gather() {
        let mut value = family(3);
        value
            .domain
            .binders
            .push(rumoca_core::StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 1,
                display_name: "j".to_owned(),
                lower: 1,
                upper: 4,
                step: 1,
            });
        value.count = 12;
        value.output_map = solve::TensorOutputMap::dense_contiguous(5, &value.domain).unwrap();
        value.base_ops[0] = solve::LinearOp::LoadY { dst: 0, index: 3 };
        value.load_strides[0].terms = vec![
            solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 4,
            },
            solve::AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: -1,
            },
        ];
        let text = emit(&value, "grid", "x").unwrap();
        assert!(text.contains("grid_c0 = (grid_row // 4) % 3"));
        assert!(text.contains("grid_c1 = (grid_row // 1) % 4"));
        assert!(text.contains("x[((3 + grid_c0 * (4)) + grid_c1 * (-1))]"));
        assert!(
            text.contains("out.at[jnp.broadcast_to(((5 + grid_c0 * (4)) + grid_c1 * (1)), (12,))]")
        );
        // An independent enumeration documents the intended gather permutation,
        // including both row boundaries; JAX execution is a separate gate.
        let gathered: Vec<_> = (0..12).map(|row| 3 + (row / 4) * 4 - row % 4).collect();
        assert_eq!(gathered, vec![3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8]);
    }

    #[test]
    fn accepted_output_stride_proofs_match_canonical_enumerated_uniqueness() {
        for case in 0..(3 * 3 * 49) {
            let outer = case / (3 * 49) + 1;
            let inner = (case / 49) % 3 + 1;
            let encoded = case % 49;
            let outer_stride = (encoded / 7) as isize - 3;
            let inner_stride = (encoded % 7) as isize - 3;
            let mut value = family(outer);
            let mut binder = value.domain.binders[0].clone();
            binder.id = 1;
            binder.upper = inner as i64;
            value.domain.binders.push(binder);
            value.count = outer * inner;
            value.output_map = solve::TensorOutputMap {
                start: 100,
                strides: vec![
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: outer_stride,
                    },
                    solve::AffineStencilIndexStrideTerm {
                        dimension: 1,
                        stride: inner_stride,
                    },
                ],
            };
            if validate_injective_output(&value, &[outer, inner]).is_ok() {
                let outputs = value.output_map.output_indices(&value.domain).unwrap();
                let unique: HashSet<_> = outputs.iter().copied().collect();
                let covered: HashSet<_> = coverage_ranges(&value)
                    .unwrap()
                    .into_iter()
                    .flat_map(|(start, end)| start..end)
                    .collect();
                assert_eq!(covered, unique, "coverage changed selected output set");
                assert_eq!(
                    unique.len(),
                    outputs.len(),
                    "unsafe accepted output map: {:?}",
                    value.output_map
                );
            }
        }
    }

    #[test]
    fn rectangular_coverage_scales_with_rows_not_cells() {
        let mut value = family(4);
        let mut inner = value.domain.binders[0].clone();
        inner.id = 1;
        inner.upper = 1024;
        value.domain.binders.push(inner);
        value.count = 4096;
        value.output_map = solve::TensorOutputMap {
            start: 7,
            strides: vec![
                solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2048,
                },
                solve::AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 1,
                },
            ],
        };
        assert_eq!(
            coverage_ranges(&value).unwrap(),
            vec![(7, 1031), (2055, 3079), (4103, 5127), (6151, 7175)]
        );
    }

    #[test]
    fn compact_jax_refuses_negative_gathers_instead_of_wrapping() {
        let mut value = family(4);
        value.load_strides[0].terms[0].stride = -1;
        assert!(emit(&value, "kernel", "x").is_err());
        assert!(!supports(&value));
        value.base_ops[0] = solve::LinearOp::LoadY { dst: 0, index: 3 };
        assert!(emit(&value, "kernel", "x").is_ok());
    }

    #[test]
    fn compact_jax_refuses_unsupported_operations_and_bad_identifiers() {
        let mut value = family(128);
        value
            .base_ops
            .insert(0, solve::LinearOp::LoadSeed { dst: 9, index: 0 });
        assert!(emit(&value, "kernel", "x").is_err());
        assert!(emit(&family(128), "bad;statement", "x").is_err());
    }
    #[test]
    fn compact_jax_refuses_overlap_and_supports_strided_output() {
        let mut value = family(128);
        value.output_map.strides[0].stride = 0;
        assert!(emit(&value, "kernel", "x").is_err());
        value.output_map.strides[0].stride = 2;
        assert!(emit(&value, "kernel", "x").is_ok());
    }
}
