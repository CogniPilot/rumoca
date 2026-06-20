use super::*;

#[derive(Debug)]
pub(in crate::codegen) struct SolveRowsValue {
    rows: Arc<Vec<Vec<solve::LinearOp>>>,
}

impl SolveRowsValue {
    pub(in crate::codegen) fn new(rows: Vec<Vec<solve::LinearOp>>) -> Self {
        Self {
            rows: Arc::new(rows),
        }
    }

    pub(in crate::codegen) fn from_arc(rows: Arc<Vec<Vec<solve::LinearOp>>>) -> Self {
        Self { rows }
    }
}

impl Object for SolveRowsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        (index < self.rows.len()).then(|| {
            Value::from_object(SolveRowValue {
                rows: self.rows.clone(),
                index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.rows.len())
    }
}

/// Native affine families exposed to templates as a typed sequence; each
/// element is a `SolveNativeFamilyValue` the native-family renderer can downcast.
#[derive(Debug)]
pub(in crate::codegen) struct SolveNativeFamiliesValue {
    families: Arc<Vec<RenderNativeAffineFamily>>,
}

impl SolveNativeFamiliesValue {
    pub(in crate::codegen) fn new(families: Vec<RenderNativeAffineFamily>) -> Self {
        Self {
            families: Arc::new(families),
        }
    }

    fn families(&self) -> &[RenderNativeAffineFamily] {
        self.families.as_ref()
    }
}

impl Object for SolveNativeFamiliesValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        (index < self.families.len()).then(|| {
            Value::from_object(SolveNativeFamilyValue {
                families: self.families.clone(),
                index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.families.len())
    }
}
#[derive(Debug)]
pub(in crate::codegen) struct SolveNativeFamilyValue {
    families: Arc<Vec<RenderNativeAffineFamily>>,
    index: usize,
}

impl SolveNativeFamilyValue {
    fn family(&self) -> &RenderNativeAffineFamily {
        &self.families[self.index]
    }
}

impl Object for SolveNativeFamilyValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }
    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "kind" => Some(Value::from(self.family().kind)),
            "output_offset" => Some(Value::from(self.family().output_offset)),
            "count" => Some(Value::from(self.family().count)),
            _ => None,
        }
    }
    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["kind", "output_offset", "count"])
    }
}

struct TemplateDomainLayout {
    shape: Vec<usize>,
    dimensions: Vec<Option<usize>>,
}

fn domain_shape_for_template(
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<Vec<usize>, minijinja::Error> {
    Ok(template_domain_layout(domain)?.shape)
}

fn output_map_for_template(
    family: &RenderNativeAffineFamily,
) -> Result<serde_json::Value, minijinja::Error> {
    let layout = template_domain_layout(&family.domain)?;
    let mut compact_strides =
        render_vec_with_capacity(layout.shape.len(), "template output-map stride count")?;
    compact_strides.extend(std::iter::repeat_n(0isize, layout.shape.len()));
    for term in &family.output_map.strides {
        if term.stride == 0 {
            continue;
        }
        let Some(Some(dimension)) = layout.dimensions.get(term.dimension) else {
            continue;
        };
        compact_strides[*dimension] += term.stride;
    }
    let mut strides = render_vec_with_capacity(
        compact_strides.len(),
        "template output-map compact stride count",
    )?;
    for (dimension, stride) in compact_strides.into_iter().enumerate() {
        if stride != 0 {
            strides.push(serde_json::json!({
                "dimension": dimension,
                "stride": stride,
            }));
        }
    }
    Ok(serde_json::json!({
        "start": family.output_map.start,
        "strides": strides,
    }))
}

fn template_domain_layout(
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<TemplateDomainLayout, minijinja::Error> {
    let mut shape = render_vec_with_capacity(domain.binders.len(), "template domain shape rank")?;
    let mut dimensions =
        render_vec_with_capacity(domain.binders.len(), "template domain dimension count")?;
    for binder in &domain.binders {
        let value_count = binder.value_count_for_render()?;
        if value_count > 1 {
            dimensions.push(Some(shape.len()));
            shape.push(value_count);
        } else {
            dimensions.push(None);
        }
    }
    if shape.is_empty() {
        shape.push(1);
    }
    Ok(TemplateDomainLayout { shape, dimensions })
}

/// Template function: render a native affine family as a WGSL expression
/// parametric in the kernel-local row offset `r`.
pub(in crate::codegen) fn render_solve_native_family_wgsl_function(
    family: Value,
    config: Value,
) -> RenderResult {
    let Some(family) = family.downcast_object_ref::<SolveNativeFamilyValue>() else {
        return Err(render_err(
            "render_solve_native_family_wgsl expects a native family from solve_blocks",
        ));
    };
    render_native_family_expr_wgsl(family.family(), &config)
}

pub(in crate::codegen) fn render_solve_native_family_output_index_wgsl_function(
    family: Value,
) -> RenderResult {
    let Some(family) = family.downcast_object_ref::<SolveNativeFamilyValue>() else {
        return Err(render_err(
            "render_solve_native_family_output_index_wgsl expects a native family from solve_blocks",
        ));
    };
    affine_output_index_expr(&family.family().output_map, &family.family().domain)
}

pub(in crate::codegen) fn render_solve_native_family_output_map_start_function(
    family: Value,
) -> RenderResult {
    let Some(family) = family.downcast_object_ref::<SolveNativeFamilyValue>() else {
        return Err(render_err(
            "render_solve_native_family_output_map_start expects a native family from solve_blocks",
        ));
    };
    output_map_for_template(family.family())?;
    Ok(family.family().output_map.start.to_string())
}

#[derive(Debug, Clone)]
pub(in crate::codegen) struct RenderNativeAffineFamily {
    pub(in crate::codegen) kind: &'static str,
    pub(in crate::codegen) domain: rumoca_core::StructuredIndexDomain,
    pub(in crate::codegen) output_offset: usize,
    pub(in crate::codegen) count: usize,
    pub(in crate::codegen) output_map: solve::TensorOutputMap,
    pub(in crate::codegen) base_ops: Vec<solve::LinearOp>,
    pub(in crate::codegen) load_strides: Vec<solve::AffineStencilLoadStride>,
    pub(in crate::codegen) const_strides: Vec<solve::AffineStencilConstStride>,
}

#[derive(Debug, Clone)]
pub(in crate::codegen) struct RenderScalarFallbackRow {
    pub(in crate::codegen) row_index: usize,
    pub(in crate::codegen) output_index: usize,
    pub(in crate::codegen) output_ordinal: usize,
}
#[derive(Debug)]
pub(in crate::codegen) struct SolveScalarFallbackRowsValue {
    rows: Arc<Vec<RenderScalarFallbackRow>>,
}

impl SolveScalarFallbackRowsValue {
    pub(in crate::codegen) fn new(rows: Vec<RenderScalarFallbackRow>) -> Self {
        Self {
            rows: Arc::new(rows),
        }
    }

    fn rows(&self) -> &[RenderScalarFallbackRow] {
        self.rows.as_ref()
    }
}

impl Object for SolveScalarFallbackRowsValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        (index < self.rows.len()).then(|| {
            Value::from_object(SolveScalarFallbackRowValue {
                rows: self.rows.clone(),
                index,
            })
        })
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.rows.len())
    }
}

#[derive(Debug)]
pub(in crate::codegen) struct SolveScalarFallbackRowValue {
    rows: Arc<Vec<RenderScalarFallbackRow>>,
    index: usize,
}

impl SolveScalarFallbackRowValue {
    fn row(&self) -> &RenderScalarFallbackRow {
        &self.rows[self.index]
    }
}

impl Object for SolveScalarFallbackRowValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Map
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        match key.as_str()? {
            "row_index" => Some(Value::from(self.row().row_index)),
            "output_index" => Some(Value::from(self.row().output_index)),
            "output_ordinal" => Some(Value::from(self.row().output_ordinal)),
            _ => None,
        }
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Str(&["row_index", "output_index", "output_ordinal"])
    }
}

pub(in crate::codegen) fn render_wgsl_kernel_schedule_json_function(
    native_families: Value,
    scalar_fallback_rows: Value,
    config: Value,
) -> RenderResult {
    let native_families = native_families
        .downcast_object_ref::<SolveNativeFamiliesValue>()
        .ok_or_else(|| render_err("wgsl_kernel_schedule_json expects solve native families"))?;
    let scalar_fallback_rows = scalar_fallback_rows
        .downcast_object_ref::<SolveScalarFallbackRowsValue>()
        .ok_or_else(|| {
            render_err("wgsl_kernel_schedule_json expects solve scalar fallback rows")
        })?;
    let native_entry_prefix = required_string_field(&config, "native_entry_prefix")?;
    let scalar_entry_prefix = required_string_field(&config, "scalar_entry_prefix")?;
    let workgroup_size = required_usize_field(&config, "workgroup_size")?;
    if workgroup_size == 0 {
        return Err(render_err("workgroup_size must be greater than zero"));
    }
    let include_empty_scalar_chunk = required_bool_field(&config, "include_empty_scalar_chunk")?;
    let schedule = wgsl_kernel_schedule(
        native_families.families(),
        scalar_fallback_rows.rows(),
        &native_entry_prefix,
        &scalar_entry_prefix,
        workgroup_size,
        include_empty_scalar_chunk,
    )?;
    serde_json::to_string_pretty(&schedule)
        .map_err(|err| render_err(format!("serialize WGSL kernel schedule: {err}")))
}

pub(in crate::codegen) fn render_wgsl_kernel_workgroup_total_function(
    native_families: Value,
    scalar_fallback_rows: Value,
    config: Value,
) -> RenderResult {
    let native_families = native_families
        .downcast_object_ref::<SolveNativeFamiliesValue>()
        .ok_or_else(|| render_err("wgsl_kernel_workgroup_total expects solve native families"))?;
    let scalar_fallback_rows = scalar_fallback_rows
        .downcast_object_ref::<SolveScalarFallbackRowsValue>()
        .ok_or_else(|| {
            render_err("wgsl_kernel_workgroup_total expects solve scalar fallback rows")
        })?;
    let workgroup_size = required_usize_field(&config, "workgroup_size")?;
    wgsl_kernel_workgroup_total(
        native_families.families(),
        scalar_fallback_rows.rows(),
        workgroup_size,
    )
    .map(|count| count.to_string())
}

pub(in crate::codegen) fn render_wgsl_native_family_inventory_json_function(
    native_families: Value,
) -> RenderResult {
    let native_families = native_families
        .downcast_object_ref::<SolveNativeFamiliesValue>()
        .ok_or_else(|| {
            render_err("wgsl_native_family_inventory_json expects solve native families")
        })?;
    let mut inventory = wgsl_manifest_vec_with_capacity(
        native_families.families().len(),
        "WGSL native family inventory entry count",
    )?;
    for family in native_families.families() {
        inventory.push(wgsl_native_family_inventory_entry(family)?);
    }
    serde_json::to_string_pretty(&inventory)
        .map_err(|err| render_err(format!("serialize WGSL native family inventory: {err}")))
}

fn wgsl_kernel_schedule(
    native_families: &[RenderNativeAffineFamily],
    scalar_fallback_rows: &[RenderScalarFallbackRow],
    native_entry_prefix: &str,
    scalar_entry_prefix: &str,
    workgroup_size: usize,
    include_empty_scalar_chunk: bool,
) -> Result<Vec<serde_json::Value>, minijinja::Error> {
    let entry_count = wgsl_kernel_schedule_entry_count(
        native_families.len(),
        scalar_fallback_rows.len(),
        workgroup_size,
        include_empty_scalar_chunk,
    )?;
    let mut entries =
        wgsl_manifest_vec_with_capacity(entry_count, "WGSL kernel schedule entry count")?;
    push_native_kernel_entries(
        &mut entries,
        native_families,
        native_entry_prefix,
        workgroup_size,
    )?;
    push_scalar_kernel_entries(
        &mut entries,
        scalar_fallback_rows,
        scalar_entry_prefix,
        workgroup_size,
    )?;
    if entries.is_empty() && include_empty_scalar_chunk {
        entries.push(serde_json::json!({
            "entry": format!("{scalar_entry_prefix}0"),
            "rows": 0,
            "start_slot": 0,
            "output_indices": [],
            "workgroup_size": workgroup_size,
        }));
    }
    Ok(entries)
}

pub(in crate::codegen) fn wgsl_kernel_schedule_entry_count(
    native_count: usize,
    scalar_row_count: usize,
    workgroup_size: usize,
    include_empty_scalar_chunk: bool,
) -> Result<usize, minijinja::Error> {
    let scalar_chunks = scalar_kernel_chunk_count(scalar_row_count, workgroup_size)?;
    let count = native_count
        .checked_add(scalar_chunks)
        .ok_or_else(|| render_err("WGSL kernel schedule entry count overflows host index range"))?;
    if count == 0 && include_empty_scalar_chunk {
        Ok(1)
    } else {
        Ok(count)
    }
}

pub(in crate::codegen) fn scalar_kernel_chunk_count(
    scalar_row_count: usize,
    workgroup_size: usize,
) -> Result<usize, minijinja::Error> {
    if workgroup_size == 0 {
        return Err(render_err("workgroup_size must be greater than zero"));
    }
    if scalar_row_count == 0 {
        return Ok(0);
    }
    scalar_row_count
        .checked_add(workgroup_size - 1)
        .and_then(|count| count.checked_div(workgroup_size))
        .ok_or_else(|| render_err("WGSL scalar chunk count overflows host index range"))
}

pub(in crate::codegen) fn kernel_workgroup_count(
    row_count: usize,
    workgroup_size: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    if workgroup_size == 0 {
        return Err(render_err("workgroup_size must be greater than zero"));
    }
    if row_count == 0 {
        return Ok(0);
    }
    row_count
        .checked_add(workgroup_size - 1)
        .and_then(|count| count.checked_div(workgroup_size))
        .ok_or_else(|| {
            render_err(format!(
                "{context} workgroup count overflows host index range"
            ))
        })
}

pub(in crate::codegen) fn wgsl_kernel_workgroup_total(
    native_families: &[RenderNativeAffineFamily],
    scalar_fallback_rows: &[RenderScalarFallbackRow],
    workgroup_size: usize,
) -> Result<usize, minijinja::Error> {
    if workgroup_size == 0 {
        return Err(render_err("workgroup_size must be greater than zero"));
    }
    let mut total = 0usize;
    for family in native_families {
        total = total
            .checked_add(kernel_workgroup_count(
                family.count,
                workgroup_size,
                "WGSL native kernel",
            )?)
            .ok_or_else(|| render_err("WGSL kernel workgroup total overflows host index range"))?;
    }
    for chunk in scalar_fallback_rows.chunks(workgroup_size) {
        total = total
            .checked_add(kernel_workgroup_count(
                chunk.len(),
                workgroup_size,
                "WGSL scalar chunk",
            )?)
            .ok_or_else(|| render_err("WGSL kernel workgroup total overflows host index range"))?;
    }
    Ok(total)
}

fn wgsl_manifest_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, minijinja::Error> {
    render_vec_with_capacity(capacity, context)
}

fn push_native_kernel_entries(
    entries: &mut Vec<serde_json::Value>,
    native_families: &[RenderNativeAffineFamily],
    native_entry_prefix: &str,
    workgroup_size: usize,
) -> Result<(), minijinja::Error> {
    for (index, family) in native_families.iter().enumerate() {
        let workgroups =
            kernel_workgroup_count(family.count, workgroup_size, "WGSL native kernel")?;
        entries.push(serde_json::json!({
            "entry": format!("{native_entry_prefix}_{}{}", family.kind, index),
            "rows": family.count,
            "output_map": output_map_for_template(family)?,
            "workgroup_size": workgroup_size,
            "workgroups": workgroups,
        }));
    }
    Ok(())
}

fn push_scalar_kernel_entries(
    entries: &mut Vec<serde_json::Value>,
    scalar_fallback_rows: &[RenderScalarFallbackRow],
    scalar_entry_prefix: &str,
    workgroup_size: usize,
) -> Result<(), minijinja::Error> {
    for (chunk_index, chunk) in scalar_fallback_rows.chunks(workgroup_size).enumerate() {
        let start_slot = chunk_index
            .checked_mul(workgroup_size)
            .ok_or_else(|| render_err("WGSL scalar chunk start slot overflows host index range"))?;
        let mut output_indices =
            wgsl_manifest_vec_with_capacity(chunk.len(), "WGSL scalar chunk output index count")?;
        for row in chunk {
            output_indices.push(row.output_index);
        }
        let workgroups = kernel_workgroup_count(chunk.len(), workgroup_size, "WGSL scalar chunk")?;
        entries.push(serde_json::json!({
            "entry": format!("{scalar_entry_prefix}{chunk_index}"),
            "rows": chunk.len(),
            "start_slot": start_slot,
            "output_indices": output_indices,
            "workgroup_size": workgroup_size,
            "workgroups": workgroups,
        }));
    }
    Ok(())
}

fn wgsl_native_family_inventory_entry(
    family: &RenderNativeAffineFamily,
) -> Result<serde_json::Value, minijinja::Error> {
    Ok(serde_json::json!({
        "kind": family.kind,
        "output_map": output_map_for_template(family)?,
        "rows": family.count,
        "domain_shape": domain_shape_for_template(&family.domain)?,
    }))
}

pub(in crate::codegen) struct NativeFamilyTemplatePartition {
    pub(in crate::codegen) families: Vec<RenderNativeAffineFamily>,
    pub(in crate::codegen) scalar_fallback_rows: Vec<RenderScalarFallbackRow>,
    pub(in crate::codegen) map_family_count: usize,
    pub(in crate::codegen) stencil_family_count: usize,
}

pub(in crate::codegen) fn native_family_template_partition(
    block: &solve::ComputeBlock,
) -> Result<NativeFamilyTemplatePartition, rumoca_eval_solve::ScalarizeError> {
    let mut partition = NativeFamilyTemplatePartition {
        families: Vec::new(),
        scalar_fallback_rows: Vec::new(),
        map_family_count: 0,
        stencil_family_count: 0,
    };
    let mut scalar_row_index = 0;
    let mut output_cursor = 0;
    for node in &block.nodes {
        partition_node_for_template(
            &mut partition,
            node,
            &mut scalar_row_index,
            &mut output_cursor,
        )?;
    }
    Ok(partition)
}

// SPEC_0021: Exception - native-family partitioning handles every ComputeNode
// variant in one place so scalar fallback cursor movement is auditable.
#[allow(clippy::too_many_lines)]
fn partition_node_for_template(
    partition: &mut NativeFamilyTemplatePartition,
    node: &solve::ComputeNode,
    scalar_row_index: &mut usize,
    output_cursor: &mut usize,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    match node {
        solve::ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            span,
            ..
        } => {
            let span = required_compute_node_span(*span, "native map family")?;
            let output_indices =
                rumoca_eval_solve::tensor_output_indices(domain, output_map, "map", span)?;
            let scalar_count = render_domain_scalar_count(domain, "map", span)?;
            push_native_affine_family(
                partition,
                "map",
                domain,
                output_map,
                scalar_count,
                base_ops,
                load_strides,
                const_strides,
                span,
            )?;
            partition.map_family_count += 1;
            *scalar_row_index = rumoca_eval_solve::checked_contiguous_output_count(
                *scalar_row_index,
                scalar_count,
                "map scalar rows",
                span,
            )?;
            *output_cursor = (*output_cursor).max(rumoca_eval_solve::checked_tensor_output_count(
                &output_indices,
                *output_cursor,
                "map",
                span,
            )?);
        }
        solve::ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            span,
            ..
        } => {
            let span = required_compute_node_span(*span, "native affine stencil family")?;
            let output_indices = rumoca_eval_solve::tensor_output_indices(
                domain,
                output_map,
                "affine stencil",
                span,
            )?;
            let scalar_count = render_domain_scalar_count(domain, "affine stencil", span)?;
            push_native_affine_family(
                partition,
                "stencil",
                domain,
                output_map,
                scalar_count,
                base_ops,
                load_strides,
                const_strides,
                span,
            )?;
            partition.stencil_family_count += 1;
            *scalar_row_index = rumoca_eval_solve::checked_contiguous_output_count(
                *scalar_row_index,
                scalar_count,
                "affine stencil scalar rows",
                span,
            )?;
            *output_cursor = (*output_cursor).max(rumoca_eval_solve::checked_tensor_output_count(
                &output_indices,
                *output_cursor,
                "affine stencil",
                span,
            )?);
        }
        solve::ComputeNode::ScalarPrograms(block) => {
            let block_span = scalar_program_block_source_span(block)?;
            let output_indices = rumoca_eval_solve::scalar_program_output_indices(
                block,
                *output_cursor,
                "scalar fallback",
            )?;
            push_scalar_program_block_fallback_rows(
                partition,
                block,
                *scalar_row_index,
                &output_indices,
                block_span,
            )?;
            *scalar_row_index = rumoca_eval_solve::checked_contiguous_output_count(
                *scalar_row_index,
                block.row_count(),
                "scalar fallback rows",
                block_span,
            )?;
            *output_cursor = rumoca_eval_solve::scalar_program_output_count(
                block,
                *output_cursor,
                "scalar fallback",
            )?;
        }
        solve::ComputeNode::MatMul { m, n, span, .. } => {
            let span = required_compute_node_span(*span, "matmul scalar fallback")?;
            let count = checked_product(*m, *n, "matmul scalar fallback", span)?;
            push_multi_output_tensor_fallback_program(
                partition,
                scalar_row_index,
                output_cursor,
                count,
                span,
            )?;
        }
        solve::ComputeNode::LinSolve { n, span, .. } => {
            let span = required_compute_node_span(*span, "linsolve scalar fallback")?;
            push_multi_output_tensor_fallback_program(
                partition,
                scalar_row_index,
                output_cursor,
                *n,
                span,
            )?;
        }
    }
    Ok(())
}

fn required_compute_node_span(
    span: rumoca_core::Span,
    kind: &'static str,
) -> Result<rumoca_core::Span, rumoca_eval_solve::ScalarizeError> {
    if span.is_dummy() {
        return Err(rumoca_eval_solve::ScalarizeError::MissingSourceSpan { kind });
    }
    Ok(span)
}

pub(in crate::codegen) fn scalar_program_block_source_span(
    block: &solve::ScalarProgramBlock,
) -> Result<rumoca_core::Span, rumoca_eval_solve::ScalarizeError> {
    block
        .first_source_span()
        .ok_or(rumoca_eval_solve::ScalarizeError::MissingSourceSpan {
            kind: "scalar fallback",
        })
}

pub(in crate::codegen) fn scalar_program_row_span(
    block: &solve::ScalarProgramBlock,
    row: usize,
    fallback: rumoca_core::Span,
) -> rumoca_core::Span {
    block.program_span(row).unwrap_or(fallback)
}

fn push_scalar_program_block_fallback_rows(
    partition: &mut NativeFamilyTemplatePartition,
    block: &solve::ScalarProgramBlock,
    row_start: usize,
    output_indices: &[usize],
    block_span: rumoca_core::Span,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    reserve_partition_capacity(
        &mut partition.scalar_fallback_rows,
        output_indices.len(),
        "scalar fallback template row count",
        Some(block_span),
    )?;
    let mut output_ordinal = 0usize;
    for (program_offset, program) in block.programs.iter().enumerate() {
        let program_span = scalar_program_row_span(block, program_offset, block_span);
        let row_index = row_start.checked_add(program_offset).ok_or(
            rumoca_eval_solve::ScalarizeError::ContiguousOutputOverflow {
                kind: "scalar fallback row",
                start: row_start,
                count: program_offset,
                span: program_span,
            },
        )?;
        push_program_output_fallback_rows(
            &mut partition.scalar_fallback_rows,
            program,
            row_index,
            output_indices,
            &mut output_ordinal,
            program_span,
        )?;
    }
    Ok(())
}

fn push_program_output_fallback_rows(
    rows: &mut Vec<RenderScalarFallbackRow>,
    program: &[solve::LinearOp],
    row_index: usize,
    output_indices: &[usize],
    output_ordinal: &mut usize,
    span: rumoca_core::Span,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    let output_count = solve::ScalarProgramBlock::program_output_count(program);
    for program_output_ordinal in 0..output_count {
        let output_index = *output_indices.get(*output_ordinal).ok_or_else(|| {
            rumoca_eval_solve::ScalarizeError::ShapeContract {
                message: format!(
                    "scalar fallback output index missing for StoreOutput ordinal {}",
                    *output_ordinal
                ),
                span: Some(span),
            }
        })?;
        rows.push(RenderScalarFallbackRow {
            row_index,
            output_index,
            output_ordinal: program_output_ordinal,
        });
        *output_ordinal = output_ordinal.checked_add(1).ok_or(
            rumoca_eval_solve::ScalarizeError::OutputCountOverflow {
                kind: "scalar fallback output ordinal",
                index: *output_ordinal,
                span,
            },
        )?;
    }
    Ok(())
}

fn render_domain_scalar_count(
    domain: &rumoca_core::StructuredIndexDomain,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, rumoca_eval_solve::ScalarizeError> {
    domain
        .scalar_count()
        .map_err(|err| rumoca_eval_solve::ScalarizeError::ShapeContract {
            message: format!("native {kind} family structured index domain is invalid: {err}"),
            span: Some(span),
        })
}

fn push_multi_output_tensor_fallback_program(
    partition: &mut NativeFamilyTemplatePartition,
    scalar_row_index: &mut usize,
    output_cursor: &mut usize,
    count: usize,
    span: rumoca_core::Span,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    let next_scalar_row_index = rumoca_eval_solve::checked_contiguous_output_count(
        *scalar_row_index,
        usize::from(count > 0),
        "scalar fallback rows",
        span,
    )?;
    let next_output_cursor = rumoca_eval_solve::checked_contiguous_output_count(
        *output_cursor,
        count,
        "scalar fallback output",
        span,
    )?;
    reserve_partition_capacity(
        &mut partition.scalar_fallback_rows,
        count,
        "scalar fallback template row count",
        Some(span),
    )?;
    for offset in 0..count {
        partition
            .scalar_fallback_rows
            .push(RenderScalarFallbackRow {
                row_index: *scalar_row_index,
                output_index: *output_cursor + offset,
                output_ordinal: offset,
            });
    }
    *scalar_row_index = next_scalar_row_index;
    *output_cursor = next_output_cursor;
    Ok(())
}

fn checked_product(
    lhs: usize,
    rhs: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, rumoca_eval_solve::ScalarizeError> {
    lhs.checked_mul(rhs)
        .ok_or(rumoca_eval_solve::ScalarizeError::ProductOverflow {
            kind,
            lhs,
            rhs,
            span,
        })
}

fn partition_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, rumoca_eval_solve::ScalarizeError> {
    let mut values = Vec::new();
    reserve_partition_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_partition_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    if additional == 0 {
        return Ok(());
    }
    let span =
        span.ok_or(rumoca_eval_solve::ScalarizeError::MissingSourceSpan { kind: context })?;
    values.try_reserve_exact(additional).map_err(|_| {
        rumoca_eval_solve::ScalarizeError::ShapeContract {
            message: format!("{context} exceeds host memory limits"),
            span: Some(span),
        }
    })
}

// SPEC_0021: Exception - affine family serialization groups domain, output
// map, op rows, strides, and span into one template object.
#[allow(clippy::too_many_arguments)]
fn push_native_affine_family(
    partition: &mut NativeFamilyTemplatePartition,
    kind: &'static str,
    domain: &rumoca_core::StructuredIndexDomain,
    output_map: &solve::TensorOutputMap,
    count: usize,
    base_ops: &[solve::LinearOp],
    load_strides: &[solve::AffineStencilLoadStride],
    const_strides: &[solve::AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<(), rumoca_eval_solve::ScalarizeError> {
    reserve_partition_capacity(
        &mut partition.families,
        1,
        "native family template partition count",
        Some(span),
    )?;
    let mut base_ops_out =
        partition_vec_with_capacity(base_ops.len(), "native family base op count", Some(span))?;
    for op in base_ops {
        base_ops_out.push(*op);
    }
    let mut load_strides_out = partition_vec_with_capacity(
        load_strides.len(),
        "native family load stride count",
        Some(span),
    )?;
    for stride in load_strides {
        load_strides_out.push(stride.clone());
    }
    let mut const_strides_out = partition_vec_with_capacity(
        const_strides.len(),
        "native family const stride count",
        Some(span),
    )?;
    for stride in const_strides {
        const_strides_out.push(stride.clone());
    }
    partition.families.push(RenderNativeAffineFamily {
        kind,
        domain: domain.clone(),
        output_offset: output_map.start,
        count,
        output_map: output_map.clone(),
        base_ops: base_ops_out,
        load_strides: load_strides_out,
        const_strides: const_strides_out,
    });
    Ok(())
}
