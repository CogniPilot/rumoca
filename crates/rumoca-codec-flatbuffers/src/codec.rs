//! Dynamic FlatBuffer pack/unpack using schema reflection plus routing config.

use rumoca_signal_frame::SignalFrame;

use crate::bfbs::{BaseType, Field, Object, SchemaSet};
use crate::config::{MessageConfig, RouteEntry};

// ── Little-endian byte helpers ───────────────────────────────────────────

fn get_u8(buf: &[u8], off: usize) -> u8 {
    buf[off]
}
fn get_u16(buf: &[u8], off: usize) -> u16 {
    u16::from_le_bytes([buf[off], buf[off + 1]])
}
fn get_i32(buf: &[u8], off: usize) -> i32 {
    i32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}
fn get_f32(buf: &[u8], off: usize) -> f32 {
    f32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}
fn get_f64(buf: &[u8], off: usize) -> f64 {
    f64::from_le_bytes([
        buf[off],
        buf[off + 1],
        buf[off + 2],
        buf[off + 3],
        buf[off + 4],
        buf[off + 5],
        buf[off + 6],
        buf[off + 7],
    ])
}

fn put_u8(buf: &mut [u8], off: usize, v: u8) {
    buf[off] = v;
}
fn put_u16(buf: &mut [u8], off: usize, v: u16) {
    buf[off..off + 2].copy_from_slice(&v.to_le_bytes());
}
fn put_u32(buf: &mut [u8], off: usize, v: u32) {
    buf[off..off + 4].copy_from_slice(&v.to_le_bytes());
}
fn put_i32(buf: &mut [u8], off: usize, v: i32) {
    buf[off..off + 4].copy_from_slice(&v.to_le_bytes());
}
fn put_f32(buf: &mut [u8], off: usize, v: f32) {
    buf[off..off + 4].copy_from_slice(&v.to_le_bytes());
}
fn put_i64(buf: &mut [u8], off: usize, v: i64) {
    buf[off..off + 8].copy_from_slice(&v.to_le_bytes());
}
fn put_f64(buf: &mut [u8], off: usize, v: f64) {
    buf[off..off + 8].copy_from_slice(&v.to_le_bytes());
}

// ── Alignment helper ─────────────────────────────────────────────────────

fn align_up(offset: usize, align: usize) -> usize {
    (offset + align - 1) & !(align - 1)
}

// ── Unpack: read values from incoming flatbuffer ─────────────────────────

/// A precompiled instruction for reading one value from a flatbuffer.
#[derive(Debug, Clone)]
struct UnpackOp {
    /// The variable name to store the result (from routing config).
    var: String,
    /// Scale factor to apply after reading.
    scale: f64,
    /// Vtable slot offset for the table field (e.g., 4 for id=0, 6 for id=1).
    vtable_slot: u16,
    /// If the field is a struct, the byte offset within that struct for the
    /// leaf scalar. If the field IS the scalar, this is 0.
    struct_byte_offset: u16,
    /// The scalar type to read.
    scalar_type: BaseType,
}

/// Reads a scalar value from a buffer at the given offset.
fn read_scalar(buf: &[u8], off: usize, base_type: BaseType) -> f64 {
    match base_type {
        BaseType::Bool => get_u8(buf, off) as f64,
        BaseType::Byte => buf[off] as i8 as f64,
        BaseType::UByte | BaseType::UType => get_u8(buf, off) as f64,
        BaseType::Short => i16::from_le_bytes([buf[off], buf[off + 1]]) as f64,
        BaseType::UShort => get_u16(buf, off) as f64,
        BaseType::Int => get_i32(buf, off) as f64,
        BaseType::UInt => {
            u32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]]) as f64
        }
        BaseType::Long => i64::from_le_bytes([
            buf[off],
            buf[off + 1],
            buf[off + 2],
            buf[off + 3],
            buf[off + 4],
            buf[off + 5],
            buf[off + 6],
            buf[off + 7],
        ]) as f64,
        BaseType::ULong => u64::from_le_bytes([
            buf[off],
            buf[off + 1],
            buf[off + 2],
            buf[off + 3],
            buf[off + 4],
            buf[off + 5],
            buf[off + 6],
            buf[off + 7],
        ]) as f64,
        BaseType::Float => get_f32(buf, off) as f64,
        BaseType::Double => get_f64(buf, off),
        _ => 0.0,
    }
}

/// Writes a scalar value to a buffer at the given offset.
fn write_scalar(buf: &mut [u8], off: usize, base_type: BaseType, val: f64) {
    match base_type {
        BaseType::Bool => put_u8(buf, off, if val != 0.0 { 1 } else { 0 }),
        BaseType::Byte => put_u8(buf, off, val as i8 as u8),
        BaseType::UByte | BaseType::UType => put_u8(buf, off, val as u8),
        BaseType::Short => buf[off..off + 2].copy_from_slice(&(val as i16).to_le_bytes()),
        BaseType::UShort => put_u16(buf, off, val as u16),
        BaseType::Int => put_i32(buf, off, val as i32),
        BaseType::UInt => put_u32(buf, off, val as u32),
        BaseType::Long => put_i64(buf, off, val as i64),
        BaseType::ULong => buf[off..off + 8].copy_from_slice(&(val as u64).to_le_bytes()),
        BaseType::Float => put_f32(buf, off, val as f32),
        BaseType::Double => put_f64(buf, off, val),
        _ => {}
    }
}

pub struct UnpackCodec {
    ops: Vec<UnpackOp>,
    /// Expected packet size (computed from schema). Packets of other sizes
    /// are likely a different message type and should be skipped.
    expected_size: usize,
}

impl UnpackCodec {
    /// Compile an unpack codec from schema + routing config.
    ///
    /// `config.root_type` names the table (e.g., "cerebri2.topic.MotorOutput").
    /// `config.route` maps field paths like "motors.m0" to variable names.
    pub fn compile(schema: &SchemaSet, config: &MessageConfig) -> anyhow::Result<Self> {
        let root = schema.object_by_name(&config.root_type).ok_or_else(|| {
            anyhow::anyhow!("root type '{}' not found in schema", config.root_type)
        })?;

        if root.is_struct {
            anyhow::bail!(
                "root type '{}' must be a table, not a struct",
                config.root_type
            );
        }

        let mut ops = Vec::new();

        for (field_path, route_entry) in &config.route {
            let op = compile_unpack_path(schema, root, field_path, route_entry)?;
            ops.push(op);
        }

        // Compute expected packet size (same layout algorithm as PackCodec)
        let has_file_id = find_file_ident_for_root(schema, &config.root_type).is_some();
        let expected_size = compute_table_buf_size(schema, root, has_file_id)?;

        Ok(Self { ops, expected_size })
    }

    /// Expected packet size for this message type.
    pub fn expected_size(&self) -> usize {
        self.expected_size
    }

    /// Unpack a flatbuffer message into variable values.
    pub fn unpack(&self, buf: &[u8]) -> SignalFrame {
        let mut values = SignalFrame::new();
        if buf.len() < 4 {
            return values;
        }

        // Follow root offset to table
        let root_off = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]) as usize;
        // Root offset may be absolute or relative — in standard flatbuffers it's
        // relative to position 0. But if there's a file_identifier, root is at buf[0..4]
        // pointing forward. Let's handle both:
        let table_off = root_off; // root_off is an offset from position 0

        if table_off >= buf.len() {
            return values;
        }

        // Get vtable
        let soff = get_i32(buf, table_off);
        let vtable = ((table_off as i64) - (soff as i64)) as usize;
        if vtable >= buf.len() {
            return values;
        }

        for op in &self.ops {
            // Read field offset from vtable
            let vtable_size = get_u16(buf, vtable) as usize;
            if (op.vtable_slot as usize) >= vtable_size {
                continue;
            }
            let field_off = get_u16(buf, vtable + op.vtable_slot as usize);
            if field_off == 0 {
                continue; // field not present
            }

            let data_off = table_off + field_off as usize + op.struct_byte_offset as usize;
            if data_off + op.scalar_type.scalar_size() > buf.len() {
                continue;
            }

            let raw = read_scalar(buf, data_off, op.scalar_type);
            values.insert(op.var.clone(), raw * op.scale);
        }

        values
    }
}

/// Compile a single field path (e.g., "motors.m0") into an UnpackOp.
fn compile_unpack_path(
    schema: &SchemaSet,
    root: &Object,
    field_path: &str,
    route: &RouteEntry,
) -> anyhow::Result<UnpackOp> {
    let parts = split_field_path(field_path);

    if parts.is_empty() {
        anyhow::bail!("empty field path");
    }

    // First part: table field
    let table_field = root
        .field_by_name(parts[0])
        .ok_or_else(|| anyhow::anyhow!("field '{}' not found in {}", parts[0], root.name))?;

    let vtable_slot = table_field.offset;

    if parts.len() == 1 {
        // Direct scalar field on the table
        if table_field.field_type.base_type == BaseType::Obj {
            anyhow::bail!(
                "field path '{}' points to a struct/table, need a leaf scalar (e.g., '{}.fieldname')",
                field_path,
                field_path
            );
        }
        return Ok(UnpackOp {
            var: route.name().to_string(),
            scale: route.scale(),
            vtable_slot,
            struct_byte_offset: 0,
            scalar_type: table_field.field_type.base_type,
        });
    }

    // Multi-part: table_field must be an Obj (struct)
    if table_field.field_type.base_type != BaseType::Obj {
        anyhow::bail!(
            "field '{}' is not a struct/table, cannot access sub-field '{}'",
            parts[0],
            parts[1]
        );
    }

    // Resolve the struct
    let struct_obj = schema
        .objects
        .get(table_field.field_type.index as usize)
        .ok_or_else(|| {
            anyhow::anyhow!("object index {} out of range", table_field.field_type.index)
        })?;

    if !struct_obj.is_struct {
        anyhow::bail!(
            "nested tables not supported in routing (field '{}' is a table, not a struct)",
            parts[0]
        );
    }

    // Walk remaining path through the struct
    let mut current_struct = struct_obj;
    let mut byte_offset: u16 = 0;

    for &part in &parts[1..parts.len() - 1] {
        let f = current_struct.field_by_name(part).ok_or_else(|| {
            anyhow::anyhow!(
                "field '{}' not found in struct {}",
                part,
                current_struct.name
            )
        })?;
        if f.field_type.base_type != BaseType::Obj {
            anyhow::bail!("field '{}' is a scalar, cannot descend further", part);
        }
        byte_offset += f.offset;
        current_struct = schema
            .objects
            .get(f.field_type.index as usize)
            .ok_or_else(|| anyhow::anyhow!("object index {} out of range", f.field_type.index))?;
    }

    let leaf_name = parts[parts.len() - 1];
    let leaf_field = current_struct.field_by_name(leaf_name).ok_or_else(|| {
        anyhow::anyhow!(
            "field '{}' not found in struct {}",
            leaf_name,
            current_struct.name
        )
    })?;

    if leaf_field.field_type.base_type == BaseType::Obj {
        anyhow::bail!("field path '{}' ends at a struct, not a scalar", field_path);
    }

    byte_offset += leaf_field.offset;

    Ok(UnpackOp {
        var: route.name().to_string(),
        scale: route.scale(),
        vtable_slot,
        struct_byte_offset: byte_offset,
        scalar_type: leaf_field.field_type.base_type,
    })
}

// ── Pack: build outgoing flatbuffer ──────────────────────────────────────

/// A precompiled instruction for writing one value into a flatbuffer.
#[derive(Debug, Clone)]
struct PackOp {
    /// The variable name to read from (from routing config).
    var: String,
    /// Scale factor: value is divided by this before writing (inverse of unpack).
    scale: f64,
    /// Absolute byte offset in the output buffer where this scalar goes.
    buf_offset: usize,
    /// The scalar type to write.
    scalar_type: BaseType,
}

pub struct PackCodec {
    /// Pre-filled template buffer (vtable, root offset, file_id all set).
    template: Vec<u8>,
    /// Write operations to fill in field values.
    ops: Vec<PackOp>,
}

impl PackCodec {
    /// Compile a pack codec from schema + routing config.
    ///
    /// Computes a fixed-size buffer layout for the message and builds a
    /// template with vtable and structural bytes pre-filled.
    pub fn compile(schema: &SchemaSet, config: &MessageConfig) -> anyhow::Result<Self> {
        let root = schema.object_by_name(&config.root_type).ok_or_else(|| {
            anyhow::anyhow!("root type '{}' not found in schema", config.root_type)
        })?;

        if root.is_struct {
            anyhow::bail!(
                "root type '{}' must be a table, not a struct",
                config.root_type
            );
        }

        // Find the correct file_ident by checking which schema defines this root type
        let file_ident = find_file_ident_for_root(schema, &config.root_type);

        // Compute table layout:
        // Sort fields by id to determine order in the object
        let mut fields_by_id: Vec<&Field> = root.fields.iter().collect();
        fields_by_id.sort_by_key(|f| f.id);

        let num_fields = fields_by_id.len();
        let vtable_size = 4 + num_fields * 2; // vtable_size(2) + object_size(2) + fields

        // Determine buffer prefix: root_offset(4) + optional file_id(4)
        let has_file_id = file_ident.is_some();
        let prefix_size = if has_file_id { 8 } else { 4 };

        // Vtable starts after prefix
        let vtable_off = prefix_size;

        // Align vtable_size to even (it's always even since it's 4 + N*2)
        let vtable_end = vtable_off + vtable_size;

        // Table starts after vtable, aligned to 4
        let table_off = align_up(vtable_end, 4);

        // Layout fields within the table object
        // First 4 bytes of table are the soffset back to vtable
        let mut cursor = 4usize; // start after soffset
        let mut field_positions: Vec<(u16, usize, &Field)> = Vec::new(); // (id, offset_in_object, field)

        for &field in &fields_by_id {
            let (align, size) = if field.field_type.base_type == BaseType::Obj {
                let idx = field.field_type.index as usize;
                let obj = schema
                    .objects
                    .get(idx)
                    .ok_or_else(|| anyhow::anyhow!("obj index {idx} out of range"))?;
                (obj.minalign.max(1) as usize, obj.bytesize as usize)
            } else {
                let s = field.field_type.base_type.scalar_size();
                (s.max(1), s)
            };

            cursor = align_up(cursor, align);
            field_positions.push((field.id, cursor, field));
            cursor += size;
        }

        // Compute max alignment for the table
        let max_align = fields_by_id
            .iter()
            .map(|f| {
                if f.field_type.base_type == BaseType::Obj {
                    schema
                        .objects
                        .get(f.field_type.index as usize)
                        .map(|o| o.minalign.max(1) as usize)
                        .unwrap_or(4)
                } else {
                    f.field_type.base_type.scalar_size().max(1)
                }
            })
            .max()
            .unwrap_or(4);

        let object_size = align_up(cursor, max_align);
        let total_size = table_off + object_size;

        // Build template buffer
        let mut template = vec![0u8; total_size];

        // Root offset (points to table)
        put_u32(&mut template, 0, table_off as u32);

        // File identifier
        if let Some(ref fid) = file_ident {
            template[4..8].copy_from_slice(fid.as_bytes());
        }

        // Vtable
        put_u16(&mut template, vtable_off, vtable_size as u16);
        put_u16(&mut template, vtable_off + 2, object_size as u16);
        for &(id, off_in_obj, _) in &field_positions {
            let slot = vtable_off + 4 + (id as usize) * 2;
            put_u16(&mut template, slot, off_in_obj as u16);
        }

        // Table soffset (signed offset from table to vtable: table_off - vtable_off)
        put_u32(&mut template, table_off, (table_off - vtable_off) as u32);

        // Compile pack operations from routing
        let mut ops = Vec::new();
        for (field_path, route_entry) in &config.route {
            let op = compile_pack_path(
                schema,
                root,
                field_path,
                route_entry,
                table_off,
                &field_positions,
            )?;
            ops.push(op);
        }

        Ok(Self { template, ops })
    }

    /// Pack variable values into a flatbuffer message.
    pub fn pack(&self, values: &SignalFrame) -> Vec<u8> {
        let mut buf = self.template.clone();
        for op in &self.ops {
            let val = values.get(&op.var).copied().unwrap_or(0.0);
            let scaled = if op.scale != 1.0 { val / op.scale } else { val };
            write_scalar(&mut buf, op.buf_offset, op.scalar_type, scaled);
        }
        buf
    }

    /// Size of the packed message.
    pub fn size(&self) -> usize {
        self.template.len()
    }
}

/// Find the file_identifier for a given root type by checking which
/// .bfbs schema declared it (the schema's file_ident matches).
fn find_file_ident_for_root(schema: &SchemaSet, root_type: &str) -> Option<String> {
    // The file_ident is associated with the schema that declares the root_type.
    // Since we merged schemas, we stored file_idents in order.
    // A .bfbs's file_ident applies to its root_type declaration.
    // For now, find the schema whose file_ident is non-None and whose objects
    // include the root_type.
    //
    // Since SchemaSet doesn't track which schema each object came from,
    // we use a heuristic: return the first non-None file_ident that matches
    // the root_type's namespace prefix.

    // Check if root_type namespace matches any file_ident
    // cerebri2.sil.SimInput → file_ident "C2SI"
    // cerebri2.topic.MotorOutput → no file_ident (topics.fbs has no root_type)

    // Actually, the .bfbs file_ident comes from the `file_identifier` declaration
    // in the .fbs source. Only schemas with root_type have file_identifier.
    // cerebri2_sil.fbs: root_type SimInput, file_identifier "C2SI"
    // cerebri2_topics.fbs: no root_type declaration

    for fi in &schema.file_idents {
        if let Some(s) = fi
            && !s.is_empty()
        {
            // We only have one meaningful file_ident in our schemas.
            // If the root_type is from the sil namespace, use it.
            if root_type.contains(".sil.") {
                return Some(s.clone());
            }
        }
    }
    None
}

/// Compute the expected buffer size for a table with all-inline fields.
fn compute_table_buf_size(
    schema: &SchemaSet,
    root: &Object,
    has_file_id: bool,
) -> anyhow::Result<usize> {
    let mut fields_by_id: Vec<&Field> = root.fields.iter().collect();
    fields_by_id.sort_by_key(|f| f.id);

    let num_fields = fields_by_id.len();
    let vtable_size = 4 + num_fields * 2;
    let prefix_size = if has_file_id { 8 } else { 4 };
    let vtable_off = prefix_size;
    let vtable_end = vtable_off + vtable_size;
    let table_off = align_up(vtable_end, 4);

    let mut cursor = 4usize;
    for &field in &fields_by_id {
        let (a, s) = if field.field_type.base_type == BaseType::Obj {
            let obj = schema
                .objects
                .get(field.field_type.index as usize)
                .ok_or_else(|| {
                    anyhow::anyhow!("obj index {} out of range", field.field_type.index)
                })?;
            (obj.minalign.max(1) as usize, obj.bytesize as usize)
        } else {
            let sz = field.field_type.base_type.scalar_size();
            (sz.max(1), sz)
        };
        cursor = align_up(cursor, a);
        cursor += s;
    }

    let max_align = fields_by_id
        .iter()
        .map(|f| {
            if f.field_type.base_type == BaseType::Obj {
                schema
                    .objects
                    .get(f.field_type.index as usize)
                    .map(|o| o.minalign.max(1) as usize)
                    .unwrap_or(4)
            } else {
                f.field_type.base_type.scalar_size().max(1)
            }
        })
        .max()
        .unwrap_or(4);

    let object_size = align_up(cursor, max_align);
    Ok(table_off + object_size)
}

/// Compile a single field path into a PackOp.
fn compile_pack_path(
    schema: &SchemaSet,
    root: &Object,
    field_path: &str,
    route: &RouteEntry,
    table_off: usize,
    field_positions: &[(u16, usize, &Field)],
) -> anyhow::Result<PackOp> {
    let parts = split_field_path(field_path);
    if parts.is_empty() {
        anyhow::bail!("empty field path");
    }

    let table_field = root
        .field_by_name(parts[0])
        .ok_or_else(|| anyhow::anyhow!("field '{}' not found in {}", parts[0], root.name))?;

    // Find this field's position in the object
    let (_, field_obj_off, _) = field_positions
        .iter()
        .find(|(id, _, _)| *id == table_field.id)
        .ok_or_else(|| anyhow::anyhow!("field '{}' not in layout", parts[0]))?;

    let field_abs = table_off + field_obj_off;

    if parts.len() == 1 {
        if table_field.field_type.base_type == BaseType::Obj {
            anyhow::bail!("field '{}' is a struct, need leaf path", field_path);
        }
        return Ok(PackOp {
            var: route.name().to_string(),
            scale: route.scale(),
            buf_offset: field_abs,
            scalar_type: table_field.field_type.base_type,
        });
    }

    // Navigate into struct
    if table_field.field_type.base_type != BaseType::Obj {
        anyhow::bail!("field '{}' is not a struct", parts[0]);
    }

    let struct_obj = schema
        .objects
        .get(table_field.field_type.index as usize)
        .ok_or_else(|| {
            anyhow::anyhow!("obj index {} out of range", table_field.field_type.index)
        })?;

    let mut current_struct = struct_obj;
    let mut byte_offset: usize = 0;

    for &part in &parts[1..parts.len() - 1] {
        let f = current_struct.field_by_name(part).ok_or_else(|| {
            anyhow::anyhow!("field '{}' not in struct {}", part, current_struct.name)
        })?;
        if f.field_type.base_type != BaseType::Obj {
            anyhow::bail!("field '{}' is scalar, cannot descend", part);
        }
        byte_offset += f.offset as usize;
        current_struct = schema
            .objects
            .get(f.field_type.index as usize)
            .ok_or_else(|| anyhow::anyhow!("obj index {} out of range", f.field_type.index))?;
    }

    let leaf_name = parts[parts.len() - 1];
    let leaf_field = current_struct.field_by_name(leaf_name).ok_or_else(|| {
        anyhow::anyhow!(
            "field '{}' not in struct {}",
            leaf_name,
            current_struct.name
        )
    })?;

    if leaf_field.field_type.base_type == BaseType::Obj {
        anyhow::bail!("field path '{}' ends at struct, not scalar", field_path);
    }

    byte_offset += leaf_field.offset as usize;

    Ok(PackOp {
        var: route.name().to_string(),
        scale: route.scale(),
        buf_offset: field_abs + byte_offset,
        scalar_type: leaf_field.field_type.base_type,
    })
}

fn split_field_path(field_path: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    for (idx, ch) in field_path.char_indices() {
        if ch == '.' {
            parts.push(&field_path[start..idx]);
            start = idx + 1;
        }
    }
    parts.push(&field_path[start..]);
    parts
}
