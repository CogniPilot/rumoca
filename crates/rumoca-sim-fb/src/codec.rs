//! Dynamic flatbuffer pack/unpack using schema reflection + routing config.
//!
//! Given a `.bfbs` schema and a TOML routing config, this module builds
//! precompiled "codecs" that can pack and unpack flatbuffer messages at
//! runtime without any generated code.

use std::collections::HashMap;

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
    pub fn unpack(&self, buf: &[u8]) -> HashMap<String, f64> {
        let mut values = HashMap::new();
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
    let parts: Vec<&str> = field_path.split('.').collect();

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
            var: route.var().to_string(),
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
        var: route.var().to_string(),
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
    pub fn pack(&self, values: &HashMap<String, f64>) -> Vec<u8> {
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
    let parts: Vec<&str> = field_path.split('.').collect();
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
            var: route.var().to_string(),
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
        var: route.var().to_string(),
        scale: route.scale(),
        buf_offset: field_abs + byte_offset,
        scalar_type: leaf_field.field_type.base_type,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn cerebri_bfbs_paths() -> Option<(PathBuf, PathBuf)> {
        let dir = std::env::var_os("RUMOCA_CEREBRI_BFBS_DIR")?;
        let dir = PathBuf::from(dir);
        Some((
            dir.join("cerebri2_topics.bfbs"),
            dir.join("cerebri2_sil.bfbs"),
        ))
    }

    fn load_test_schema() -> Option<SchemaSet> {
        let mut ss = SchemaSet::new();
        let Some((topics, sil)) = cerebri_bfbs_paths() else {
            eprintln!("skipping: set RUMOCA_CEREBRI_BFBS_DIR to a directory with bfbs files");
            return None;
        };
        if !topics.exists() || !sil.exists() {
            return None;
        }
        ss.load_bfbs(&topics).unwrap();
        ss.load_bfbs(&sil).unwrap();
        Some(ss)
    }

    #[test]
    fn pack_sim_input_matches_handcoded() {
        let Some(schema) = load_test_schema() else {
            eprintln!("skipping: bfbs files not found");
            return;
        };

        // Build routing config matching the hand-coded SimInput packer
        let mut route = HashMap::new();
        route.insert(
            "gyro.x".into(),
            crate::config::RouteEntry::Simple("gyro_x".into()),
        );
        route.insert(
            "gyro.y".into(),
            crate::config::RouteEntry::Simple("gyro_y".into()),
        );
        route.insert(
            "gyro.z".into(),
            crate::config::RouteEntry::Simple("gyro_z".into()),
        );
        route.insert(
            "accel.x".into(),
            crate::config::RouteEntry::Simple("accel_x".into()),
        );
        route.insert(
            "accel.y".into(),
            crate::config::RouteEntry::Simple("accel_y".into()),
        );
        route.insert(
            "accel.z".into(),
            crate::config::RouteEntry::Simple("accel_z".into()),
        );
        for i in 0..16 {
            route.insert(
                format!("rc.ch{i}"),
                crate::config::RouteEntry::Simple(format!("rc_{i}")),
            );
        }
        route.insert(
            "rc_link_quality".into(),
            crate::config::RouteEntry::Simple("rc_link_quality".into()),
        );
        route.insert(
            "rc_valid".into(),
            crate::config::RouteEntry::Simple("rc_valid".into()),
        );
        route.insert(
            "imu_valid".into(),
            crate::config::RouteEntry::Simple("imu_valid".into()),
        );

        let config = MessageConfig {
            root_type: "cerebri2.sil.SimInput".into(),
            route,
        };

        let codec = PackCodec::compile(&schema, &config).unwrap();
        eprintln!("SimInput pack size: {} bytes", codec.size());
        eprintln!("Template hex:");
        for (i, chunk) in codec.template.chunks(16).enumerate() {
            eprint!("  {:04x}: ", i * 16);
            for b in chunk {
                eprint!("{:02x} ", b);
            }
            eprintln!();
        }

        // Pack with test values
        let mut values = HashMap::new();
        values.insert("gyro_x".into(), 1.0);
        values.insert("gyro_y".into(), 2.0);
        values.insert("gyro_z".into(), 3.0);
        values.insert("accel_x".into(), 0.1);
        values.insert("accel_y".into(), 0.2);
        values.insert("accel_z".into(), 9.81);
        for i in 0..16 {
            values.insert(format!("rc_{i}"), 1500.0);
        }
        values.insert("rc_link_quality".into(), 255.0);
        values.insert("rc_valid".into(), 1.0);
        values.insert("imu_valid".into(), 1.0);

        let buf = codec.pack(&values);
        eprintln!("Packed {} bytes", buf.len());

        // Verify file identifier
        assert_eq!(&buf[4..8], b"C2SI", "file identifier mismatch");

        // Verify we can read back values
        // Root offset
        let root_off = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]) as usize;
        eprintln!("root_off = {}", root_off);

        // Check the buffer size matches expected (should be 120 bytes like the hand-coded version)
        eprintln!("Buffer size: {} (expected 120)", buf.len());
    }

    #[test]
    fn unpack_motor_output() {
        let Some(schema) = load_test_schema() else {
            eprintln!("skipping: bfbs files not found");
            return;
        };

        let mut route = HashMap::new();
        route.insert(
            "motors.m0".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m1".into(),
                scale: Some(1100.0),
            },
        );
        route.insert(
            "motors.m1".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m2".into(),
                scale: Some(1100.0),
            },
        );
        route.insert(
            "armed".into(),
            crate::config::RouteEntry::Simple("armed".into()),
        );

        let config = MessageConfig {
            root_type: "cerebri2.topic.MotorOutput".into(),
            route,
        };

        let codec = UnpackCodec::compile(&schema, &config).unwrap();
        eprintln!("UnpackCodec compiled with {} ops", codec.ops.len());
        for op in &codec.ops {
            eprintln!(
                "  {} → vtable_slot={} struct_off={} type={:?} scale={}",
                op.var, op.vtable_slot, op.struct_byte_offset, op.scalar_type, op.scale
            );
        }
    }

    #[test]
    fn roundtrip_pack_unpack() {
        let Some(schema) = load_test_schema() else {
            eprintln!("skipping: bfbs files not found");
            return;
        };

        // Pack a SimInput
        let mut pack_route = HashMap::new();
        pack_route.insert(
            "gyro.x".into(),
            crate::config::RouteEntry::Simple("gx".into()),
        );
        pack_route.insert(
            "gyro.y".into(),
            crate::config::RouteEntry::Simple("gy".into()),
        );
        pack_route.insert(
            "gyro.z".into(),
            crate::config::RouteEntry::Simple("gz".into()),
        );
        pack_route.insert(
            "rc_link_quality".into(),
            crate::config::RouteEntry::Simple("lq".into()),
        );
        pack_route.insert(
            "imu_valid".into(),
            crate::config::RouteEntry::Simple("imu_ok".into()),
        );

        let pack_config = MessageConfig {
            root_type: "cerebri2.sil.SimInput".into(),
            route: pack_route,
        };
        let packer = PackCodec::compile(&schema, &pack_config).unwrap();

        let mut values = HashMap::new();
        values.insert("gx".into(), 1.5);
        values.insert("gy".into(), -0.5);
        values.insert("gz".into(), 0.1);
        values.insert("lq".into(), 200.0);
        values.insert("imu_ok".into(), 1.0);

        let buf = packer.pack(&values);

        // Unpack it back
        let mut unpack_route = HashMap::new();
        unpack_route.insert(
            "gyro.x".into(),
            crate::config::RouteEntry::Simple("gx".into()),
        );
        unpack_route.insert(
            "gyro.y".into(),
            crate::config::RouteEntry::Simple("gy".into()),
        );
        unpack_route.insert(
            "gyro.z".into(),
            crate::config::RouteEntry::Simple("gz".into()),
        );
        unpack_route.insert(
            "rc_link_quality".into(),
            crate::config::RouteEntry::Simple("lq".into()),
        );
        unpack_route.insert(
            "imu_valid".into(),
            crate::config::RouteEntry::Simple("imu_ok".into()),
        );

        let unpack_config = MessageConfig {
            root_type: "cerebri2.sil.SimInput".into(),
            route: unpack_route,
        };
        let unpacker = UnpackCodec::compile(&schema, &unpack_config).unwrap();
        let result = unpacker.unpack(&buf);

        eprintln!("Roundtrip results: {:?}", result);

        // Check values (f32 precision)
        let eps = 1e-5;
        assert!(
            (result["gx"] - 1.5).abs() < eps,
            "gx mismatch: {}",
            result["gx"]
        );
        assert!(
            (result["gy"] - (-0.5)).abs() < eps,
            "gy mismatch: {}",
            result["gy"]
        );
        assert!(
            (result["gz"] - 0.1).abs() < eps,
            "gz mismatch: {}",
            result["gz"]
        );
        assert!(
            (result["lq"] - 200.0).abs() < eps,
            "lq mismatch: {}",
            result["lq"]
        );
        assert!(
            (result["imu_ok"] - 1.0).abs() < eps,
            "imu_ok mismatch: {}",
            result["imu_ok"]
        );
    }

    /// Byte-for-byte comparison against the old hand-coded pack_sim_input.
    #[test]
    #[allow(
        clippy::too_many_lines,
        clippy::needless_range_loop,
        clippy::excessive_nesting
    )]
    fn pack_sim_input_byte_exact() {
        let Some(schema) = load_test_schema() else {
            eprintln!("skipping: bfbs files not found");
            return;
        };

        let mut route = HashMap::new();
        route.insert(
            "gyro.x".into(),
            crate::config::RouteEntry::Simple("gyro_x".into()),
        );
        route.insert(
            "gyro.y".into(),
            crate::config::RouteEntry::Simple("gyro_y".into()),
        );
        route.insert(
            "gyro.z".into(),
            crate::config::RouteEntry::Simple("gyro_z".into()),
        );
        route.insert(
            "accel.x".into(),
            crate::config::RouteEntry::Simple("accel_x".into()),
        );
        route.insert(
            "accel.y".into(),
            crate::config::RouteEntry::Simple("accel_y".into()),
        );
        route.insert(
            "accel.z".into(),
            crate::config::RouteEntry::Simple("accel_z".into()),
        );
        for i in 0..16 {
            route.insert(
                format!("rc.ch{i}"),
                crate::config::RouteEntry::Simple(format!("rc_{i}")),
            );
        }
        route.insert(
            "rc_link_quality".into(),
            crate::config::RouteEntry::Simple("rc_link_quality".into()),
        );
        route.insert(
            "rc_valid".into(),
            crate::config::RouteEntry::Simple("rc_valid".into()),
        );
        route.insert(
            "imu_valid".into(),
            crate::config::RouteEntry::Simple("imu_valid".into()),
        );

        let config = MessageConfig {
            root_type: "cerebri2.sil.SimInput".into(),
            route,
        };

        let codec = PackCodec::compile(&schema, &config).unwrap();

        // Build the EXACT same values the old code would produce
        let gyro: [f32; 3] = [0.1, -0.2, 0.3];
        let accel: [f32; 3] = [0.5, -0.1, 9.81];
        let rc: [i32; 16] = [
            1500, 1500, 1000, 1500, 1000, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500,
            1500, 1500,
        ];
        let rc_link_quality: u8 = 255;
        let rc_valid: bool = true;
        let imu_valid: bool = true;

        // --- Old hand-coded packer (reconstructed) ---
        let mut expected = [0u8; 120];
        // Root offset
        expected[0..4].copy_from_slice(&24u32.to_le_bytes());
        // File identifier
        expected[4..8].copy_from_slice(b"C2SI");
        // Vtable at offset 8
        expected[8..10].copy_from_slice(&16u16.to_le_bytes()); // vtable size
        expected[10..12].copy_from_slice(&96u16.to_le_bytes()); // object size
        expected[12..14].copy_from_slice(&4u16.to_le_bytes()); // gyro field offset
        expected[14..16].copy_from_slice(&16u16.to_le_bytes()); // accel field offset
        expected[16..18].copy_from_slice(&28u16.to_le_bytes()); // rc field offset
        expected[18..20].copy_from_slice(&92u16.to_le_bytes()); // rc_link_quality
        expected[20..22].copy_from_slice(&93u16.to_le_bytes()); // rc_valid
        expected[22..24].copy_from_slice(&94u16.to_le_bytes()); // imu_valid
        // Table at offset 24: soffset back to vtable
        expected[24..28].copy_from_slice(&16u32.to_le_bytes()); // 24 - 8 = 16
        // Gyro at table+4 = 28
        for i in 0..3 {
            expected[28 + i * 4..32 + i * 4].copy_from_slice(&gyro[i].to_le_bytes());
        }
        // Accel at table+16 = 40
        for i in 0..3 {
            expected[40 + i * 4..44 + i * 4].copy_from_slice(&accel[i].to_le_bytes());
        }
        // RC at table+28 = 52
        for i in 0..16 {
            expected[52 + i * 4..56 + i * 4].copy_from_slice(&(rc[i] as u32).to_le_bytes());
        }
        // Scalars
        expected[116] = rc_link_quality;
        expected[117] = rc_valid as u8;
        expected[118] = imu_valid as u8;

        // --- New codec pack ---
        let mut values = HashMap::new();
        values.insert("gyro_x".into(), gyro[0] as f64);
        values.insert("gyro_y".into(), gyro[1] as f64);
        values.insert("gyro_z".into(), gyro[2] as f64);
        values.insert("accel_x".into(), accel[0] as f64);
        values.insert("accel_y".into(), accel[1] as f64);
        values.insert("accel_z".into(), accel[2] as f64);
        for (i, &val) in rc.iter().enumerate() {
            values.insert(format!("rc_{i}"), val as f64);
        }
        values.insert("rc_link_quality".into(), rc_link_quality as f64);
        values.insert("rc_valid".into(), rc_valid as u8 as f64);
        values.insert("imu_valid".into(), imu_valid as u8 as f64);

        let actual = codec.pack(&values);
        assert_eq!(actual.len(), 120, "size mismatch");

        // Compare byte-by-byte, reporting first difference
        for (i, (a, e)) in actual.iter().zip(expected.iter()).enumerate() {
            if a != e {
                eprintln!("MISMATCH at byte {i}: actual=0x{a:02x} expected=0x{e:02x}");
                let start = i.saturating_sub(4);
                let end = (i + 8).min(120);
                eprint!("  actual:   ");
                for (j, b) in actual[start..end].iter().enumerate() {
                    let mark = if j + start == i { ">" } else { " " };
                    eprint!("{mark}{b:02x} ");
                }
                eprintln!();
                eprint!("  expected: ");
                for (j, b) in expected[start..end].iter().enumerate() {
                    let mark = if j + start == i { ">" } else { " " };
                    eprint!("{mark}{b:02x} ");
                }
                eprintln!();
            }
        }

        assert_eq!(
            actual.as_slice(),
            &expected[..],
            "packed bytes do not match hand-coded output"
        );
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::bfbs;
    use std::net::UdpSocket;
    use std::path::PathBuf;
    use std::time::Duration;

    fn cerebri_bfbs_paths() -> Option<(PathBuf, PathBuf)> {
        let dir = std::env::var_os("RUMOCA_CEREBRI_BFBS_DIR")?;
        let dir = PathBuf::from(dir);
        Some((
            dir.join("cerebri2_topics.bfbs"),
            dir.join("cerebri2_sil.bfbs"),
        ))
    }

    /// Simulate the full cerebri ↔ rumoca loop locally.
    /// Sends a fake MotorOutput, verifies the sim can unpack it,
    /// and verifies the packed SimInput is valid.
    #[test]
    #[allow(clippy::too_many_lines)]
    fn full_loop_simulation() {
        let mut ss = bfbs::SchemaSet::new();
        let Some((topics, sil)) = cerebri_bfbs_paths() else {
            eprintln!("skipping: set RUMOCA_CEREBRI_BFBS_DIR to a directory with bfbs files");
            return;
        };
        if !topics.exists() || !sil.exists() {
            eprintln!("skipping: bfbs files not found");
            return;
        }
        ss.load_bfbs(&topics).unwrap();
        ss.load_bfbs(&sil).unwrap();

        // Build a MotorOutput packet the way cerebri does (48 bytes)
        // Using the pack codec to build a valid MotorOutput
        let mut motor_route = HashMap::new();
        motor_route.insert(
            "motors.m0".into(),
            crate::config::RouteEntry::Simple("m0".into()),
        );
        motor_route.insert(
            "motors.m1".into(),
            crate::config::RouteEntry::Simple("m1".into()),
        );
        motor_route.insert(
            "motors.m2".into(),
            crate::config::RouteEntry::Simple("m2".into()),
        );
        motor_route.insert(
            "motors.m3".into(),
            crate::config::RouteEntry::Simple("m3".into()),
        );
        motor_route.insert(
            "armed".into(),
            crate::config::RouteEntry::Simple("armed".into()),
        );
        motor_route.insert(
            "test_mode".into(),
            crate::config::RouteEntry::Simple("test_mode".into()),
        );
        // Also need raw fields to fill the full table
        motor_route.insert(
            "raw.m0".into(),
            crate::config::RouteEntry::Simple("r0".into()),
        );
        motor_route.insert(
            "raw.m1".into(),
            crate::config::RouteEntry::Simple("r1".into()),
        );
        motor_route.insert(
            "raw.m2".into(),
            crate::config::RouteEntry::Simple("r2".into()),
        );
        motor_route.insert(
            "raw.m3".into(),
            crate::config::RouteEntry::Simple("r3".into()),
        );

        let motor_pack_cfg = crate::config::MessageConfig {
            root_type: "cerebri2.topic.MotorOutput".into(),
            route: motor_route,
        };
        let motor_packer = PackCodec::compile(&ss, &motor_pack_cfg).unwrap();
        eprintln!("MotorOutput pack size: {} bytes", motor_packer.size());

        let mut motor_vals = HashMap::new();
        motor_vals.insert("m0".into(), 0.5);
        motor_vals.insert("m1".into(), 0.5);
        motor_vals.insert("m2".into(), 0.5);
        motor_vals.insert("m3".into(), 0.5);
        motor_vals.insert("armed".into(), 1.0);
        motor_vals.insert("test_mode".into(), 0.0);
        motor_vals.insert("r0".into(), 1500.0);
        motor_vals.insert("r1".into(), 1500.0);
        motor_vals.insert("r2".into(), 1500.0);
        motor_vals.insert("r3".into(), 1500.0);
        let motor_buf = motor_packer.pack(&motor_vals);
        eprintln!("Packed MotorOutput: {} bytes", motor_buf.len());
        eprint!("Hex: ");
        for b in &motor_buf {
            eprint!("{:02x} ", b);
        }
        eprintln!();

        // Now unpack it the way rumoca_sil does
        let mut recv_route = HashMap::new();
        recv_route.insert(
            "motors.m0".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m1".into(),
                scale: Some(1100.0),
            },
        );
        recv_route.insert(
            "motors.m1".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m2".into(),
                scale: Some(1100.0),
            },
        );
        recv_route.insert(
            "motors.m2".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m3".into(),
                scale: Some(1100.0),
            },
        );
        recv_route.insert(
            "motors.m3".into(),
            crate::config::RouteEntry::Full {
                var: "omega_m4".into(),
                scale: Some(1100.0),
            },
        );
        recv_route.insert(
            "armed".into(),
            crate::config::RouteEntry::Simple("armed".into()),
        );
        recv_route.insert(
            "test_mode".into(),
            crate::config::RouteEntry::Simple("test_mode".into()),
        );

        let recv_cfg = crate::config::MessageConfig {
            root_type: "cerebri2.topic.MotorOutput".into(),
            route: recv_route,
        };
        let unpacker = UnpackCodec::compile(&ss, &recv_cfg).unwrap();
        eprintln!("Expected receive size: {} bytes", unpacker.expected_size());

        let values = unpacker.unpack(&motor_buf);
        eprintln!("Unpacked values: {:?}", values);

        assert!(
            (values["omega_m1"] - 550.0).abs() < 0.1,
            "omega_m1 should be 0.5*1100=550"
        );
        assert!((values["omega_m2"] - 550.0).abs() < 0.1);
        assert!((values["armed"] - 1.0).abs() < 0.1, "should be armed");

        // Now test UDP roundtrip
        let listen = UdpSocket::bind("127.0.0.1:0").unwrap();
        let listen_addr = listen.local_addr().unwrap();
        listen
            .set_read_timeout(Some(Duration::from_millis(100)))
            .unwrap();

        let sender = UdpSocket::bind("127.0.0.1:0").unwrap();
        sender.send_to(&motor_buf, listen_addr).unwrap();

        let mut recv_buf = [0u8; 512];
        let (n, _) = listen.recv_from(&mut recv_buf).unwrap();
        assert_eq!(n, motor_buf.len());
        assert_eq!(n, unpacker.expected_size());

        let udp_values = unpacker.unpack(&recv_buf[..n]);
        assert!((udp_values["omega_m1"] - 550.0).abs() < 0.1);
        eprintln!("UDP roundtrip OK: {:?}", udp_values);
    }
}
