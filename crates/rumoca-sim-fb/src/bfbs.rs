//! Minimal .bfbs (Binary FlatBuffer Schema) parser.
//!
//! A .bfbs file is itself a FlatBuffer whose schema is `reflection.fbs`.
//! We parse it using raw flatbuffer binary traversal — no generated code
//! or external flatbuffers crate needed.

use std::collections::HashMap;
use std::path::Path;

// ── BaseType enum (from reflection.fbs) ──────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BaseType {
    None = 0,
    UType = 1,
    Bool = 2,
    Byte = 3,
    UByte = 4,
    Short = 5,
    UShort = 6,
    Int = 7,
    UInt = 8,
    Long = 9,
    ULong = 10,
    Float = 11,
    Double = 12,
    String = 13,
    Vector = 14,
    Obj = 15,
    Union = 16,
    Array = 17,
}

impl BaseType {
    fn from_u8(v: u8) -> Self {
        match v {
            0 => Self::None,
            1 => Self::UType,
            2 => Self::Bool,
            3 => Self::Byte,
            4 => Self::UByte,
            5 => Self::Short,
            6 => Self::UShort,
            7 => Self::Int,
            8 => Self::UInt,
            9 => Self::Long,
            10 => Self::ULong,
            11 => Self::Float,
            12 => Self::Double,
            13 => Self::String,
            14 => Self::Vector,
            15 => Self::Obj,
            16 => Self::Union,
            17 => Self::Array,
            _ => Self::None,
        }
    }

    /// Size in bytes of a scalar base type, or 0 for non-scalar.
    pub fn scalar_size(self) -> usize {
        match self {
            Self::Bool | Self::Byte | Self::UByte | Self::UType => 1,
            Self::Short | Self::UShort => 2,
            Self::Int | Self::UInt | Self::Float => 4,
            Self::Long | Self::ULong | Self::Double => 8,
            _ => 0,
        }
    }
}

// ── Parsed schema types ──────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct FieldType {
    pub base_type: BaseType,
    pub element: BaseType,
    /// Index into the schema's objects array (for Obj base_type).
    pub index: i32,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub field_type: FieldType,
    /// For tables: field id (vtable slot index, 0-based).
    /// For structs: byte offset within the struct.
    pub id: u16,
    pub offset: u16,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub name: String,
    pub fields: Vec<Field>,
    pub is_struct: bool,
    pub minalign: i32,
    pub bytesize: i32,
}

impl Object {
    pub fn field_by_name(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct Schema {
    pub objects: Vec<Object>,
    pub file_ident: Option<String>,
}

impl Schema {
    pub fn object_by_name(&self, name: &str) -> Option<&Object> {
        self.objects.iter().find(|o| o.name == name)
    }
}

// ── Merged schema set from multiple .bfbs files ──────────────────────────

#[derive(Debug, Clone)]
pub struct SchemaSet {
    pub objects: Vec<Object>,
    /// Map from object name to index in `objects`.
    name_to_idx: HashMap<String, usize>,
    /// File identifiers from each loaded schema.
    pub file_idents: Vec<Option<String>>,
}

impl Default for SchemaSet {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemaSet {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            name_to_idx: HashMap::new(),
            file_idents: Vec::new(),
        }
    }

    /// Load and merge a .bfbs file into the schema set.
    ///
    /// Field type indices from the .bfbs are remapped from the local schema's
    /// object array to the merged SchemaSet's object array.
    #[allow(clippy::excessive_nesting)]
    pub fn load_bfbs(&mut self, path: &Path) -> anyhow::Result<()> {
        let data = std::fs::read(path)?;
        let schema = parse_bfbs(&data)?;
        self.file_idents.push(schema.file_ident);

        // First pass: add all objects (dedup by name), recording local→global index map
        let mut local_to_global: Vec<usize> = Vec::with_capacity(schema.objects.len());
        let mut newly_added: Vec<usize> = Vec::new();
        for obj in &schema.objects {
            if let Some(&existing) = self.name_to_idx.get(&obj.name) {
                local_to_global.push(existing);
            } else {
                let idx = self.objects.len();
                self.name_to_idx.insert(obj.name.clone(), idx);
                self.objects.push(obj.clone());
                local_to_global.push(idx);
                newly_added.push(idx);
            }
        }

        // Second pass: remap field type indices ONLY for newly added objects.
        // Existing objects already have correct global indices from their original load.
        for &global_idx in &newly_added {
            let obj = &mut self.objects[global_idx];
            for field in &mut obj.fields {
                if field.field_type.base_type == BaseType::Obj && field.field_type.index >= 0 {
                    let local_idx = field.field_type.index as usize;
                    if local_idx < local_to_global.len() {
                        field.field_type.index = local_to_global[local_idx] as i32;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn object_by_name(&self, name: &str) -> Option<&Object> {
        self.name_to_idx.get(name).map(|&i| &self.objects[i])
    }

    /// Resolve a field type's object index to an Object (for Obj fields).
    /// The index refers to the object's position within the *original* .bfbs
    /// objects vector. Since we merge schemas, we look up by name instead
    /// when resolving nested types during codec compilation.
    pub fn object_by_index_in(&self, schema_objects: &[Object], index: i32) -> Option<&Object> {
        let name = schema_objects
            .get(index as usize)
            .map(|o| o.name.as_str())?;
        self.object_by_name(name)
    }
}

// ── Low-level FlatBuffer binary helpers ──────────────────────────────────

fn read_u8(buf: &[u8], off: usize) -> u8 {
    buf[off]
}

fn read_u16(buf: &[u8], off: usize) -> u16 {
    u16::from_le_bytes([buf[off], buf[off + 1]])
}

fn read_i32(buf: &[u8], off: usize) -> i32 {
    i32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}

fn read_u32(buf: &[u8], off: usize) -> u32 {
    u32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}

/// Follow a 32-bit relative offset at `off` to get the pointed-to position.
fn follow_offset(buf: &[u8], off: usize) -> usize {
    off + read_u32(buf, off) as usize
}

/// Get the vtable location for a table at `table_off`.
fn vtable_of(buf: &[u8], table_off: usize) -> usize {
    // The table stores a negative (signed) offset back to its vtable.
    let soff = read_i32(buf, table_off);
    ((table_off as i64) - (soff as i64)) as usize
}

/// Read a field offset from a vtable. Returns 0 if the field is absent.
/// `field_index` is the 0-based field id.
fn vtable_field_offset(buf: &[u8], vtable: usize, field_index: usize) -> u16 {
    let vtable_size = read_u16(buf, vtable) as usize;
    let slot = 4 + field_index * 2; // 4 = sizeof(vtable_size) + sizeof(object_size)
    if slot < vtable_size {
        read_u16(buf, vtable + slot)
    } else {
        0
    }
}

/// Read a string at the given offset (offset points to the string's length prefix).
fn read_string(buf: &[u8], off: usize) -> String {
    let len = read_u32(buf, off) as usize;
    String::from_utf8_lossy(&buf[off + 4..off + 4 + len]).into_owned()
}

/// Read a table field that is a string (indirect offset → string).
fn table_string(buf: &[u8], table_off: usize, vtable: usize, field_index: usize) -> Option<String> {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return None;
    }
    let abs = table_off + foff as usize;
    let str_off = follow_offset(buf, abs);
    Some(read_string(buf, str_off))
}

/// Read a table field that is a bool (u8).
fn table_bool(buf: &[u8], table_off: usize, vtable: usize, field_index: usize) -> bool {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return false;
    }
    read_u8(buf, table_off + foff as usize) != 0
}

/// Read a table field that is an i32.
fn table_i32(buf: &[u8], table_off: usize, vtable: usize, field_index: usize) -> i32 {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return 0;
    }
    read_i32(buf, table_off + foff as usize)
}

/// Read a table field that is a u16.
fn table_u16(buf: &[u8], table_off: usize, vtable: usize, field_index: usize) -> u16 {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return 0;
    }
    read_u16(buf, table_off + foff as usize)
}

/// Read a table field that is a vector of table offsets.
/// Returns (element_count, data_start) where data_start points to the first offset.
fn table_vector(
    buf: &[u8],
    table_off: usize,
    vtable: usize,
    field_index: usize,
) -> Option<(usize, usize)> {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return None;
    }
    let vec_off = follow_offset(buf, table_off + foff as usize);
    let count = read_u32(buf, vec_off) as usize;
    Some((count, vec_off + 4))
}

/// Follow an offset in a vector of tables to get the table position.
fn vector_table_at(buf: &[u8], data_start: usize, index: usize) -> usize {
    let off = data_start + index * 4;
    follow_offset(buf, off)
}

/// Read a table field that is a nested table (indirect offset → table).
fn table_table(buf: &[u8], table_off: usize, vtable: usize, field_index: usize) -> Option<usize> {
    let foff = vtable_field_offset(buf, vtable, field_index);
    if foff == 0 {
        return None;
    }
    Some(follow_offset(buf, table_off + foff as usize))
}

// ── .bfbs parsing ────────────────────────────────────────────────────────

// reflection.fbs field indices for the Schema table
const SCHEMA_OBJECTS: usize = 0;
const SCHEMA_FILE_IDENT: usize = 2;

// reflection.fbs field indices for the Object table
const OBJECT_NAME: usize = 0;
const OBJECT_FIELDS: usize = 1;
const OBJECT_IS_STRUCT: usize = 2;
const OBJECT_MINALIGN: usize = 3;
const OBJECT_BYTESIZE: usize = 4;

// reflection.fbs field indices for the Field table
const FIELD_NAME: usize = 0;
const FIELD_TYPE: usize = 1;
const FIELD_ID: usize = 2;
const FIELD_OFFSET: usize = 3;

// reflection.fbs field indices for the Type table
const TYPE_BASE_TYPE: usize = 0;
const TYPE_ELEMENT: usize = 1;
const TYPE_INDEX: usize = 2;

fn parse_type(buf: &[u8], type_off: usize) -> FieldType {
    let vt = vtable_of(buf, type_off);
    FieldType {
        base_type: BaseType::from_u8({
            let foff = vtable_field_offset(buf, vt, TYPE_BASE_TYPE);
            if foff == 0 {
                0
            } else {
                read_u8(buf, type_off + foff as usize)
            }
        }),
        element: BaseType::from_u8({
            let foff = vtable_field_offset(buf, vt, TYPE_ELEMENT);
            if foff == 0 {
                0
            } else {
                read_u8(buf, type_off + foff as usize)
            }
        }),
        index: table_i32(buf, type_off, vt, TYPE_INDEX),
    }
}

fn parse_field(buf: &[u8], field_off: usize) -> Field {
    let vt = vtable_of(buf, field_off);
    let name = table_string(buf, field_off, vt, FIELD_NAME).unwrap_or_default();
    let field_type = match table_table(buf, field_off, vt, FIELD_TYPE) {
        Some(type_off) => parse_type(buf, type_off),
        None => FieldType {
            base_type: BaseType::None,
            element: BaseType::None,
            index: -1,
        },
    };
    let id = table_u16(buf, field_off, vt, FIELD_ID);
    let offset = table_u16(buf, field_off, vt, FIELD_OFFSET);
    Field {
        name,
        field_type,
        id,
        offset,
    }
}

fn parse_object(buf: &[u8], obj_off: usize) -> Object {
    let vt = vtable_of(buf, obj_off);
    let name = table_string(buf, obj_off, vt, OBJECT_NAME).unwrap_or_default();
    let is_struct = table_bool(buf, obj_off, vt, OBJECT_IS_STRUCT);
    let minalign = table_i32(buf, obj_off, vt, OBJECT_MINALIGN);
    let bytesize = table_i32(buf, obj_off, vt, OBJECT_BYTESIZE);

    let mut fields = Vec::new();
    if let Some((count, data)) = table_vector(buf, obj_off, vt, OBJECT_FIELDS) {
        for i in 0..count {
            let field_off = vector_table_at(buf, data, i);
            fields.push(parse_field(buf, field_off));
        }
    }

    Object {
        name,
        fields,
        is_struct,
        minalign,
        bytesize,
    }
}

/// Parse a .bfbs buffer into a Schema.
pub fn parse_bfbs(buf: &[u8]) -> anyhow::Result<Schema> {
    if buf.len() < 8 {
        anyhow::bail!("bfbs too small");
    }
    // Verify file identifier
    if &buf[4..8] != b"BFBS" {
        anyhow::bail!("not a BFBS file (expected 'BFBS' identifier at offset 4)");
    }

    let root_off = follow_offset(buf, 0);
    let vt = vtable_of(buf, root_off);

    let file_ident = table_string(buf, root_off, vt, SCHEMA_FILE_IDENT);

    let mut objects = Vec::new();
    if let Some((count, data)) = table_vector(buf, root_off, vt, SCHEMA_OBJECTS) {
        for i in 0..count {
            let obj_off = vector_table_at(buf, data, i);
            objects.push(parse_object(buf, obj_off));
        }
    }

    Ok(Schema {
        objects,
        file_ident,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn cerebri_bfbs_path(file_name: &str) -> Option<PathBuf> {
        let dir = std::env::var_os("RUMOCA_CEREBRI_BFBS_DIR")?;
        Some(PathBuf::from(dir).join(file_name))
    }

    #[test]
    fn parse_cerebri2_topics() {
        let Some(path) = cerebri_bfbs_path("cerebri2_topics.bfbs") else {
            eprintln!("skipping: set RUMOCA_CEREBRI_BFBS_DIR to a directory with bfbs files");
            return;
        };
        if !path.exists() {
            eprintln!("skipping test: bfbs not found");
            return;
        }
        let data = std::fs::read(&path).unwrap();
        let schema = parse_bfbs(&data).unwrap();

        eprintln!("Objects:");
        for obj in &schema.objects {
            eprintln!(
                "  {} (struct={}, size={}, align={})",
                obj.name, obj.is_struct, obj.bytesize, obj.minalign
            );
            for f in &obj.fields {
                eprintln!(
                    "    {} id={} offset={} type={:?} index={}",
                    f.name, f.id, f.offset, f.field_type.base_type, f.field_type.index
                );
            }
        }

        // Vec3f should exist with 3 float fields
        let vec3f = schema
            .object_by_name("cerebri2.topic.Vec3f")
            .expect("Vec3f not found");
        assert!(vec3f.is_struct);
        assert_eq!(vec3f.fields.len(), 3);
        assert_eq!(vec3f.bytesize, 12);

        // MotorOutput should be a table
        let motor = schema
            .object_by_name("cerebri2.topic.MotorOutput")
            .expect("MotorOutput not found");
        assert!(!motor.is_struct);
        assert!(motor.field_by_name("armed").is_some());
    }

    #[test]
    fn parse_cerebri2_sil() {
        let Some(path) = cerebri_bfbs_path("cerebri2_sil.bfbs") else {
            eprintln!("skipping: set RUMOCA_CEREBRI_BFBS_DIR to a directory with bfbs files");
            return;
        };
        if !path.exists() {
            eprintln!("skipping test: bfbs not found");
            return;
        }
        let data = std::fs::read(&path).unwrap();
        let schema = parse_bfbs(&data).unwrap();

        eprintln!("file_ident: {:?}", schema.file_ident);
        eprintln!("Objects:");
        for obj in &schema.objects {
            eprintln!(
                "  {} (struct={}, size={}, align={})",
                obj.name, obj.is_struct, obj.bytesize, obj.minalign
            );
            for f in &obj.fields {
                eprintln!(
                    "    {} id={} offset={} type={:?} index={}",
                    f.name, f.id, f.offset, f.field_type.base_type, f.field_type.index
                );
            }
        }

        let sim = schema
            .object_by_name("cerebri2.sil.SimInput")
            .expect("SimInput not found");
        assert!(!sim.is_struct);
        assert!(sim.field_by_name("gyro").is_some());
        assert_eq!(schema.file_ident.as_deref(), Some("C2SI"));
    }
}
