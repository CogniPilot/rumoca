use std::{
    io::Read,
    path::{Path, PathBuf},
};

pub(super) fn read_mat_matrix_size(file_name: &str, matrix_name: &str) -> Option<(i64, i64)> {
    let path = resolve_modelica_resource_path(file_name)?;
    let bytes = std::fs::read(path).ok()?;
    read_mat_v4_matrix_size(&bytes, matrix_name)
        .or_else(|| read_mat_v5_matrix_size(&bytes, matrix_name))
}

pub(super) fn resolve_modelica_resource_path(raw: &str) -> Option<PathBuf> {
    let raw_path = Path::new(raw);
    if raw_path.is_file() {
        return Some(raw_path.to_path_buf());
    }
    let rest = raw.strip_prefix("modelica://Modelica/")?;
    modelica_source_roots()
        .into_iter()
        .map(|root| root.join(rest))
        .find(|candidate| candidate.is_file())
}

fn modelica_source_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();
    for msl_base in modelica_cache_dirs() {
        let Ok(msl_entries) = std::fs::read_dir(msl_base) else {
            continue;
        };
        for msl_entry in msl_entries.flatten() {
            let msl_path = msl_entry.path();
            let Some(msl_name) = msl_path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            if !msl_name.starts_with("ModelicaStandardLibrary-") {
                continue;
            }
            let Ok(version_entries) = std::fs::read_dir(&msl_path) else {
                continue;
            };
            for version_entry in version_entries.flatten() {
                let version_path = version_entry.path();
                let Some(version_name) = version_path.file_name().and_then(|name| name.to_str())
                else {
                    continue;
                };
                if version_name.starts_with("Modelica ") && version_path.is_dir() {
                    roots.push(version_path);
                }
            }
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

fn modelica_cache_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let Ok(current_dir) = std::env::current_dir() else {
        return dirs;
    };
    for ancestor in current_dir.ancestors() {
        let candidate = ancestor.join("target").join("msl");
        if candidate.is_dir() {
            dirs.push(candidate);
        }
    }
    dirs
}

fn read_mat_v4_matrix_size(bytes: &[u8], matrix_name: &str) -> Option<(i64, i64)> {
    let mut offset = 0usize;
    while offset.checked_add(20)? <= bytes.len() {
        let mopt = read_i32_le(bytes, offset)?;
        let rows = read_i32_le(bytes, offset + 4)?;
        let cols = read_i32_le(bytes, offset + 8)?;
        let imagf = read_i32_le(bytes, offset + 12)?;
        let name_len = read_i32_le(bytes, offset + 16)?;
        if rows <= 0 || cols <= 0 || name_len <= 0 || imagf < 0 {
            return None;
        }
        let name_start = offset + 20;
        let name_end = name_start.checked_add(name_len as usize)?;
        if name_end > bytes.len() {
            return None;
        }
        let raw_name = &bytes[name_start..name_end];
        let name = std::str::from_utf8(raw_name.strip_suffix(&[0]).unwrap_or(raw_name)).ok()?;
        if name == matrix_name {
            return Some((i64::from(rows), i64::from(cols)));
        }
        let value_size = mat_v4_value_size(mopt)?;
        let value_count = (rows as usize)
            .checked_mul(cols as usize)?
            .checked_mul(if imagf == 0 { 1 } else { 2 })?;
        offset = name_end.checked_add(value_count.checked_mul(value_size)?)?;
    }
    None
}

fn mat_v4_value_size(mopt: i32) -> Option<usize> {
    match (mopt / 10) % 10 {
        0 => Some(8),
        1 | 2 => Some(4),
        3 | 4 => Some(2),
        5 => Some(1),
        _ => None,
    }
}

fn read_mat_v5_matrix_size(bytes: &[u8], matrix_name: &str) -> Option<(i64, i64)> {
    if bytes.len() < 128 {
        return None;
    }
    read_mat_v5_elements(bytes, 128, matrix_name)
}

fn read_mat_v5_elements(bytes: &[u8], start: usize, matrix_name: &str) -> Option<(i64, i64)> {
    let mut offset = start;
    while offset.checked_add(8)? <= bytes.len() {
        let tag = read_mat_v5_tag(bytes, offset)?;
        match tag.data_type {
            14 => {
                if let Some(size) = read_mat_v5_matrix_element(bytes, tag.payload, matrix_name) {
                    return Some(size);
                }
            }
            15 => {
                let mut decoded = Vec::new();
                flate2::read::ZlibDecoder::new(&bytes[tag.payload.clone()])
                    .read_to_end(&mut decoded)
                    .ok()?;
                if let Some(size) = read_mat_v5_elements(&decoded, 0, matrix_name) {
                    return Some(size);
                }
            }
            _ => {}
        }
        offset = offset.checked_add(tag.total_size)?;
    }
    None
}

fn read_mat_v5_matrix_element(
    bytes: &[u8],
    payload: std::ops::Range<usize>,
    matrix_name: &str,
) -> Option<(i64, i64)> {
    let mut offset = payload.start;
    let end = payload.end;
    let flags = read_mat_v5_tag(bytes, offset)?;
    offset = offset.checked_add(flags.total_size)?;
    let dims_tag = read_mat_v5_tag(bytes, offset)?;
    offset = offset.checked_add(dims_tag.total_size)?;
    let name_tag = read_mat_v5_tag(bytes, offset)?;
    let dims = read_i32_values_le(&bytes[dims_tag.payload], 2)?;
    let name = std::str::from_utf8(&bytes[name_tag.payload]).ok()?;
    (offset <= end && name == matrix_name).then_some((i64::from(dims[0]), i64::from(dims[1])))
}

struct MatV5Tag {
    data_type: u32,
    payload: std::ops::Range<usize>,
    total_size: usize,
}

fn read_mat_v5_tag(bytes: &[u8], offset: usize) -> Option<MatV5Tag> {
    let word = read_u32_le(bytes, offset)?;
    let small_bytes = word >> 16;
    if small_bytes != 0 {
        let data_type = word & 0xffff;
        let payload_start = offset.checked_add(4)?;
        let payload_end = payload_start.checked_add(small_bytes as usize)?;
        return (payload_end <= offset.checked_add(8)? && payload_end <= bytes.len()).then_some(
            MatV5Tag {
                data_type,
                payload: payload_start..payload_end,
                total_size: 8,
            },
        );
    }
    let byte_count = read_u32_le(bytes, offset + 4)? as usize;
    let payload_start = offset.checked_add(8)?;
    let payload_end = payload_start.checked_add(byte_count)?;
    let total_size = 8usize.checked_add(pad_to_eight(byte_count)?)?;
    (payload_end <= bytes.len()).then_some(MatV5Tag {
        data_type: word,
        payload: payload_start..payload_end,
        total_size,
    })
}

fn pad_to_eight(value: usize) -> Option<usize> {
    value.checked_add(7).map(|v| v & !7)
}

fn read_i32_values_le(bytes: &[u8], count: usize) -> Option<Vec<i32>> {
    (0..count).map(|i| read_i32_le(bytes, i * 4)).collect()
}

fn read_i32_le(bytes: &[u8], offset: usize) -> Option<i32> {
    read_u32_le(bytes, offset).map(|value| value as i32)
}

fn read_u32_le(bytes: &[u8], offset: usize) -> Option<u32> {
    let slice = bytes.get(offset..offset.checked_add(4)?)?;
    Some(u32::from_le_bytes(slice.try_into().ok()?))
}
