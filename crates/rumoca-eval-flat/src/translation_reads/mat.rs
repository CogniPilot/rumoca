//! MATLAB MAT-file matrices, as `ModelicaIO.c` reads them through MATIO.
//!
//! A file is level 5 (MAT v6, and v7 with zlib-compressed elements) when its
//! 128-byte header carries version `0x0100` and an `MI`/`IM` endian
//! indicator, and level 4 otherwise. Version `0x0200` (v7.3) is HDF5, which
//! the reader refuses. A variable is found by its exact name; it must be a
//! rank-2, real, numeric array, and its stored element type converts to
//! binary64 (MATLAB stores integer-valued doubles as smaller integer types).

use std::io::Read;

/// One rank-2 real numeric MAT variable.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MatMatrix {
    pub(crate) rows: usize,
    pub(crate) cols: usize,
    /// The elements in the file's column-major order.
    pub(crate) column_major: Vec<f64>,
}

/// Why a variable could not be read, in the terms `ModelicaIO.c` reports.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MatError {
    /// No variable of the name is stored.
    NotFound,
    /// The variable is not a rank-2 array.
    NotRank2,
    /// The variable is not of a numeric class.
    NotNumeric,
    /// The variable has an imaginary part.
    Complex,
    /// The file is a v7.3 (HDF5) MAT file.
    Hdf5,
    /// The file structure is malformed.
    Malformed,
}

const HEADER_LEN: usize = 128;

/// Read the variable `name` from the bytes of a MAT file.
pub(crate) fn read_matrix(bytes: &[u8], name: &str) -> Result<MatMatrix, MatError> {
    match level5_endian(bytes) {
        Some(Level5::V5(endian)) => read_level5(&bytes[HEADER_LEN..], endian, name),
        Some(Level5::Hdf5) => Err(MatError::Hdf5),
        None => read_level4(bytes, name),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Endian {
    Little,
    Big,
}

impl Endian {
    fn u16(self, bytes: [u8; 2]) -> u16 {
        match self {
            Self::Little => u16::from_le_bytes(bytes),
            Self::Big => u16::from_be_bytes(bytes),
        }
    }

    fn u32(self, bytes: &[u8]) -> Option<u32> {
        let bytes: [u8; 4] = bytes.get(..4)?.try_into().ok()?;
        Some(match self {
            Self::Little => u32::from_le_bytes(bytes),
            Self::Big => u32::from_be_bytes(bytes),
        })
    }

    fn array<const N: usize>(self, bytes: &[u8]) -> [u8; N] {
        let mut array: [u8; N] = bytes.try_into().expect("caller slices exactly N bytes");
        if self == Self::Big {
            array.reverse();
        }
        array
    }
}

enum Level5 {
    V5(Endian),
    Hdf5,
}

fn level5_endian(bytes: &[u8]) -> Option<Level5> {
    let header = bytes.get(..HEADER_LEN)?;
    // The writer stores the characters `M`, `I` as one 16-bit value, so a
    // little-endian file holds the bytes `I`, `M`.
    let endian = match &header[126..128] {
        b"IM" => Endian::Little,
        b"MI" => Endian::Big,
        _ => return None,
    };
    match endian.u16([header[124], header[125]]) {
        0x0100 => Some(Level5::V5(endian)),
        0x0200 => Some(Level5::Hdf5),
        _ => None,
    }
}

// MAT level 5 data types.
const MI_INT8: u32 = 1;
const MI_UINT8: u32 = 2;
const MI_INT16: u32 = 3;
const MI_UINT16: u32 = 4;
const MI_INT32: u32 = 5;
const MI_UINT32: u32 = 6;
const MI_SINGLE: u32 = 7;
const MI_DOUBLE: u32 = 9;
const MI_INT64: u32 = 12;
const MI_UINT64: u32 = 13;
const MI_MATRIX: u32 = 14;
const MI_COMPRESSED: u32 = 15;

// MAT level 5 array classes: double, single, and the integer classes.
const NUMERIC_CLASSES: std::ops::RangeInclusive<u32> = 6..=15;
const COMPLEX_FLAG: u32 = 0x0800;

/// One data element: its type and the bytes it holds.
struct Element<'a> {
    data_type: u32,
    data: &'a [u8],
}

/// Split the next data element off `bytes`, returning it and the rest.
fn next_element(bytes: &[u8], endian: Endian) -> Result<(Element<'_>, &[u8]), MatError> {
    let tag = endian.u32(bytes).ok_or(MatError::Malformed)?;
    if tag >> 16 != 0 {
        // Small data element: size and type share the first word, and the
        // data occupies the second.
        let size = (tag >> 16) as usize;
        let data = bytes.get(4..4 + size).ok_or(MatError::Malformed)?;
        let rest = bytes.get(8..).ok_or(MatError::Malformed)?;
        return Ok((
            Element {
                data_type: tag & 0xffff,
                data,
            },
            rest,
        ));
    }
    let size = endian.u32(&bytes[4..]).ok_or(MatError::Malformed)? as usize;
    let data = bytes.get(8..8 + size).ok_or(MatError::Malformed)?;
    // Compressed elements are stored unpadded; every other element is padded
    // to a multiple of eight bytes.
    let stored = if tag == MI_COMPRESSED {
        size
    } else {
        size.next_multiple_of(8)
    };
    let rest = bytes.get((8 + stored).min(bytes.len())..).unwrap_or(&[]);
    Ok((
        Element {
            data_type: tag,
            data,
        },
        rest,
    ))
}

fn read_level5(mut bytes: &[u8], endian: Endian, name: &str) -> Result<MatMatrix, MatError> {
    while !bytes.is_empty() {
        let (element, rest) = next_element(bytes, endian)?;
        bytes = rest;
        let found = match element.data_type {
            MI_MATRIX => level5_matrix(element.data, endian, name)?,
            MI_COMPRESSED => {
                let mut inflated = Vec::new();
                flate2::read::ZlibDecoder::new(element.data)
                    .read_to_end(&mut inflated)
                    .map_err(|_| MatError::Malformed)?;
                let (inner, _) = next_element(&inflated, endian)?;
                if inner.data_type == MI_MATRIX {
                    level5_matrix(inner.data, endian, name)?
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(matrix) = found {
            return Ok(matrix);
        }
    }
    Err(MatError::NotFound)
}

/// The matrix an `miMATRIX` element holds when it is named `name`.
fn level5_matrix(data: &[u8], endian: Endian, name: &str) -> Result<Option<MatMatrix>, MatError> {
    if data.is_empty() {
        return Ok(None);
    }
    let (flags, rest) = next_element(data, endian)?;
    let (dims, rest) = next_element(rest, endian)?;
    let (stored_name, rest) = next_element(rest, endian)?;
    if stored_name.data != name.as_bytes() {
        return Ok(None);
    }
    let flags = endian.u32(flags.data).ok_or(MatError::Malformed)?;
    let rank = dims.data.len() / 4;
    if rank != 2 {
        return Err(MatError::NotRank2);
    }
    if !NUMERIC_CLASSES.contains(&(flags & 0xff)) {
        return Err(MatError::NotNumeric);
    }
    if flags & COMPLEX_FLAG != 0 {
        return Err(MatError::Complex);
    }
    let extent = |offset: usize| {
        endian
            .u32(&dims.data[offset..])
            .map(|value| value as usize)
            .ok_or(MatError::Malformed)
    };
    let (rows, cols) = (extent(0)?, extent(4)?);
    let (real, _) = next_element(rest, endian)?;
    let column_major = level5_values(&real, endian)?;
    if Some(column_major.len()) != rows.checked_mul(cols) {
        return Err(MatError::Malformed);
    }
    Ok(Some(MatMatrix {
        rows,
        cols,
        column_major,
    }))
}

fn level5_values(element: &Element<'_>, endian: Endian) -> Result<Vec<f64>, MatError> {
    let width = match element.data_type {
        MI_INT8 | MI_UINT8 => 1,
        MI_INT16 | MI_UINT16 => 2,
        MI_INT32 | MI_UINT32 | MI_SINGLE => 4,
        MI_DOUBLE | MI_INT64 | MI_UINT64 => 8,
        _ => return Err(MatError::Malformed),
    };
    if !element.data.len().is_multiple_of(width) {
        return Err(MatError::Malformed);
    }
    Ok(element
        .data
        .chunks_exact(width)
        .map(|chunk| match element.data_type {
            MI_INT8 => f64::from(chunk[0] as i8),
            MI_UINT8 => f64::from(chunk[0]),
            MI_INT16 => f64::from(i16::from_le_bytes(endian.array(chunk))),
            MI_UINT16 => f64::from(u16::from_le_bytes(endian.array(chunk))),
            MI_INT32 => f64::from(i32::from_le_bytes(endian.array(chunk))),
            MI_UINT32 => f64::from(u32::from_le_bytes(endian.array(chunk))),
            MI_SINGLE => f64::from(f32::from_le_bytes(endian.array(chunk))),
            MI_INT64 => i64::from_le_bytes(endian.array(chunk)) as f64,
            MI_UINT64 => u64::from_le_bytes(endian.array(chunk)) as f64,
            _ => f64::from_le_bytes(endian.array(chunk)),
        })
        .collect())
}

/// One level 4 matrix header: `MOPT` type, extents, imaginary flag, and the
/// name length including its terminating NUL.
struct Level4Header {
    endian: Endian,
    precision: u32,
    text_or_sparse: bool,
    rows: usize,
    cols: usize,
    complex: bool,
    name_len: usize,
}

const LEVEL4_HEADER_LEN: usize = 20;

fn level4_header(bytes: &[u8]) -> Result<Level4Header, MatError> {
    let words = bytes.get(..LEVEL4_HEADER_LEN).ok_or(MatError::Malformed)?;
    // The `M` digit of `MOPT` names the byte order; a type read in the wrong
    // order is not a valid `MOPT` value.
    let endian = [Endian::Little, Endian::Big]
        .into_iter()
        .find(|endian| {
            endian
                .u32(words)
                .is_some_and(|mopt| mopt < 2000 && (mopt / 100) % 10 == 0)
        })
        .ok_or(MatError::Malformed)?;
    let word = |index: usize| {
        endian
            .u32(&words[index * 4..])
            .map(|value| value as usize)
            .ok_or(MatError::Malformed)
    };
    let mopt = word(0)?;
    let precision = (mopt / 10 % 10) as u32;
    if precision > 5 {
        return Err(MatError::Malformed);
    }
    Ok(Level4Header {
        endian,
        precision,
        text_or_sparse: mopt % 10 != 0,
        rows: word(1)?,
        cols: word(2)?,
        complex: word(3)? != 0,
        name_len: word(4)?,
    })
}

fn read_level4(mut bytes: &[u8], name: &str) -> Result<MatMatrix, MatError> {
    while !bytes.is_empty() {
        let header = level4_header(bytes)?;
        let name_start = LEVEL4_HEADER_LEN;
        // Every extent is read from the file, so each offset is checked
        // arithmetic: a header whose sizes overflow is malformed.
        let data_start = name_start
            .checked_add(header.name_len)
            .ok_or(MatError::Malformed)?;
        let stored_name = bytes
            .get(name_start..data_start)
            .ok_or(MatError::Malformed)?;
        let stored_name = stored_name.split(|byte| *byte == 0).next().unwrap_or(&[]);
        let width = [8, 4, 4, 2, 2, 1][header.precision as usize];
        let count = header
            .rows
            .checked_mul(header.cols)
            .ok_or(MatError::Malformed)?;
        let real_len = count.checked_mul(width).ok_or(MatError::Malformed)?;
        let data_len = real_len
            .checked_mul(if header.complex { 2 } else { 1 })
            .ok_or(MatError::Malformed)?;
        let data_end = data_start
            .checked_add(data_len)
            .ok_or(MatError::Malformed)?;
        let data = bytes.get(data_start..data_end).ok_or(MatError::Malformed)?;
        if stored_name == name.as_bytes() {
            if header.text_or_sparse {
                return Err(MatError::NotNumeric);
            }
            if header.complex {
                return Err(MatError::Complex);
            }
            let endian = header.endian;
            let column_major = data[..real_len]
                .chunks_exact(width)
                .map(|chunk| match header.precision {
                    0 => f64::from_le_bytes(endian.array(chunk)),
                    1 => f64::from(f32::from_le_bytes(endian.array(chunk))),
                    2 => f64::from(i32::from_le_bytes(endian.array(chunk))),
                    3 => f64::from(i16::from_le_bytes(endian.array(chunk))),
                    4 => f64::from(u16::from_le_bytes(endian.array(chunk))),
                    _ => f64::from(chunk[0]),
                })
                .collect();
            return Ok(MatMatrix {
                rows: header.rows,
                cols: header.cols,
                column_major,
            });
        }
        bytes = &bytes[data_end..];
    }
    Err(MatError::NotFound)
}

#[cfg(test)]
mod tests;
