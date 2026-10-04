use std::io::Write;

use super::*;

const NAME: &str = "Matrix_A";
/// `[11 12; 21 22; 31 32]` in column-major order.
const COLUMN_MAJOR: [f64; 6] = [11.0, 21.0, 31.0, 12.0, 22.0, 32.0];

fn expected() -> MatMatrix {
    MatMatrix {
        rows: 3,
        cols: 2,
        column_major: COLUMN_MAJOR.to_vec(),
    }
}

fn level4(name: &str, mopt: u32, big_endian: bool, complex: bool) -> Vec<u8> {
    let word = |value: u32| {
        if big_endian {
            value.to_be_bytes()
        } else {
            value.to_le_bytes()
        }
    };
    let mut bytes = Vec::new();
    let name_len = u32::try_from(name.len() + 1).expect("short name");
    for value in [mopt, 3, 2, u32::from(complex), name_len] {
        bytes.extend(word(value));
    }
    bytes.extend(name.as_bytes());
    bytes.push(0);
    let parts = if complex { 2 } else { 1 };
    for _ in 0..parts {
        for value in COLUMN_MAJOR {
            bytes.extend(if big_endian {
                value.to_be_bytes()
            } else {
                value.to_le_bytes()
            });
        }
    }
    bytes
}

fn element(data_type: u32, data: &[u8]) -> Vec<u8> {
    let mut bytes = Vec::new();
    bytes.extend(data_type.to_le_bytes());
    bytes.extend(
        u32::try_from(data.len())
            .expect("short element")
            .to_le_bytes(),
    );
    bytes.extend(data);
    bytes.resize(bytes.len().next_multiple_of(8), 0);
    bytes
}

fn small_element(data_type: u32, data: &[u8]) -> Vec<u8> {
    assert!(data.len() <= 4);
    let mut bytes = Vec::new();
    let size = u32::try_from(data.len()).expect("small element");
    bytes.extend(((size << 16) | data_type).to_le_bytes());
    bytes.extend(data);
    bytes.resize(8, 0);
    bytes
}

fn matrix_element(name: &str, class: u32, dims: &[u32], real: Vec<u8>) -> Vec<u8> {
    let mut data = element(
        MI_UINT32,
        &[class.to_le_bytes(), 0_u32.to_le_bytes()].concat(),
    );
    let dims = dims
        .iter()
        .flat_map(|dim| dim.to_le_bytes())
        .collect::<Vec<_>>();
    data.extend(element(MI_INT32, &dims));
    if name.len() <= 4 {
        data.extend(small_element(MI_INT8, name.as_bytes()));
    } else {
        data.extend(element(MI_INT8, name.as_bytes()));
    }
    data.extend(real);
    element(MI_MATRIX, &data)
}

fn level5(elements: &[Vec<u8>]) -> Vec<u8> {
    let mut bytes = vec![b' '; 116];
    bytes.extend([0; 8]);
    bytes.extend(0x0100_u16.to_le_bytes());
    bytes.extend(b"IM");
    for element in elements {
        bytes.extend(element);
    }
    bytes
}

fn uint8_values() -> Vec<u8> {
    let values = COLUMN_MAJOR.map(|value| value as u8);
    element(MI_UINT8, &values)
}

fn compressed(element: &[u8]) -> Vec<u8> {
    let mut encoder = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
    encoder.write_all(element).expect("in-memory zlib stream");
    let data = encoder.finish().expect("in-memory zlib stream");
    let mut bytes = Vec::new();
    bytes.extend(MI_COMPRESSED.to_le_bytes());
    bytes.extend(
        u32::try_from(data.len())
            .expect("short element")
            .to_le_bytes(),
    );
    bytes.extend(data);
    bytes
}

#[test]
fn level4_matrices_read_in_either_byte_order() {
    let little = [
        level4("Other", 0, false, false),
        level4(NAME, 0, false, false),
    ]
    .concat();
    assert_eq!(read_matrix(&little, NAME), Ok(expected()));
    let big = level4(NAME, 1000, true, false);
    assert_eq!(read_matrix(&big, NAME), Ok(expected()));
}

#[test]
fn level4_text_and_complex_matrices_are_refused() {
    assert_eq!(
        read_matrix(&level4(NAME, 1, false, false), NAME),
        Err(MatError::NotNumeric)
    );
    assert_eq!(
        read_matrix(&level4(NAME, 0, false, true), NAME),
        Err(MatError::Complex)
    );
    assert_eq!(
        read_matrix(&level4(NAME, 0, false, false), "Missing"),
        Err(MatError::NotFound)
    );
}

/// Extents whose byte size overflows are a malformed header, never an
/// arithmetic overflow.
#[test]
fn level4_overflowing_extents_are_malformed() {
    let mut bytes = level4(NAME, 0, false, false);
    bytes[4..8].copy_from_slice(&u32::MAX.to_le_bytes());
    bytes[8..12].copy_from_slice(&u32::MAX.to_le_bytes());
    assert_eq!(read_matrix(&bytes, NAME), Err(MatError::Malformed));
    let mut bytes = level4(NAME, 0, false, false);
    bytes[16..20].copy_from_slice(&u32::MAX.to_le_bytes());
    assert_eq!(read_matrix(&bytes, NAME), Err(MatError::Malformed));
}

#[test]
fn level5_integer_typed_data_converts_to_real() {
    let file = level5(&[
        matrix_element("B", 6, &[1, 1], element(MI_DOUBLE, &1.5_f64.to_le_bytes())),
        matrix_element(NAME, 6, &[3, 2], uint8_values()),
    ]);
    assert_eq!(read_matrix(&file, NAME), Ok(expected()));
    let doubles = COLUMN_MAJOR
        .iter()
        .flat_map(|value| value.to_le_bytes())
        .collect::<Vec<_>>();
    let file = level5(&[matrix_element(
        NAME,
        6,
        &[3, 2],
        element(MI_DOUBLE, &doubles),
    )]);
    assert_eq!(read_matrix(&file, NAME), Ok(expected()));
}

#[test]
fn level5_compressed_elements_inflate() {
    let file = level5(&[compressed(&matrix_element(
        NAME,
        6,
        &[3, 2],
        uint8_values(),
    ))]);
    assert_eq!(read_matrix(&file, NAME), Ok(expected()));
}

#[test]
fn level5_checks_class_rank_and_complexity() {
    let char_class = level5(&[matrix_element(NAME, 4, &[3, 2], uint8_values())]);
    assert_eq!(read_matrix(&char_class, NAME), Err(MatError::NotNumeric));
    let rank3 = level5(&[matrix_element(NAME, 6, &[3, 2, 1], uint8_values())]);
    assert_eq!(read_matrix(&rank3, NAME), Err(MatError::NotRank2));
    let complex = level5(&[matrix_element(
        NAME,
        6 | COMPLEX_FLAG,
        &[3, 2],
        uint8_values(),
    )]);
    assert_eq!(read_matrix(&complex, NAME), Err(MatError::Complex));
    let short = level5(&[matrix_element(NAME, 6, &[3, 3], uint8_values())]);
    assert_eq!(read_matrix(&short, NAME), Err(MatError::Malformed));
}

#[test]
fn hdf5_files_are_refused() {
    let mut file = level5(&[]);
    file[124..126].copy_from_slice(&0x0200_u16.to_le_bytes());
    assert_eq!(read_matrix(&file, NAME), Err(MatError::Hdf5));
}
