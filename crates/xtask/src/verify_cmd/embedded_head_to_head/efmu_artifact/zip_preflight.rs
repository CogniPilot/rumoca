//! Raw canonical-ZIP authentication performed before `zip::ZipArchive` may
//! allocate from archive metadata or normalize member names.

use anyhow::{Context, Result, ensure};

const EOCD_SIGNATURE: &[u8; 4] = b"PK\x05\x06";
const CENTRAL_SIGNATURE: &[u8; 4] = b"PK\x01\x02";
const LOCAL_SIGNATURE: &[u8; 4] = b"PK\x03\x04";
const EOCD_LEN: usize = 22;
const CENTRAL_HEADER_LEN: usize = 46;
const LOCAL_HEADER_LEN: usize = 30;
const FLAG_ENCRYPTED: u16 = 1;
const FLAG_DATA_DESCRIPTOR: u16 = 1 << 3;
const DEFLATE_METHOD: u16 = 8;

struct EndRecord {
    offset: usize,
    central_offset: usize,
    central_size: usize,
}

struct CentralEntry {
    flags: u16,
    method: u16,
    modified_time: u16,
    modified_date: u16,
    crc32: u32,
    compressed_size: usize,
    uncompressed_size: u32,
    local_offset: usize,
    next_offset: usize,
}

pub(super) fn authenticate(raw: &[u8], canonical_names: &[&str]) -> Result<()> {
    let end = authenticate_end_record(raw, canonical_names.len())?;
    let mut central_offset = end.central_offset;
    let mut next_local_offset = 0_usize;
    for (index, expected_name) in canonical_names.iter().enumerate() {
        let entry = authenticate_central_entry(raw, central_offset, expected_name, index)?;
        ensure!(
            entry.local_offset == next_local_offset,
            "eFMU ZIP local member {index} begins at {}, expected the canonical contiguous offset {next_local_offset}",
            entry.local_offset
        );
        next_local_offset = authenticate_local_entry(raw, &entry, expected_name, index)?;
        central_offset = entry.next_offset;
    }
    ensure!(
        next_local_offset == end.central_offset,
        "eFMU ZIP local-member region ends at {next_local_offset}, central directory begins at {}",
        end.central_offset
    );
    ensure!(
        central_offset
            .checked_sub(end.central_offset)
            .is_some_and(|size| size == end.central_size)
            && central_offset == end.offset,
        "eFMU ZIP central directory does not exactly fill its authenticated region"
    );
    Ok(())
}

fn authenticate_end_record(raw: &[u8], canonical_count: usize) -> Result<EndRecord> {
    let offset = raw
        .len()
        .checked_sub(EOCD_LEN)
        .context("eFMU ZIP is shorter than the canonical end record")?;
    ensure!(
        bytes(raw, offset, 4, "end-record signature")? == EOCD_SIGNATURE,
        "eFMU ZIP has no canonical comment-free end record"
    );
    ensure!(
        word(raw, offset + 4, "end-record disk")? == 0
            && word(raw, offset + 6, "central-directory disk")? == 0,
        "eFMU ZIP must be a single-disk archive"
    );
    let disk_entries = usize::from(word(raw, offset + 8, "disk member count")?);
    let total_entries = usize::from(word(raw, offset + 10, "total member count")?);
    ensure!(
        disk_entries == canonical_count && total_entries == canonical_count,
        "eFMU ZIP declares {disk_entries}/{total_entries} members; exact canonical inventory requires {canonical_count}"
    );
    ensure!(
        word(raw, offset + 20, "archive comment length")? == 0,
        "eFMU ZIP archive comments are not canonical"
    );
    let central_size = usize_from_u32(dword(raw, offset + 12, "central-directory size")?)?;
    let central_offset = usize_from_u32(dword(raw, offset + 16, "central-directory offset")?)?;
    let central_end = central_offset
        .checked_add(central_size)
        .context("eFMU ZIP central-directory range overflows")?;
    ensure!(
        central_end == offset,
        "eFMU ZIP central directory does not end at the canonical end record"
    );
    Ok(EndRecord {
        offset,
        central_offset,
        central_size,
    })
}

fn authenticate_central_entry(
    raw: &[u8],
    offset: usize,
    expected_name: &str,
    index: usize,
) -> Result<CentralEntry> {
    ensure!(
        bytes(raw, offset, 4, "central-header signature")? == CENTRAL_SIGNATURE,
        "eFMU ZIP member {index} has no canonical central header"
    );
    let name_length = usize::from(word(raw, offset + 28, "central name length")?);
    let extra_length = usize::from(word(raw, offset + 30, "central extra length")?);
    let comment_length = usize::from(word(raw, offset + 32, "member comment length")?);
    ensure!(
        extra_length == 0 && comment_length == 0,
        "eFMU ZIP member {index} must not carry central extra fields or comments"
    );
    ensure!(
        word(raw, offset + 34, "member start disk")? == 0,
        "eFMU ZIP member {index} must begin on the sole archive disk"
    );
    let name_offset = offset
        .checked_add(CENTRAL_HEADER_LEN)
        .context("eFMU ZIP central name offset overflows")?;
    ensure_exact_name(
        raw,
        name_offset,
        name_length,
        expected_name,
        index,
        "central",
    )?;
    let next_offset = name_offset
        .checked_add(name_length)
        .context("eFMU ZIP central member range overflows")?;
    Ok(CentralEntry {
        flags: word(raw, offset + 8, "central flags")?,
        method: word(raw, offset + 10, "central compression method")?,
        modified_time: word(raw, offset + 12, "central modified time")?,
        modified_date: word(raw, offset + 14, "central modified date")?,
        crc32: dword(raw, offset + 16, "central CRC-32")?,
        compressed_size: usize_from_u32(dword(raw, offset + 20, "central compressed size")?)?,
        uncompressed_size: dword(raw, offset + 24, "central uncompressed size")?,
        local_offset: usize_from_u32(dword(raw, offset + 42, "local-header offset")?)?,
        next_offset,
    })
}

fn authenticate_local_entry(
    raw: &[u8],
    central: &CentralEntry,
    expected_name: &str,
    index: usize,
) -> Result<usize> {
    let offset = central.local_offset;
    ensure!(
        bytes(raw, offset, 4, "local-header signature")? == LOCAL_SIGNATURE,
        "eFMU ZIP member {index} has no canonical local header"
    );
    let flags = word(raw, offset + 6, "local flags")?;
    ensure!(
        flags == central.flags
            && flags & (FLAG_ENCRYPTED | FLAG_DATA_DESCRIPTOR) == 0
            && central.method == DEFLATE_METHOD
            && word(raw, offset + 8, "local compression method")? == central.method
            && word(raw, offset + 10, "local modified time")? == central.modified_time
            && word(raw, offset + 12, "local modified date")? == central.modified_date
            && dword(raw, offset + 14, "local CRC-32")? == central.crc32
            && dword(raw, offset + 18, "local compressed size")?
                == u32::try_from(central.compressed_size)
                    .context("authenticated ZIP compressed size exceeds u32")?
            && dword(raw, offset + 22, "local uncompressed size")? == central.uncompressed_size,
        "eFMU ZIP member {index} local and central metadata differ or use a forbidden ZIP feature"
    );
    let name_length = usize::from(word(raw, offset + 26, "local name length")?);
    let extra_length = usize::from(word(raw, offset + 28, "local extra length")?);
    ensure!(
        extra_length == 0,
        "eFMU ZIP member {index} must not carry local extra fields"
    );
    let name_offset = offset
        .checked_add(LOCAL_HEADER_LEN)
        .context("eFMU ZIP local name offset overflows")?;
    ensure_exact_name(raw, name_offset, name_length, expected_name, index, "local")?;
    name_offset
        .checked_add(name_length)
        .and_then(|data_offset| data_offset.checked_add(central.compressed_size))
        .context("eFMU ZIP local member range overflows")
}

fn ensure_exact_name(
    raw: &[u8],
    offset: usize,
    length: usize,
    expected: &str,
    index: usize,
    header: &str,
) -> Result<()> {
    ensure!(
        length == expected.len()
            && bytes(raw, offset, length, "member name")? == expected.as_bytes(),
        "eFMU ZIP member {index} {header} raw name differs from canonical `{expected}`"
    );
    Ok(())
}

fn bytes<'raw>(raw: &'raw [u8], offset: usize, length: usize, field: &str) -> Result<&'raw [u8]> {
    let end = offset
        .checked_add(length)
        .with_context(|| format!("eFMU ZIP {field} range overflows"))?;
    raw.get(offset..end)
        .with_context(|| format!("eFMU ZIP is truncated in {field}"))
}

fn word(raw: &[u8], offset: usize, field: &str) -> Result<u16> {
    let value: [u8; 2] = bytes(raw, offset, 2, field)?
        .try_into()
        .with_context(|| format!("eFMU ZIP {field} has the wrong width"))?;
    Ok(u16::from_le_bytes(value))
}

fn dword(raw: &[u8], offset: usize, field: &str) -> Result<u32> {
    let value: [u8; 4] = bytes(raw, offset, 4, field)?
        .try_into()
        .with_context(|| format!("eFMU ZIP {field} has the wrong width"))?;
    Ok(u32::from_le_bytes(value))
}

fn usize_from_u32(value: u32) -> Result<usize> {
    usize::try_from(value).context("eFMU ZIP offset or size does not fit this host")
}
