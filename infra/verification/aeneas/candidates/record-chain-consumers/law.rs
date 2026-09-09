pub fn p32(x: u32) -> Result<u32, u8> { if x > 9 { Err(1) } else { Ok(x + 1) } }
pub fn p64(x: u64) -> Result<u64, u16> { if x > 9 { Err(2) } else { Ok(x + 1) } }
pub fn t32(x: u32) -> Result<u32, u8> { let a = p32(x)?; Ok(a) }
pub fn t64(x: u64) -> Result<u64, u16> { let a = p64(x)?; Ok(a) }
