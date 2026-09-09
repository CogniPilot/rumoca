pub fn parse(x: u32) -> Result<u32, u8> { if x > 9 { Err(1) } else { Ok(x + 1) } }
pub fn twice(x: u32) -> Result<u32, u8> { let a = parse(x)?; let b = parse(a)?; Ok(b) }
