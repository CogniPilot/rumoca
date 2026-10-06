//! Checked disjoint reservations for one unshared browser-private memory.
#[cfg(test)]
mod tests;

const PAGE_BYTES: u64 = 65536;
const MAX_KERNEL_BYTES: u32 = 64 * 1024 * 1024;
const MAX_POOL_BYTES: u64 = 1u64 << 32;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ArenaRegion {
    pub(crate) base: u32,
    pub(crate) end: u64,
}

#[derive(Default)]
pub(crate) struct ArenaPoolPlan {
    next: u64,
}

impl ArenaPoolPlan {
    pub(crate) fn prepare(&self, bytes: u32) -> Result<ArenaRegion, String> {
        if bytes == 0 || bytes > MAX_KERNEL_BYTES || u64::from(bytes) % PAGE_BYTES != 0 {
            return Err(
                "private arena requires a nonempty page-aligned region at most 64 MiB".into(),
            );
        }
        let end = self
            .next
            .checked_add(u64::from(bytes))
            .filter(|end| *end <= MAX_POOL_BYTES)
            .ok_or("private arena pool exceeds wasm32 address capacity")?;
        let base = u32::try_from(self.next).map_err(|_| "private arena base exceeds wasm32")?;
        Ok(ArenaRegion { base, end })
    }

    pub(crate) fn commit(&mut self, region: ArenaRegion) -> Result<(), String> {
        if u64::from(region.base) != self.next || region.end <= self.next {
            return Err("private arena reservation differs from current pool cursor".into());
        }
        self.next = region.end;
        Ok(())
    }
}
