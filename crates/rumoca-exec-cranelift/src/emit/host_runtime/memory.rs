//! Page-isolated JIT chunks keep code protection out of the general heap.
#[cfg(test)]
mod tests;

use cranelift_jit::{ArenaMemoryProvider, BranchProtection, JITMemoryProvider};
use cranelift_module::ModuleResult;
use std::io;

// Growth granularity, not a limit: a larger request gets a larger chunk.
const CHUNK_GRANULARITY_BYTES: usize = 64 * 1024;

#[derive(Clone, Copy)]
enum Kind {
    Code,
    ReadOnly,
    Writable,
}

struct Chunk {
    arena: ArenaMemoryProvider,
    capacity: usize,
    used: usize,
    committed: usize,
    sealed: bool,
}

struct Pool {
    kind: Kind,
    chunks: Vec<Chunk>,
}

pub(super) struct ChunkedMemory {
    pools: [Pool; 3],
    poisoned: bool,
}

impl Default for ChunkedMemory {
    fn default() -> Self {
        Self {
            poisoned: false,
            pools: [Kind::Code, Kind::ReadOnly, Kind::Writable].map(|kind| Pool {
                kind,
                chunks: Vec::new(),
            }),
        }
    }
}

fn aligned(value: usize, alignment: usize) -> io::Result<usize> {
    value
        .checked_add(alignment - 1)
        .map(|sum| sum & !(alignment - 1))
        .ok_or_else(|| io::Error::other("JIT chunk alignment overflows"))
}

fn allocation_end(used: usize, size: usize, alignment: usize) -> io::Result<usize> {
    aligned(used, alignment)?
        .checked_add(size)
        .ok_or_else(|| io::Error::other("JIT chunk allocation overflows"))
}

impl Pool {
    fn allocate(&mut self, size: usize, alignment: u64) -> io::Result<*mut u8> {
        let page = region::page::size();
        let align = usize::try_from(alignment)
            .ok()
            .filter(|align| align.is_power_of_two() && *align <= page)
            .ok_or_else(|| {
                io::Error::other("JIT alignment must be a power of two within a page")
            })?;
        let size = size.max(1);
        if let Some(chunk) = self.chunks.last_mut()
            && !chunk.sealed
            && chunk.required_commit(size, align, page)? <= chunk.capacity
        {
            return chunk.allocate(self.kind, size, alignment, align, page);
        }
        // Tiny owners and newly published batches need only one actual page.
        // Filled, unsealed pools grow geometrically to amortize protections.
        let granularity = self
            .chunks
            .last()
            .filter(|chunk| !chunk.sealed)
            .map_or(page, |chunk| {
                chunk
                    .capacity
                    .saturating_mul(2)
                    .min(CHUNK_GRANULARITY_BYTES)
                    .max(page)
            });
        let capacity = aligned(size.max(granularity), page)?;
        self.chunks
            .try_reserve(1)
            .map_err(|error| io::Error::new(io::ErrorKind::OutOfMemory, error))?;
        let mut chunk = Chunk {
            arena: ArenaMemoryProvider::new_with_size(capacity).map_err(io::Error::other)?,
            capacity,
            used: 0,
            committed: 0,
            sealed: false,
        };
        let pointer = chunk.allocate(self.kind, size, alignment, align, page)?;
        self.chunks.push(chunk);
        Ok(pointer)
    }
}

impl Chunk {
    fn required_commit(&self, size: usize, align: usize, page: usize) -> io::Result<usize> {
        if allocation_end(self.used, size, align)? <= self.committed {
            return Ok(self.committed);
        }
        // The upstream arena extends a segment by the page-rounded complete
        // request, rather than only the bytes beyond its existing tail.
        self.committed
            .checked_add(aligned(size, page)?)
            .ok_or_else(|| io::Error::other("JIT chunk commitment overflows"))
    }

    fn allocate(
        &mut self,
        kind: Kind,
        size: usize,
        alignment: u64,
        align: usize,
        page: usize,
    ) -> io::Result<*mut u8> {
        let end = allocation_end(self.used, size, align)?;
        let committed = self.required_commit(size, align, page)?;
        let pointer = match kind {
            Kind::Code => self.arena.allocate_readexec(size, alignment),
            Kind::ReadOnly => self.arena.allocate_readonly(size, alignment),
            Kind::Writable => self.arena.allocate_readwrite(size, alignment),
        }?;
        self.used = end;
        self.committed = committed;
        Ok(pointer)
    }
}

impl ChunkedMemory {
    fn allocation<T>(
        &mut self,
        operation: impl FnOnce(&mut [Pool; 3]) -> io::Result<T>,
    ) -> io::Result<T> {
        if self.poisoned {
            return Err(io::Error::other("JIT memory provider is poisoned"));
        }
        // Upstream arena protection operations can unwind on an OS error.
        // The interrupted arena cannot be reused because its segment accounting
        // may be partially updated. Its reservation stays owned for cleanup.
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| operation(&mut self.pools)))
        {
            Ok(result) => result,
            Err(payload) => {
                self.poisoned = true;
                let message = payload
                    .downcast_ref::<String>()
                    .map(String::as_str)
                    .or_else(|| payload.downcast_ref::<&str>().copied())
                    .unwrap_or("unknown upstream memory protection failure");
                Err(io::Error::other(format!(
                    "JIT memory allocation failed: {message}"
                )))
            }
        }
    }
}

impl JITMemoryProvider for ChunkedMemory {
    fn allocate_readexec(&mut self, size: usize, align: u64) -> io::Result<*mut u8> {
        self.allocation(|pools| pools[0].allocate(size, align))
    }

    fn allocate_readonly(&mut self, size: usize, align: u64) -> io::Result<*mut u8> {
        self.allocation(|pools| pools[1].allocate(size, align))
    }

    fn allocate_readwrite(&mut self, size: usize, align: u64) -> io::Result<*mut u8> {
        self.allocation(|pools| pools[2].allocate(size, align))
    }

    fn finalize(&mut self, protection: BranchProtection) -> ModuleResult<()> {
        if self.poisoned {
            return Err(cranelift_module::ModuleError::Backend(
                io::Error::other("JIT memory provider is poisoned").into(),
            ));
        }
        for chunk in self
            .pools
            .iter_mut()
            .flat_map(|pool| &mut pool.chunks)
            .filter(|chunk| !chunk.sealed)
        {
            chunk.arena.finalize(protection)?;
            chunk.sealed = true;
        }
        Ok(())
    }

    unsafe fn free_memory(&mut self) {
        for pool in &mut self.pools {
            for chunk in &mut pool.chunks {
                // SAFETY: the consuming JIT module owner guarantees that none
                // of its emitted pointers remains callable when freeing it.
                unsafe { chunk.arena.free_memory() };
            }
            pool.chunks.clear();
        }
    }
}
