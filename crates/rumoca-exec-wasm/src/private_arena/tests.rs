use super::*;

#[test]
fn disjoint_regions_preserve_first_allocation_and_do_not_publish_failed_preparation() {
    let mut pool = ArenaPoolPlan::default();
    let first = pool.prepare(65536).unwrap();
    assert_eq!(
        first,
        ArenaRegion {
            base: 0,
            end: 65536
        }
    );
    assert!(pool.prepare(0).is_err());
    assert_eq!(pool.prepare(65536).unwrap(), first);
    pool.commit(first).unwrap();
    let second = pool.prepare(2 * 65536).unwrap();
    assert_eq!(u64::from(second.base), first.end);
    assert_eq!(second.end, 3 * 65536);
    assert!(pool.commit(first).is_err());
    pool.commit(second).unwrap();
}

#[test]
fn original_per_kernel_limit_and_complete_wasm32_pool_boundary_remain_checked() {
    let mut pool = ArenaPoolPlan::default();
    assert!(pool.prepare(MAX_KERNEL_BYTES + 65536).is_err());
    assert!(pool.prepare(8).is_err());
    for _ in 0..64 {
        let region = pool.prepare(MAX_KERNEL_BYTES).unwrap();
        pool.commit(region).unwrap();
    }
    assert_eq!(pool.next, MAX_POOL_BYTES);
    assert!(pool.prepare(65536).is_err());
}
