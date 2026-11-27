// Keep scalar and volatile operations on LLVM's ordinary lowering path.
export function lowerBulkCopies(ir) {
  const call = /\bcall void @llvm\.memcpy\.p0\.p0\.i64\(([^\n]*?), i64 ([^,]+), i1 false\)/g;
  return ir.replace(call, (original, pointers, count) => {
    if (Number(count) < 96) return original;
    return 'call void @rmc_cuda_copy(' + pointers + ', i64 ' + count + ')';
  }) + '\ndeclare void @rmc_cuda_copy(ptr, ptr, i64)\n';
}
