// Place fixed-size entry-block aggregates on a bounded stack per active CUDA lane.
export function lowerLaneWorkspace(input) {
  let functions = 0;
  let maxFrame = 0;
  const definitions = /^define ([^\n]*) \{\n([\s\S]*?)^\}/gm;
  const aggregate = /^(  )(%[a-zA-Z0-9._-]+) = alloca \[(\d+) x (double|float|i8|i16|i32|i64|ptr)\], align (\d+)(?:, [^\n]+)?$/gm;
  const bytes = {double:8, float:4, i8:1, i16:2, i32:4, i64:8, ptr:8};
  const alignUp = (n, alignment) => Math.ceil(n / alignment) * alignment;
  const ir = input.replace(definitions, (original, heading, body) => {
    if (body.includes('musttail call')) return original;
    const prefix = body.match(/^[^ \n][^\n]*:\s*(?:;[^\n]*)?\n/)?.[0] ?? '';
    const statements = body.slice(prefix.length);
    const nextBlock = statements.search(/^[^ ;\n][^\n]*:/m);
    let size = 0;
    const entry = (nextBlock < 0 ? statements : statements.slice(0, nextBlock)).replace(
      aggregate, (line, indent, destination, count, type, alignment) => {
        const length = bytes[type] * Number(count);
        if (length < 32 || Number(alignment) > 256) return line;
        const offset = alignUp(size, Number(alignment));
        size = offset + length;
        return indent + destination + ' = getelementptr i8, ptr %rmc.arena, i64 ' + offset;
      });
    if (size === 0) return original;
    const frame = alignUp(size, 256);
    const tail = nextBlock < 0 ? '' : statements.slice(nextBlock);
    const lowered = (entry + tail).replace(/^  ret (.+)$/gm,
      '  call void @rmc_release(i64 ' + frame + ')\n  ret $1');
    maxFrame = Math.max(maxFrame, frame);
    functions++;
    return 'define ' + heading + ' {\n' + prefix +
      '  %rmc.arena = call ptr @rmc_reserve(i64 ' + frame + ')\n' + lowered + '}';
  });
  return {ir: ir + '\n' + deviceStack, functions, maxFrame};
}

const deviceStack = `@rmc_scratch = addrspace(1) externally_initialized global ptr null, align 8
@rmc_stride = addrspace(1) externally_initialized global i64 0, align 8
define internal ptr @rmc_lane_ptr(){
  %bx = call i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
  %nt = call i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
  %tx = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
  %i = mul i32 %bx,%nt
  %ix = add i32 %i,%tx
  %lane = zext i32 %ix to i64
  %stride = load volatile i64, ptr addrspace(1) @rmc_stride
  %offset = mul i64 %lane,%stride
  %base = load volatile ptr, ptr addrspace(1) @rmc_scratch
  %p = getelementptr i8,ptr %base,i64 %offset
  ret ptr %p
}
define internal ptr @rmc_reserve(i64 %size){
  %lane = call ptr @rmc_lane_ptr()
  %sp = load volatile i64, ptr %lane
  %end = add i64 %sp,%size
  %last = add i64 %end,256
  %stride = load volatile i64, ptr addrspace(1) @rmc_stride
  %overflow = icmp ugt i64 %last,%stride
  br i1 %overflow,label %bad,label %ok
bad:
  call void @llvm.trap()
  unreachable
ok:
  store volatile i64 %end,ptr %lane
  %offset = add i64 %sp,256
  %p = getelementptr i8,ptr %lane,i64 %offset
  ret ptr %p
}
define internal void @rmc_release(i64 %size){
  %lane = call ptr @rmc_lane_ptr()
  %sp = load volatile i64,ptr %lane
  %end = sub i64 %sp,%size
  store volatile i64 %end,ptr %lane
  ret void
}`;
