target datalayout = "e-i64:64-i128:128-v16:16-v32:32-n16:32:64"
target triple = "nvptx64-nvidia-cuda"

define void @euler_update_kernel(ptr %y, ptr %xdot, double %dt, i64 %n) {
entry:
  %tidx_raw  = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
  %bidx_raw  = call i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
  %bdimx_raw = call i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
  %tidx      = zext i32 %tidx_raw  to i64
  %bidx      = zext i32 %bidx_raw  to i64
  %bdimx     = zext i32 %bdimx_raw to i64
  %blk_off   = mul i64 %bidx, %bdimx
  %i         = add i64 %blk_off, %tidx
  %in_range  = icmp ult i64 %i, %n
  br i1 %in_range, label %active, label %exit
active:
  %y_ptr    = getelementptr inbounds double, ptr %y,    i64 %i
  %xdot_ptr = getelementptr inbounds double, ptr %xdot, i64 %i
  %yi    = load  double, ptr %y_ptr,    align 8
  %xi    = load  double, ptr %xdot_ptr, align 8
  %dxi   = fmul double %xi, %dt
  %new_y = fadd double %yi, %dxi
  store double %new_y, ptr %y_ptr, align 8
  br label %exit
exit:
  ret void
}

declare i32 @llvm.nvvm.read.ptx.sreg.tid.x() #0
declare i32 @llvm.nvvm.read.ptx.sreg.ctaid.x() #0
declare i32 @llvm.nvvm.read.ptx.sreg.ntid.x() #0

attributes #0 = { nounwind readnone }

!nvvm.annotations = !{!0}
!0 = !{ptr @euler_update_kernel, !"kernel", i32 1}
