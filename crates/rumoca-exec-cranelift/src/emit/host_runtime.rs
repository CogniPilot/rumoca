use cranelift_jit::JITBuilder;

pub(super) fn register_math_symbols(builder: &mut JITBuilder) {
    builder.symbol("rumoca_host_sin", rumoca_host_sin as *const u8);
    builder.symbol("rumoca_host_cos", rumoca_host_cos as *const u8);
    builder.symbol("rumoca_host_tan", rumoca_host_tan as *const u8);
    builder.symbol("rumoca_host_asin", rumoca_host_asin as *const u8);
    builder.symbol("rumoca_host_acos", rumoca_host_acos as *const u8);
    builder.symbol("rumoca_host_atan", rumoca_host_atan as *const u8);
    builder.symbol("rumoca_host_atan2", rumoca_host_atan2 as *const u8);
    builder.symbol("rumoca_host_sinh", rumoca_host_sinh as *const u8);
    builder.symbol("rumoca_host_cosh", rumoca_host_cosh as *const u8);
    builder.symbol("rumoca_host_tanh", rumoca_host_tanh as *const u8);
    builder.symbol("rumoca_host_exp", rumoca_host_exp as *const u8);
    builder.symbol("rumoca_host_log", rumoca_host_log as *const u8);
    builder.symbol("rumoca_host_log10", rumoca_host_log10 as *const u8);
    builder.symbol("rumoca_host_floor", rumoca_host_floor as *const u8);
    builder.symbol("rumoca_host_ceil", rumoca_host_ceil as *const u8);
    builder.symbol("rumoca_host_trunc", rumoca_host_trunc as *const u8);
    builder.symbol("rumoca_host_powf", rumoca_host_powf as *const u8);
}

extern "C" fn rumoca_host_sin(x: f64) -> f64 {
    x.sin()
}
extern "C" fn rumoca_host_cos(x: f64) -> f64 {
    x.cos()
}
extern "C" fn rumoca_host_tan(x: f64) -> f64 {
    x.tan()
}
extern "C" fn rumoca_host_asin(x: f64) -> f64 {
    x.asin()
}
extern "C" fn rumoca_host_acos(x: f64) -> f64 {
    x.acos()
}
extern "C" fn rumoca_host_atan(x: f64) -> f64 {
    x.atan()
}
extern "C" fn rumoca_host_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}
extern "C" fn rumoca_host_sinh(x: f64) -> f64 {
    x.sinh()
}
extern "C" fn rumoca_host_cosh(x: f64) -> f64 {
    x.cosh()
}
extern "C" fn rumoca_host_tanh(x: f64) -> f64 {
    x.tanh()
}
extern "C" fn rumoca_host_exp(x: f64) -> f64 {
    x.exp()
}
extern "C" fn rumoca_host_log(x: f64) -> f64 {
    x.ln()
}
extern "C" fn rumoca_host_log10(x: f64) -> f64 {
    x.log10()
}
extern "C" fn rumoca_host_floor(x: f64) -> f64 {
    x.floor()
}
extern "C" fn rumoca_host_ceil(x: f64) -> f64 {
    x.ceil()
}
extern "C" fn rumoca_host_trunc(x: f64) -> f64 {
    x.trunc()
}
extern "C" fn rumoca_host_powf(x: f64, y: f64) -> f64 {
    x.powf(y)
}
