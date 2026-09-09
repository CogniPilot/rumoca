pub fn once<G: FnOnce(u32) -> u32>(g: G) -> u32 {
    g(1)
}
pub fn wrap<F: FnMut(u32) -> u32>(f: &mut F) -> u32 {
    once(f)
}
