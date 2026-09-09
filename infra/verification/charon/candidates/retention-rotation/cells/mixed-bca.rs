pub trait B: C { }
pub trait C: Sized { type Z: A; }
pub trait A: Sized { type X: B; }
pub struct SA; pub struct SB; pub struct SC;
impl A for SA { type X = SB; }
impl C for SB { type Z = SA; }
impl B for SB { }
pub fn use_a<T: A>(t: T) -> T { t }
pub fn use_c<T: C>(t: T) -> T { t }
