pub trait B: Sized { type Y: A; }
pub trait A: Sized { type X: B<Y = Self>; }
pub struct SA; pub struct SB;
impl A for SA { type X = SB; }
impl B for SB { type Y = SA; }
pub fn use_a<T: A>(t: T) -> T { t }
