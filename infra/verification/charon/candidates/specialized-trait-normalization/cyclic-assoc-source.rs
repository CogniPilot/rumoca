pub trait TryLike: Sized {
    type Output;
    type Residual: ResidualLike<Self::Output, TryType = Self>;
    fn extract(self) -> Self::Output;
}

pub trait ResidualLike<O>: Sized {
    type TryType: TryLike<Output = O, Residual = Self>;
}

pub struct Carrier<T>(pub T);
pub struct Empty;

impl<T> TryLike for Carrier<T> {
    type Output = T;
    type Residual = Empty;
    fn extract(self) -> T {
        self.0
    }
}

impl<T> ResidualLike<T> for Empty {
    type TryType = Carrier<T>;
}

pub fn extract_borrowed(value: &mut u32) -> &mut u32 {
    Carrier(value).extract()
}
