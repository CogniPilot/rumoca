pub trait TryLike: Sized {
    type Output;
    type Residual: ResidualLike<Self::Output>;
    fn extract(self) -> Self::Output;
}

pub trait ResidualLike<O>: Sized {
    type TryType: TryLike<Output = O, Residual = Self>;
}

pub struct Carrier<T>(pub T);
pub struct Alternative<T>(pub T);
pub struct Empty;

impl<T> TryLike for Carrier<T> {
    type Output = T;
    type Residual = Empty;
    fn extract(self) -> T {
        self.0
    }
}

impl<T> TryLike for Alternative<T> {
    type Output = T;
    type Residual = Empty;
    fn extract(self) -> T {
        self.0
    }
}

impl<T> ResidualLike<T> for Empty {
    type TryType = Carrier<T>;
}

// The return type is Carrier<T>, not Alternative<T>. Shared residual/output
// types therefore cannot justify identifying the two TryLike implementors.
pub fn canonicalize<T>(value: Alternative<T>) -> <Empty as ResidualLike<T>>::TryType {
    Carrier(value.extract())
}

// Unlike identifying arbitrary TryLike implementors, revisiting this exact
// ResidualLike predicate is justified by its declared Output/Residual equalities.
pub fn revisit_residual<O, R: ResidualLike<O>>(
    value: R::TryType,
) -> <<R::TryType as TryLike>::Residual as ResidualLike<O>>::TryType {
    value
}

pub fn convert_borrowed(value: &mut u32) -> Carrier<&mut u32> {
    revisit_residual::<&mut u32, Empty>(canonicalize(Alternative(value)))
}

#[test]
fn reverse_trait_selects_carrier_without_losing_the_borrow() {
    let mut value = 6;
    let Carrier(borrow) = convert_borrowed(&mut value);
    *borrow += 1;
    assert_eq!(value, 7);
}
