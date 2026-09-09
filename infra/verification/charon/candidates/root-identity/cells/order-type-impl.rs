pub struct View<'a> { pub data: &'a [u32] }
impl<'a> View<'a> { pub fn refine(&self) -> u32 { self.data.len() as u32 } }
pub fn free(view: &View<'_>) -> u32 { view.refine() + 1 }
