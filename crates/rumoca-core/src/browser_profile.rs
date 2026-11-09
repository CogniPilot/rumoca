//! Opt-in browser diagnostic clocks. No numerical or execution decisions.
//! Times are inclusive and exclusive nested wall time, not CPU time.
#[cfg(all(target_arch = "wasm32", feature = "browser-profile"))]
mod enabled {
    use wasm_bindgen::prelude::*;
    #[wasm_bindgen(inline_js = r#"
let active=false, records=Object.create(null), stack=[];
export function rumoca_profile_enable(value){active=value;records=Object.create(null);stack=[];}
export function rumoca_profile_start(name){
 if(!active)return -1;
 const start=performance.now();stack.push({name,start,children:0});return start;
}
export function rumoca_profile_end(name,start){
 if(start<0)return;
 const end=performance.now(),entry=stack.pop();
 if(!entry||entry.name!==name||entry.start!==start)throw Error('profile stack mismatch');
 const elapsed=end-start,r=records[name]??={calls:0,inclusive_ms:0,exclusive_ms:0,max_ms:0};
 r.calls++;r.inclusive_ms+=elapsed;r.exclusive_ms+=elapsed-entry.children;r.max_ms=Math.max(r.max_ms,elapsed);
 if(stack.length)stack[stack.length-1].children+=elapsed;
}
export function rumoca_profile_snapshot(){return JSON.stringify({records,open:stack.map(x=>x.name),clock:'performance.now',instrumented:true});}
"#)]
    extern "C" {
        fn rumoca_profile_enable(value: bool);
        fn rumoca_profile_start(name: &str) -> f64;
        fn rumoca_profile_end(name: &str, start: f64);
        fn rumoca_profile_snapshot() -> String;
    }
    pub fn enable(value: bool) {
        rumoca_profile_enable(value);
    }
    pub fn snapshot() -> String {
        rumoca_profile_snapshot()
    }
    pub struct Span {
        name: &'static str,
        start: f64,
    }
    impl Span {
        #[inline]
        pub fn new(name: &'static str) -> Self {
            Self {
                name,
                start: rumoca_profile_start(name),
            }
        }
    }
    impl Drop for Span {
        fn drop(&mut self) {
            rumoca_profile_end(self.name, self.start);
        }
    }
}
#[cfg(all(target_arch = "wasm32", feature = "browser-profile"))]
pub use enabled::{Span, enable, snapshot};
#[cfg(not(all(target_arch = "wasm32", feature = "browser-profile")))]
mod disabled {
    pub struct Span;
    impl Span {
        #[inline]
        pub fn new(_: &'static str) -> Self {
            Self
        }
    }
    pub fn enable(_: bool) {}
    pub fn snapshot() -> String {
        "{\"instrumented\":false}".into()
    }
}
#[cfg(not(all(target_arch = "wasm32", feature = "browser-profile")))]
pub use disabled::{Span, enable, snapshot};
