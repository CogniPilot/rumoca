pub mod ast;
pub mod parser_helper;
pub use parser_helper::parse_file;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[allow(clippy::vec_box)]
    #[rustfmt::skip]
    modelica,
    "/s1_parser/modelica.rs"
);
