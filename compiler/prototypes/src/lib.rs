//! Aurora compiler prototype crate.
//! Currently exposes the lexer experiment, AST types, and a lightweight parser.

pub mod ast;
pub mod backend;
pub mod borrow;
pub mod const_eval;
pub mod constraints;
pub mod ctfe;
pub mod diagnostics;
pub mod driver;
pub mod hir;
pub mod incremental;
pub mod lexer;
pub mod macro_expander;
pub mod mir;
pub mod parser;
pub mod resolver;
pub mod solver;
pub mod span;
pub mod trait_solver;
pub mod types;
pub mod zone;
