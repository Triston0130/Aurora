# Aurora Compiler Stack

The compiler will be implemented in stages, bootstrapping from Rust before transitioning to self-hosted Aurora.

## Crate Layout (Planned)
- `frontend/`: lexer, parser, AST, desugaring.
- `typecheck/`: type inference, trait resolution, effect propagation, borrow checker.
- `ir/`: mid-level IR definition, transformations, analysis passes.
- `codegen/`: lowering to LLVM IR, zone-aware code generation, build artefact orchestration.
- `ctfe/`: compile-time evaluation engine and macro expansion.
- `driver/`: CLI entry point, configuration, incremental compilation cache.

## Immediate Tasks
1. Prototype lexer in Rust (`prototypes/lexer.rs`) to validate tokenisation rules.
2. Define AST data structures and serialisation format for debugging.
3. Establish error reporting style guide (codes, spans, suggestions).
4. Integrate with testing harness for golden-file parser tests.

## Technology Choices
- Rust 1.78+ for bootstrap implementation.
- Rust’s `logos` crate as optional lexer generator (to be evaluated against a hand-written approach).
- LLVM 17 backend via `inkwell` or direct `llvm-sys` bindings.

## Research Questions
- How to encode effect rows efficiently in the type inference graph.
- Strategies for borrow checking with region polymorphism.
- Compile-time scheduling of zone transitions.

*Contributions follow the forthcoming RFC guidelines; prototypes live under `prototypes/` prior to integration.*
