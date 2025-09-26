# Aurora Design Alignment Report — 2025-02-14

This document cross-references the original Aurora schematics (language spec blueprint) with repository deliverables to ensure work-in-progress aligns with the long-term vision.

## 1. Language Specification
| Pillar | Spec Expectations | Current Status | Next Steps |
| --- | --- | --- | --- |
| Formal Grammar & Syntax | Full EBNF, lexical tokens, unambiguous parse rules | `docs/spec/LRM.md` contains lexical draft and partial grammar; lexer prototype validates tokenisation | Expand grammar to cover declarations, patterns, modules; add parser prototype and golden tests |
| Type System | HM-based inference with subtyping, traits, lifetimes | Type/effect judgement scaffolding in LRM; no implementation yet | Prototype type inference crate incorporating effect solver; define trait resolution strategy |
| Effect System | Algebraic effect rows, effect inference, checked propagation | `aurora-effect-solver` crate prototypes row unification; ADR 0001 accepted | Extend solver with substitution maps and integrate with type inference engine |
| Memory Model | Ownership+borrowing+regions, smart pointers, optional GC | Spec placeholder; runtime skeleton future | Author memory section in LRM with rules/examples; spike region allocator design |
| Concurrency | Actors, structured concurrency, async/await | Scheduler spike (`runtime/scheduler`) with joinable actors; benchmark recorded | Design message routing APIs, error handling, structured join API; plan supervision model |
| Zones & Resource Awareness | GPU/RT zones, resource typing | Not yet implemented | Draft zone spec chapter; explore target annotations and runtime hooks |
| Compile-time Meta | Hygienic macros, CTFE, reflection | LRM placeholders only | Outline macro system design; evaluate CTFE execution strategy |

## 2. Compiler Architecture
| Layer | Blueprint | Current Artefacts | Gaps |
| --- | --- | --- | --- |
| Front-End | Lexer, parser, AST, type/effect checker | Lexer prototype crate; effect solver prototype | Parser, AST structures, type checker, borrow checker pending |
| Middle-End | IR, optimisation, effect validation | Not started | Choose IR representation; plan effect-aware passes |
| Back-End | LLVM integration, zone-aware codegen | Not started | Research LLVM bindings; design codegen pipeline |
| CTFE Engine | Macro expansion, const eval | Not started | Prototype evaluator leveraging interpreter or MIR-like IR |

## 3. Runtime System
| Component | Blueprint | Current Artefacts | Next Steps |
| --- | --- | --- | --- |
| Actor Scheduler | Work-stealing, structured concurrency | Tokio-based spike with structured join + benchmark | Introduce supervision, mailbox tuning, deterministic tests |
| Zone Manager | GPU/RT orchestration | Not yet | Research GPU backend integration; design zone descriptors |
| Memory Services | Region allocator, smart pointers, optional GC | Not yet | Draft allocator interface; evaluate ref-count + cycle detection |
| Tooling hooks | Debugging, profiling | Benchmark logging in research notes | Plan tracing infrastructure and metrics exposure |

## 4. Tooling & Ecosystem
| Item | Blueprint | Status | Next Steps |
| --- | --- | --- | --- |
| Package Manager | Cargo-like | Placeholder (none) | Design manifest schema, CLI layout |
| Formatter | aurorafmt | Not started | Define style guide, evaluate rustfmt-like approach |
| LSP | Language server | Not started | Outline architecture, tie into compiler front-end | 
| CI | Automated builds | GitHub Actions workflow (fmt/clippy/tests) | Add lint configs, doc generation |

## 5. Research & Governance
- Research notes seeded (`research/notes/2025-02-14-initial.md`) with comparative matrix, open questions, benchmark results.
- Citations database initiated (`research/citations/aurora.bib`).
- ADR process begun (ADR 0001 on effect rows).
- Roadmap exists (`docs/ROADMAP.md`); still aligned with phases.

## 6. Priority Gaps (Immediate Focus)
1. **Specification Depth**: Expand LRM sections on type system, memory model, concurrency semantics with concrete rules/examples.
2. **Parser Prototype**: Build AST + parser to complement lexer; necessary for further compiler work.
3. **Effect Solver Integration**: Extend solver with row variable substitution and embed into type inference design doc.
4. **Runtime Supervision Plan**: Design actor supervision/error propagation consistent with effect types.
5. **Zone Architecture Draft**: Produce design note outlining zone descriptors, codegen hooks, runtime enforcement plan.

## 7. Alignment Verdict
Current work tracks the blueprint’s emphasis on effects, concurrency, and rigorous specification. Major subsystems (parser, type checker, zone manager, tooling) remain to be implemented; prioritising these next will keep Aurora on trajectory toward the original schematics.

---
Prepared with intention to stay loving, careful, and curious in Aurora’s unfolding.
