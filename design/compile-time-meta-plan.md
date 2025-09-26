# Compile-Time Metaprogramming Plan (TODO #15)

## Vision
Aurora’s compile-time surface combines hygienic macros, evaluable `const fn`, and structured reflection so that effect-aware programs can generate code without compromising soundness. Item 15 of the checklist targets a shippable 1.0 feature set:

- Deterministic evaluation of `const` expressions/functions during compilation.
- Declarative macros for syntax sugar that preserve hygiene (no accidental capture).
- Procedural macros with constrained side effects, reusing the effect system so compile-time work remains auditable.
- Reflection hooks exposing type/effect metadata without leaking implementation details.

## Guiding Principles
1. **Soundness first** – evaluation runs in a sandboxed interpreter that mirrors runtime semantics. No unchecked `unsafe` escape hatches.
2. **Effect awareness** – compile-time work has its own effect row (e.g., `CompileIO`, `CompileTimeDiagnostics`) to keep build scripts honest.
3. **Determinism & caching** – results depend solely on explicit inputs (source text, environment variables passed via config). We reuse the incremental cache (TODO 11) to memoise CTFE results.
4. **Progressive disclosure** – start with expression-level const evaluation and attribute macros, then expand toward statement/trait reflection.

## Architectural Outline
```
          +--------------+
          |   Parser     |
          +------+-------+
                 |
                 v
         macro expansion (declarative)
                 |
                 v
        +--------+---------+
        |  AST + Attributes|
        +--------+---------+
                 |
      const-eval lowerings (const fn bodies)
                 |
                 v
        +--------+---------+
        |  MIR (SSA)       |
        +------------------+
                 |
                 v
            LLVM backend
```

### Components
- **Macro Registry** – stores macro definitions (`macro_rules!`-style) in HIR, keyed by module path.
- **Macro Expander** – runs between parsing and name resolution; expands declarative macros, emits diagnostics with spans from source map.
- **CTFE Interpreter** – reuses MIR structures in “const mode”; executes a restricted instruction set with pure operations, producing `ConstValue` results.
- **Reflection API** – exposes functions like `type_name<T>()` or `effects_of!(fn_path)` using CTFE intrinsics.
- **Diagnostics Channel** – macros and const functions can emit notes/warnings via the shared diagnostic engine.

### Data Flow
1. Parse module → collect macro defs → store in registry.
2. Walk AST; whenever encountering macro invocation:
   - Expand using declarative matcher.
   - Re-lex/parse generated tokens under hygiene rules.
   - Record expansion span mapping for diagnostics and IDE tooling.
3. During constraint generation, mark `const` items for CTFE.
4. After MIR SSA lowering, run CTFE interpreter on marked bodies to produce values; feed into type inference/trait solver when needed (e.g., array lengths, zone parameters).
5. Cache CTFE results keyed by MIR hash + macro arguments using incremental infrastructure.

## Phased Delivery
### Phase A – Expression CTFE & Attributes (Milestone 15.1)
- Extend AST with `Const` qualifiers on functions/let bindings.
- Upgrade `const_eval::eval` to interpret MIR expression trees (current literal evaluator becomes base cases).
- Allow attribute macros on items (e.g., `@derive(Debug)` placeholder) that expand into AST nodes.
- Add driver flag `--emit=ctfe-trace` for debugging.

### Phase B – Declarative Macros (Milestone 15.2)
- Grammar additions: `macro_rules! name { ... }` with pattern/action pairs.
- Macro expander with hygiene using synthetic `MacroSpan` to resolve identifiers.
- Snapshot tests for expansion; reject recursive expansion exceeding fuel limit.

### Phase C – Procedural Macros & Reflection (Milestone 15.3)
- Define `macro proc name(args)` syntax returning AST fragments.
- CTFE interpreter gains host functions for limited reflection (type/effect queries) guarded by capability attributes.
- Integrate incremental cache to avoid re-running unchanged macros across builds.

### Phase D – Integration & Tooling (Milestone 15.4)
- Expose CTFE/macro support through CLI (`--emit=expanded`, `--emit=consts`).
- Update diagnostics renderer to show expansion traces (primary span + origin).
- Document language reference chapter 9 with examples.

## Open Questions
- Borrowing semantics during CTFE – do we permit references in `const fn`? Likely yes, but restrict to immutable data and `Copy` types initially.
- Interaction with zones – can macros introduce zone annotations dynamically? Need policy to restrict zone creation to trusted macros.
- Procedural macro sandboxing – consider WASM plug-ins vs. in-process Rust functions; start with in-process but require `@trusted_macro` attribute.

## Next Steps
1. Implement Phase A scaffolding: AST markers for `const`, expand `const_eval` into MIR interpreter, wire CTFE results into driver diagnostics.
2. Prepare parser changes for `macro_rules!`; schedule for Phase B once CTFE MVP is stable.
3. Update `docs/spec/LRM.md` section 9 with MVP feature description after implementation.

This plan keeps CTFE and macros aligned with Aurora’s effect-aware philosophy while delivering incremental value.
