# Front-End Architecture Blueprint (2025-02-14)

## Pipeline Overview
1. **Lexical Analysis** — `lexer.rs`
   - Converts source text into tokens, handling keywords, operators, and literals.
   - Emits structural tokens for attributes (`@`) and comments for tooling.

2. **Parsing** — `parser.rs`
   - Builds the AST defined in `ast.rs`, including attributes, effect clauses, and block expressions.
   - Validates syntactic invariants and produces early diagnostics.

3. **Attribute & Zone Handling**
   - Attributes captured on AST nodes (currently functions). The `zone` helper module converts `@zone(...)` attributes into `ZoneDescriptor`s for later validation and codegen.

4. **Constraint Generation (Planned)**
   - Traverse AST to emit type/effect/lifetime constraints.
   - Utilise `types.rs` data structures (`TypeEnv`, `InferenceContext`, `Scheme`).

5. **Unification and Effect Solving (Planned)**
   - Integrate `aurora-effect-solver` to resolve effect rows alongside type unification.
   - Produce substitution maps covering type, effect, and lifetime variables.

6. **Borrow Checking (Planned)**
   - Run after inference to ensure ownership rules via region/lifetime constraints.

7. **IR Lowering (Future)**
   - Translate validated AST into intermediate representation annotated with type/effect/zone metadata.

## Module Responsibilities
- `ast.rs`: canonical AST types, attribute representation, literals.
- `lexer.rs`: token stream producer, keyword classification.
- `parser.rs`: recursive-descent parser with precedence-aware expression parsing, attribute support.
- `types.rs`: inference scaffolding (type variables, environments, schemes).
- `zone.rs`: attribute-to-descriptor conversion for zone-aware compilation.
- `effect-solver` crate: shared library for effect row unification.

## Diagnostics Strategy
- Lexer/Parser produce `ParseError` with structured messages.
- Inference stage to emit diagnostic codes (`E1001` etc.) referencing LRM sections.
- Zone validation errors should cite capability violations (`realtime` forbidding `IO`, etc.).

## Next Milestones
- Implement constraint generator covering expression subset (Phase B).
- Integrate zone descriptors in constraint validation (Phase D).
- Produce IR design note to bridge front-end with middle-end.

---
Living document; update alongside front-end implementation.
