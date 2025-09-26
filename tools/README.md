# Aurora Tooling Suite

Tooling ensures developer productivity through automation and rich IDE support.

## Current Prototypes
- `aurorapm/` — package manager stub (manifest init/add).
- `aurorafmt/` — formatter stub (line reordering placeholder, diff/check modes).
- `auroralint/` — CLI linter detecting trailing whitespace and TODO markers.
- `aurorals/` — minimal JSON-RPC language server prototype.
- `aurorarepl/` — REPL prototype with simple `let` binding recognition.
- `auroradebug/` — Debug Adapter Protocol stub responding to initialise/launch/breakpoint requests.

## Immediate Tasks
1. Expand `aurorapm` to resolve dependency graphs and emit lockfiles.
2. Replace `aurorafmt` placeholder formatting with AST-driven rewriter.
3. Enhance `auroralint` with AST integration and effect/zone checks.
4. Implement incremental analysis in `aurorals` and integrate with compiler crates.
5. Grow `aurorarepl` into an interactive evaluator backed by the runtime.
6. Flesh out `auroradebug` with source maps, stepping, and runtime inspection hooks.

## Integration
All tooling should integrate with CI pipelines and support macOS, Linux, and Windows. Provide JSON-schema definitions where applicable for editor plugins.

---
Tool design discussions will produce RFCs housed under `docs/reference/tooling/` (to be created).
