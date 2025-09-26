# Aurora Language Reference Manual (LRM)

*Version: draft-0.1 — maintained by the Language Design Working Group*

This manual provides the authoritative specification for the Aurora programming language. It defines lexical structure, syntax, static and dynamic semantics, type and effect systems, runtime guarantees, and interoperability. The document is self-contained; references to research papers supplement the normative text but do not supersede it.

---
## Table of Contents
1. [Overview](#overview)
2. [Lexical Structure](#lexical-structure)
3. [Syntactic Forms](#syntactic-forms)
4. [Static Semantics](#static-semantics)
   1. [Kinds and Types](#kinds-and-types)
   2. [Effect Rows](#effect-rows)
   3. [Ownership and Borrowing](#ownership-and-borrowing)
   4. [Traits and Generics](#traits-and-generics)
5. [Dynamic Semantics](#dynamic-semantics)
   1. [Evaluation Strategy](#evaluation-strategy)
   2. [Drop Semantics and RAII](#drop-semantics-and-raii)
6. [Concurrency Model](#concurrency-model)
7. [Zones](#zones)
8. [Modules, Packages, and Imports](#modules-packages-and-imports)
9. [Foreign Function Interface (FFI)](#foreign-function-interface-ffi)
10. [Diagnostics and Source Positions](#diagnostics-and-source-positions)
11. [Appendix A — Core Calculus](#appendix-a--core-calculus)
12. [Appendix B — Effect Typing Rules](#appendix-b--effect-typing-rules)
13. [Appendix C — Ownership & Region Rules](#appendix-c--ownership--region-rules)
14. [Appendix D — Worked Examples](#appendix-d--worked-examples)
15. [Appendix E — Glossary](#appendix-e--glossary)

---
## 1. Overview
Aurora is a statically-typed, compiled systems language designed to deliver:
- **Safety**: Ownership and borrowing prevent data races, use-after-free, and unexpected shared mutations.
- **Effect transparency**: Every function’s potential side-effects are tracked by the type system via polymorphic effect rows.
- **Structured concurrency**: Lightweight actors, async/await, and scoped task management guarantee deterministic shutdown and error propagation.
- **Heterogeneous execution**: Zones annotate code for different resource regimes (GPU, realtime, sandboxed, etc.), enforced statically and at runtime.

Aurora programs compile to native code through a sequence of IR stages (AST → HIR → MIR → LLVM). This manual describes the language independent of any specific backend.

---
## 2. Lexical Structure
Aurora source files are Unicode text encoded as UTF-8. Lexical analysis partitions input into tokens:

- **Whitespace**: spaces, horizontal tabs, carriage returns, and line feeds. Whitespace separates tokens but is otherwise ignored.
- **Comments**:
  - Line comments begin with `//` and extend to the end of the line.
  - Block comments begin with `/*` and end with `*/`; they nest.
- **Identifiers**: start with `_` or any Unicode letter; subsequent characters may include digits, underscore, and Unicode combining marks. Identifiers are case-sensitive.
- **Keywords**: reserved words listed below cannot be used as identifiers: `actor`, `async`, `await`, `break`, `continue`, `do`, `else`, `enum`, `effect`, `false`, `fn`, `for`, `if`, `impl`, `in`, `let`, `loop`, `match`, `mod`, `move`, `mut`, `return`, `struct`, `trait`, `true`, `type`, `use`, `where`, `while`, `zone`.
- **Literals**:
  - Integer: decimal (`123`), hexadecimal (`0x7f`), binary (`0b1010`), or octal (`0o755`). Underscores allowed as separators.
  - Floating-point: decimal with optional exponent (`3.14`, `2.0e-3`).
  - Boolean: `true`, `false`.
  - Character: `'a'`, `'\n'`. Unicode scalar values only.
  - String: `"Hello"` with escape sequences (`\n`, `\t`, `\u{1F600}`).
- **Punctuation and Operators**: parentheses `(` `)`, brackets `[` `]`, braces `{` `}`, comma `,`, semicolon `;`, colon `:`, double colon `::`, arrow `->`, fat arrow `=>`, dot `.`, range `..` `..=`, arithmetic operators `+ - * / %`, logical `&& || !`, comparison `== != < <= > >=`, assignment `= += -= *= /= %=`, effect separator `!`, zone attribute `@zone`, lifetime sigil `'`, borrow operators `&` `&mut`.

Tokens are separated by the longest match rule. The scanner returns `Eof` when all input is consumed.

---
## 3. Syntactic Forms
This section gives a prose description of Aurora’s syntactic categories. Formal grammars appear in Task 3’s output; this manual references production names for clarity.

### 3.1 Compilation Units
A source file consists of zero or more *items*: module declarations, use statements, struct/enum definitions, trait and impl blocks, function definitions, zone declarations, and extern blocks. Items may be annotated with attributes (prefixed by `@`). Attribute arguments must be compile-time constant expressions; the compiler folds arithmetic on literals, boolean logic, and simple path references so metadata such as `@zone(realtime(deadline_ms = 1000 * 2))` is resolved during parsing.

### 3.2 Modules and Visibility
```aurora
mod networking {
    use crate::net::TcpStream;
    pub fn connect(endpoint: Endpoint) -> Result<Stream, NetError> { ... }
}
```
- `pub` marks items or fields as visible outside the current module.
- Nested modules map to filesystem hierarchy unless declared inline.
- `use` supports aliasing (`use foo::bar as baz;`) and glob imports (`use foo::*;`).

### 3.3 Types
Type forms include:
- Primitive types `Bool`, `Int{8,16,32,64}`, `UInt{8,16,32,64}`, `Float{32,64}`, `String`, `Unit` (`()`), `Never` (`!`).
- Tuples `(T1, T2, ...)`.
- Arrays `[T; n]` and slices `[T]`.
- Structs `struct Point { x: Float64, y: Float64 }`.
- Enums `enum Option<T> { Some(T), None }`.
- References `&'ℓ T` and mutable references `&'ℓ mut T` (lifetime `'ℓ` may be elided in many contexts).
- Raw pointers `*const T` / `*mut T` (unsafe, no automatic lifetime tracking).
- Trait objects `dyn TraitName` (optionally with generic arguments).
- Function types `fn(T1, T2) -> T3 ! E`, where `E` is an effect row (possibly empty).
- Generic instantiations `Vec<Int32>`, trait bounds `T: Trait`.
- Lifetimes participate in generic parameter lists and bounds (`<'a: 'b>`), and may appear as standalone arguments (`Foo<'a, T>`).

### 3.4 Patterns
Patterns appear in `let` bindings, `match` arms, and `for` loops. Supported forms include:
- Wildcard `_` and literal matches.
- Identifier bindings with optional `mut` and/or `move`, and destructuring via `name @ subpattern`.
- Tuples, structs, enums, and references (`&pat`, `&mut pat`).
- Slice patterns `[head, ..tail, last]` with prefix/suffix/optional rest.
- Alternations `p1 | p2 | ...`, evaluated left-to-right with identical binding sets required by the type checker.
Pattern matching is exhaustive and effect-safe; move bindings transfer ownership, while other patterns borrow or bind by value depending on context.

### 3.5 Expressions
Expressions include literals, variable references, field/project operations, array indexing, function calls, method calls, closures, async blocks, actor literals, and block expressions. `spawn expr` is a prefix form that schedules `expr` on the structured concurrency runtime. Actor literals use `actor Path { field: expr, ... }` and evaluate to a value implementing the actor interface. Control flow expressions: `if`, `while`, `loop`, `for`, `match`, `break`, `continue`, `return`, and `await` compose with these constructs.

### 3.6 Statements
- `let` binding: introduces new variables with optional type annotations.
- Expression statement: evaluated for effects; must end with semicolon unless expression yields unit and semicolon is intentionally omitted.
- Item statement: reuses the full item grammar (e.g., nested `fn`/`impl` inside a block) and is hoisted into the surrounding scope.
- `zone` block: enters a scoped zone with resource constraints.

---
## 4. Static Semantics
Aurora’s static semantics combine Hindley–Milner inference, effect rows with row polymorphism, trait-based constraints, and borrow checking.

### 4.1 Kinds and Types
We adopt a kind system to classify types and effects.
- `*` (type kind) classifies value types.
- `Eff` classifies effect rows.
- `Zone` classifies zone descriptors.
- Type constructors (`Vec`, `Option`, etc.) have higher-kinded signatures.

Type variables `α, β` may be universally quantified, with kind annotations when necessary. Type schemes are written `∀α:*. (C => τ ! ε)` where `C` is a set of trait/effect constraints, `τ` a type, and `ε` an effect row.

### 4.2 Effect Rows
Effects track observable side effects. Base effect labels include `IO`, `Net`, `Time`, `State(Region)`, `Actor(ActorId)`, `Async`, `Gpu`, `Realtime`, `Error(E)`, plus user-defined effect aliases. Effect rows use row-polymorphic lists with optional tail variable `ρ`:
Effect labels may optionally carry arguments, written `Label(expr1, expr2, …)`; these arguments participate in diagnosis but do not alter row equality (they are compared structurally).
- Closed row: `<IO, Error(IOException)>`.
- Open row: `<IO | ρ>`.
Union is commutative and idempotent. Equivalence classes ignore order and duplicates. Effect inference uses constraint solving to unify effect rows, propagate declared effects, and ensure call sites acknowledge all results. See Appendix B for typing rules.

### 4.3 Ownership and Borrowing
Ownership rules mirror affine type systems:
1. Each value has a unique owner; moving transfers ownership.
2. Borrowing obtains references with lifetimes anchored to the owner.
3. Borrow rules:
   - Any number of shared borrows `&T` allowed when no mutable borrow exists.
   - Exactly one mutable borrow `&mut T` allowed, exclusive.
4. Lifetimes are inferred; explicit lifetime parameters express relationships in APIs.
5. Dropping occurs when an owned value goes out of scope and no outstanding borrows exist.
6. Regions are named memory arenas; values allocated inside a region must not escape its lifetime unless transferred via safe abstractions (see Appendix C).

### 4.4 Traits and Generics
- Traits define behaviour contracts. Methods may declare required effect rows.
- Trait bounds appear on type parameters (`fn log<T: Display>(value: T)`), where `Display` may itself constrain effects associated with formatting.
- Associated types/effects allow traits to expose type members and effect aliases.
- Impl blocks implement traits for types, respecting coherence (orphan rules).

---
## 5. Dynamic Semantics
Aurora uses call-by-value semantics with deterministic destruction.

### 5.1 Evaluation Strategy
- Expressions evaluate left-to-right.
- Function application: evaluate callee, then arguments, then enter new frame.
- Async functions return futures; awaiting polls them cooperatively.
- Actor messages are processed sequentially by the owning mailbox task.

### 5.2 Drop Semantics and RAII
- Each owned type may define a `drop` method executed when the value goes out of scope.
- Destruction order is reverse lexical order.
- Drop runs even during unwinding caused by effect propagation (e.g., error effects) unless the effect is declared `@abort` (panic-like) or crosses FFI boundaries without unwind support.

---
## 6. Concurrency Model
Aurora integrates structured concurrency with actors and async/await.

### Actors
- Declared via `actor` keyword or actor struct deriving `Actor` trait.
- Each actor owns a mailbox; sending returns immediately; receiving is sequential.
- Actor effects: `Actor(ActorId)` ensures effect typing tracks message passing.

### Structured Concurrency
- `parallel { ... }` blocks spawn child tasks; block completes when all child tasks resolve or when an error effect terminates the scope.
- Cancellation propagates downward; parents may specify compensation handlers.
- Async tasks integrate with effect system: `async fn foo() -> Result ! Net`.

### Synchronisation
- Provided by standard library (channels, mutexes, condition variables) with effect annotations to expose blocking behaviour.

---
## 7. Zones
Zones partition execution contexts with resource constraints.

### Zone Annotations
```
@zone(gpu(kernel="sgemm"))
fn matmul(a: Buffer<f32>, b: Buffer<f32>) -> Buffer<f32> ! Gpu {
    // ...
}
```
- `@zone(name(args...))` attaches metadata to functions, blocks, or actors.
- Zone descriptors include predefined types: `gpu`, `realtime`, `sandbox`, `io`, `custom`.
- Compiler validates capability tables to forbid disallowed effects (e.g., `Realtime` zone forbids `IO` unless handled).

### Zone Blocks
```
zone realtime(deadline = 1.ms) {
    loop { process_frame().await?; }
}
```
- Enters a child scheduler with defined policies.
- Static analysis ensures region allocations and effect usage conform.
- At module scope, `zone name(args) { ... }` declares a zone region whose body executes at load time and may contain nested item declarations.

---
## 8. Modules, Packages, and Imports
- Module system uses `crate` root; `crate::` references root, `super::` references parent module.
- Packages are built and distributed via `aurorapm`; manifest describes dependencies, zone capabilities, and required toolchain.
- Visibility: `pub(crate)` limited to current package, `pub(super)` to parent module, `pub(zone)` exposes to zone-specific runtime instrumentation.

---
## 9. Foreign Function Interface (FFI)
Aurora interoperates primarily with C.

### Extern Declarations
```
extern "C" {
    fn printf(fmt: *const c_char, ...) -> c_int ! IO;
}
```
- Extern blocks declare functions adhering to specified calling convention (`"C"`, `"C++"` limited subset, `"system"`).
- Types crossing FFI must be `repr(C)` or equivalent.
- Unsafe boundary: calling extern functions requires `unsafe { ... }` block.
- Effect annotations must include all side-effects performed by the foreign call.

### Exporting Aurora Functions
```
#[no_mangle]
pub extern "C" fn aurora_init() -> c_int ! IO {
    0
}
```
- `#[no_mangle]` preserves symbol name.
- Exporters must ensure no unwinding crosses the boundary unless the ABI supports it.

---
## 10. Diagnostics and Source Positions
- Compiler tracks byte offsets, line/column numbers, and file IDs for all AST nodes.
- Diagnostics include severity, message, primary span, secondary notes, and suggestions.
- Effect leaks, borrow violations, and zone capability failures cite relevant code spans with explanations and hints.

---
## 11. Appendix A — Core Calculus
We model Aurora’s core as a typed λ-calculus with regions and effect rows.

### Syntax (core fragment)
```
t ::= x | λx:τ. t | t1 t2 | let x = t1 in t2 | fix x. t | region r in t | borrow^k_{ℓ}(t)
τ ::= α | τ -> τ ! ε | &_{ℓ}^k τ | ∀α. τ | Unit | ...
ε ::= ⟨l1, …, ln | ρ⟩
```
- `k ∈ {shared, mut}` denotes borrow kind.
- `ℓ` denotes lifetime.

### Operational Semantics
- Small-step evaluation relation `⟨σ, t⟩ → ⟨σ', t'⟩` with store `σ` capturing region-allocated values.
- Regions form a stack; `region r in t` pushes new region, evaluating `t`, then pops region (freeing resources).

---
## 12. Appendix B — Effect Typing Rules
Selected typing judgments (`Γ ⊢ t : τ ! ε`).

1. **Function Introduction**
```
Γ, x:τ1 ⊢ t : τ2 ! ε
——————————————— (T-Lam)
Γ ⊢ λx.t : τ1 -> τ2 ! ε
```
2. **Function Application**
```
Γ ⊢ t1 : τ1 -> τ2 ! ε1   Γ ⊢ t2 : τ1 ! ε2
ε = ε1 ∪ ε2
————————————————————————— (T-App)
Γ ⊢ t1 t2 : τ2 ! ε
```
3. **Let Binding**
```
Γ ⊢ t1 : τ1 ! ε1   Γ, x:τ1 ⊢ t2 : τ2 ! ε2
ε = ε1 ∪ ε2
————————————————————————— (T-Let)
Γ ⊢ let x = t1 in t2 : τ2 ! ε
```
4. **Effect Annotation**
A function declaration `fn f(...) -> τ ! ε_decl { body }` enforces constraint `ε_body ⊆ ε_decl`. Violations yield compile-time errors.

5. **Effect Polymorphism**
Generalisation introduces fresh row variables for unconstrained effects; instantiation unifies rows via solver substitution.

---
## 13. Appendix C — Ownership & Region Rules
Core rules for borrowing:

1. **Shared Borrow**
```
Γ ⊢ v : τ owned, no mutable borrow outstanding
ℓ fresh
————————————————————————————
Γ ⊢ &v : &'ℓ τ ! ε_shared
```
2. **Mutable Borrow** analogous but ensures exclusivity. Region rule prevents escaping references:
```
Γ, r:Region ⊢ t : τ ! ε   τ contains no references to r
—————————————————————————————————————————
Γ ⊢ region r in t : τ ! ε
```
Violations produce diagnostic referencing lifetimes.

---
## 14. Appendix D — Worked Examples

### D.1 Hello World with Effects
```aurora
fn main() -> Unit ! IO {
    println("Hello, Aurora!");
}
```
- `println` declares `IO` effect; compiler enforces callers handle or propagate.

### D.2 Borrowed Vectors
```aurora
fn sum(values: &[Int32]) -> Int32 ! ∅ {
    let mut total = 0;
    for v in values {
        total += *v;
    }
    total
}
```
- Shared borrow ensures iteration is read-only.

### D.3 Actor Pipeline
```aurora
actor Logger {
    async fn run(mut self, mut inbox: Inbox<String>) -> Unit ! IO {
        while let Some(msg) = inbox.recv().await {
            write_log(msg).await?; // effect IO
        }
    }
}

fn launch() -> Unit ! Actor(LoggerId) + IO {
    let logger = spawn(Logger::new());
    logger.send("started");
    parallel {
        produce_work(logger.clone());
        consume_results(logger);
    }
}
```
- Demonstrates actor effect tagging and structured concurrency.

### D.4 GPU Zone Example
```aurora
@zone(gpu(kernel = "sgemm"))
fn matmul(a: Buffer<f32>, b: Buffer<f32>) -> Buffer<f32> ! Gpu {
    // invocation lowered to GPU runtime, forbidden effects IO/Realtim
}
```

---
## 15. Appendix E — Glossary
- **Actor**: Single-threaded concurrent entity with a message queue.
- **Effect row**: Multiset of effect labels describing side-effects.
- **Region**: Scoped allocator with compile-time lifetime guarantees.
- **Zone**: Execution context with resource and capability constraints.
- **Borrow**: Temporary reference to owned data with explicit lifetime semantics.

---
*Future revisions will integrate full EBNF listings (Task 3), link to the formal calculus mechanisation, and include extended examples validated by the compiler prototype.*
