# Aurora Language Reference Manual (Draft)

> **Revision**: 2025-02-14 (bootstrap draft)
> **Status**: Sections 2–5 seeded for authoring; remaining chapters retain skeletal headings.

## 1. Introduction
- Scope and goals
- Design principles
- Notational conventions

## 2. Lexical Structure

### 2.1 Character Set
Aurora source files are encoded in UTF-8. The core grammar restricts identifiers and keywords to ASCII for portability; future revisions may permit selected Unicode categories.

### 2.2 Tokens
Tokens are produced via maximal munch. The following categories are recognised:

| Category      | Examples / Definition |
|---------------|------------------------|
| `keyword`     | `fn`, `let`, `mut`, `struct`, `enum`, `actor`, `async`, `await`, `effect`, `zone`, `region`, `if`, `else`, `match`, `for`, `while`, `loop`, `return`, `break`, `continue`, `impl`, `trait`, `spawn`, `parallel`, `try`, `catch` |
| `identifier`  | `ID ::= XID_START XID_CONT*` (letters `_` and digits, excluding keywords) |
| `integer`     | `INT ::= DIGIT+` with optional `_` separators |
| `float`       | `FLOAT ::= DIGIT+ '.' DIGIT+ (EXPONENT)?` |
| `string`      | `"` ... `"` with escapes `\n`, `\t`, `\u{FFFF}` |
| `char`        | `'` ... `'` single Unicode scalar |
| `boolean`     | `true`, `false` |
| `operator`    | `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`, `=`, `=>`, `->`, `::`, `:`, `;`, `,`, `.`, `..`, `...`, `?` |
| `delimiter`   | `(`, `)`, `{`, `}`, `[`, `]` |
| `comment`     | Line `// ...` or block `/* ... */` (nestable) |

Whitespace and comments separate tokens but otherwise ignored by the parser.

### 2.3 Numeric Literals
- Integer suffixes: `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `isize`, `usize`.
- Float suffixes: `f32`, `f64`.
- Binary/hex literals: `0b[01_]+`, `0x[0-9A-Fa-f_]+`.

### 2.4 Identifier Grammar
```
XID_START ::= '_' | [A-Za-z]
XID_CONT  ::= XID_START | [0-9]
```
Reserved identifiers (keywords) cannot be reused.

## 3. Grammar and Syntax

### 3.1 Compilation Units
```
CompilationUnit ::= Item*
Item            ::= FunctionDecl
                 | StructDecl
                 | EnumDecl
                 | TraitDecl
                 | ImplBlock
                 | ActorDecl
                 | UseDecl
                 | ModDecl

UseDecl        ::= 'use' Path ('as' Identifier)? ';'
ModDecl        ::= 'mod' Identifier (';' | Block)
Path           ::= Identifier ('::' Identifier)*
```

### 3.2 Expressions (excerpt)
```
Expr ::=
    Literal
  | Path
  | Expr '(' ArgList? ')'
  | 'let' Pattern '=' Expr
  | 'if' Expr Block ('else' Block)?
  | 'match' Expr '{' MatchArm+ '}'
  | 'async' Block
  | 'spawn' Expr
  | 'await' Expr
  | Expr '!' EffectSet        // effect cast
  | Expr '?'                  // error propagation sugar
  | Block

Block ::= '{' Statement* Expr? '}'
```

### 3.3 Patterns
```
Pattern ::= '_'
          | Identifier
          | Literal
          | '&' Pattern
          | StructPattern
          | EnumPattern
          | TuplePattern

StructPattern ::= Path '{' PatternField (',' PatternField)* (',')? '}'
PatternField  ::= Identifier ':' Pattern | Identifier
EnumPattern   ::= Path '(' PatternList? ')' | Path StructPatternBody?
TuplePattern  ::= '(' Pattern (',' Pattern)+ ')' | '('

MatchArm      ::= Pattern Guard? '=>' (Expr | Block)
Guard         ::= 'if' Expr
```

Full EBNF, including statements, loops, and actor handlers, will be appended in Appendix A once the parser prototype stabilises.

## 4. Type System

### 4.1 Type Forms
- Primitive: `Bool`, `Int{8,16,32,64}`, `UInt{8,16,32,64}`, `Float{32,64}`, `Str`, `Char`.
- Composite: tuples `(T1, T2, ...)`, arrays `[T; n]`, slices `[T]`, function types `fn<T>(Args) -> Ret ! Effects`.
- Algebraic data types: `struct`, `enum`.
- Generic parameters with trait bounds: `T: Trait + ?Sized`.
- Reference types: `&'a T`, `&'a mut T`.
- Smart pointers: `Box<T>`, `Shared<T>`, `Weak<T>` (core library definitions but part of type system when resolved).

#### 4.1.1 Type Expression Grammar
```
Type           ::= PathType | TupleType | ArrayType | FunctionType | ReferenceType | PointerType | NeverType
PathType       ::= Path GenericArgs?
TupleType      ::= '(' (Type (',' Type)+)? ')'
ArrayType      ::= '[' Type ';' IntegerLiteral ']'
FunctionType   ::= 'fn' GenericParams? '(' ParamTypeList? ')' ReturnType EffectClause?
ParamTypeList  ::= Type (',' Type)* (',')?
ReturnType     ::= '->' Type
ReferenceType  ::= '&' Lifetime? 'mut'? Type
PointerType    ::= '*' ('const' | 'mut') Type
NeverType      ::= '!'
GenericArgs    ::= '<' Type (',' Type)* '>'
```

### 4.2 Typing Judgements
We adopt standard notation `Γ ⊢ e : τ ! ε` meaning expression `e` has type `τ` under context `Γ` producing effect set `ε`.

Key inference rules (to be proven sound):

```
(T-Var)      Γ(x) = τ
             ----------------------
             Γ ⊢ x : τ ! ∅

(T-Let)      Γ ⊢ e1 : τ1 ! ε1    Γ, x:τ1 ⊢ e2 : τ2 ! ε2
             ------------------------------------------
             Γ ⊢ let x = e1; e2 : τ2 ! (ε1 ∪ ε2)
```

Effect rows are modelled as finite sets with row variables; unification extends Algorithm W with effect constraints.

Additional rules (selection):

```
(T-Assign)   Γ ⊢ place : &'a mut τ ! εp    Γ ⊢ value : τ ! εv
             -----------------------------------------------
             Γ ⊢ place = value : Unit ! (εp ∪ εv)

(T-Return)   Γ ⊢ e : τ ! ε
             ------------------------
             Γ ⊢ return e : !

(T-Closure)  Γ, x:τx ⊢ body : τ ! ε
             -----------------------------
             Γ ⊢ |x| -> τ ! ε { body } : (τx → τ ! ε)
```

**Example**: `fn make_adder(delta: Int32) -> fn(Int32) -> Int32 { move |x| { x + delta } }` produces a closure typed `fn(Int32) -> Int32 ! ∅` because the body is pure and captures `delta` by move without additional effects.


### 4.3 Trait and Constraint Solving
- Constraint environment `Δ` maintains obligations `τ : Trait<…>`.
- Solver integrates with effect analysis so traits can express effect capabilities (`trait IO { effect IO }`).

Resolution proceeds via:
1. **Canonicalisation**: normalise type/effect expressions removing superficial differences (alpha-renaming lifetimes/effect variables).
2. **Goal Stack Evaluation**: iteratively attempt to satisfy goals using trait impls, accumulating nested obligations.
3. **Coherence Enforcement**: prevent overlapping impls unless explicitly marked for specialisation; orphan rules mirror Rust’s coherence policy.
4. **Effect Capability Check**: ensure traits that declare `effect` clauses contribute those effects to the caller’s obligations; failure yields compile-time diagnostics with suggested handlers.

### 4.4 Lifetimes and Regions
- Lifetime parameters `'a`, `'static` tracked via outlives relations `Γ ⊢ 'a :> 'b`.
- Region annotations `region('r)` associated with allocations; region subtyping ensures `region('r1) ≤ region('r2)` iff `'r1` outlives `'r2`.

#### 4.4.1 Borrow Checking Rules
- At most one mutable borrow or any number of immutable borrows may be active simultaneously for a given resource.
- Moves invalidate previous bindings unless the type implements `Copy`.
- Borrows extend until last use (non-lexical lifetimes) where possible to reduce artificial conflicts.
- Destructors (`Drop::drop`) run deterministically at scope end; effect system tracks `Drop` effects for potential panics.

#### 4.4.2 Region Semantics
- `region ('r) { ... }` introduces arena allocation for objects created within the block.
- Values allocated in a region must not escape (`&'r T` references cannot outlive `'r`).
- Region polymorphism: functions may quantify over regions `for<'r>` enabling generic APIs (e.g., parsers that work within caller-provided pools).
- Region subtyping corresponds to lifetime outlives: if `'r1 :> 'r2`, then `region('r1)` encompasses `region('r2)`.

## 5. Effect System

### 5.1 Effect Syntax
```
EffectSet ::= '∅'
            | Identifier
            | EffectSet '|' EffectSet
            | '[' EffectElem (',' EffectElem)* ']'
EffectElem ::= Identifier ( '<' TypeArgs '>' )?
```
Convenience syntax `fn foo() ! IOError | NetworkError` is sugar for `fn foo() ! [IOError, NetworkError]`.

### 5.2 Effect Judgements
Typing judgements carry effect sets. Representative rules:

```
(E-Call)   Γ ⊢ f : (τ1 → τ2 ! εf) ! εf'
           Γ ⊢ arg : τ1 ! εarg
           ---------------------------------
           Γ ⊢ f(arg) : τ2 ! (εf ∪ εf' ∪ εarg)

(E-Try)    Γ ⊢ e : τ ! ε
           ε ⊆ εhandled ∪ εprop
           ---------------------------------
           Γ ⊢ try e catch {…} : τ' ! εprop
```

### 5.3 Effect Polymorphism
Effect variables `ϵ` permit abstracting over effects. Example signature:
```
fn compose<A, B, C, ϵ1, ϵ2>(f: fn(A) -> B ! ϵ1, g: fn(B) -> C ! ϵ2) -> fn(A) -> C ! (ϵ1 | ϵ2)
```
Row polymorphism ensures effect sets are order- and duplicate-insensitive.

During inference, the solver produces a substitution map `σ` from effect variables to canonical rows. Effect generalisation follows Hindley–Milner style: only unconstrained effect variables are generalised at `let` bindings.

### 5.4 Interaction with Concurrency
- `async fn` implicitly introduces effect `Async` unless proven pure.
- Actor message handlers declare effects describing message processing side-effects; runtime supervision enforces unhandled effects cause actor restart.
- Cancellation modelled as effect `Cancel`; structured concurrency ensures cancellation either handled locally or propagates via effect annotations.

## 6. Memory Model
Aurora guarantees freedom from data races, use-after-free, and dangling references through compile-time ownership and borrowing analysis augmented with regions.

### 6.1 Ownership and Moves
- Every value has a unique owner responsible for destruction.
- Move semantics transfer ownership: after `let b = a;`, `a` cannot be used unless it implements `Copy`.
- Types implementing `Drop` run custom destructors when scope ends; unwinding due to effects still triggers destructors in reverse order.

### 6.2 Borrowing
- Immutable borrow `&T` permits multiple concurrent readers.
- Mutable borrow `&mut T` grants exclusive access; compiler rejects overlapping borrows.
- Borrow lifetimes inferred; non-lexical lifetime analysis shortens borrow scopes to last use.
- Reborrowing rules: `&mut T` may temporarily produce a narrower `&T` borrow provided no mutation occurs during that window.

### 6.3 Regions
- Region blocks `region ('pool) { ... }` allocate objects within an arena freed wholesale at scope exit.
- Compiler enforces that references or pointers annotated with a region lifetime cannot escape the region.
- Region polymorphism enables APIs to abstract over caller-provided arenas for deterministic allocation patterns.
- Region effects may be tracked (`Alloc<'pool>`) allowing effect system to reason about allocation footprints.

### 6.4 Smart Pointers and Optional GC
- `Box<T>`: unique-ownership heap allocation with deterministic drop.
- `Shared<T>`: reference-counted pointer with compile-time choice between atomic/non-atomic increments based on thread escape analysis.
- `Weak<T>`: non-owning pointer preventing reference cycles; downgrade and upgrade APIs follow Option semantics.
- Optional cycle detector or tracing collector may run for designated zones/types; effect system tracks `Gc` effect when collectors may execute.

### 6.5 Unsafe Operations
- `unsafe` blocks unlock manual pointer manipulation but require proof obligations (documented in safety comments) that aliasing/validity invariants hold.
- FFI boundaries must convert raw pointers into safe abstractions, ensuring lifetimes/ownership honoured.

## 7. Concurrency Model
- Actor semantics and message passing
- Structured concurrency constructs
- Async functions and awaitables
- Cancellation and error propagation

## 8. Zones and Resource Awareness
- Zone declaration syntax
- GPU offload semantics
- Real-time guarantees and restrictions
- Resource typing extensions

## 9. Compile-Time Metaprogramming
- Macro system (hygienic + procedural)
- Compile-time function evaluation
- Reflection APIs

## 10. Error Handling
- Effect-based errors
- Panic and abort semantics
- Interoperability with host errors (FFI)

## 11. Standard Library Contracts
- Core module overview
- Naming conventions
- API stability guarantees

## 12. Foreign Function Interface
- C interoperability model
- Memory ownership across FFI boundaries
- Safety guidelines

## 13. Tooling Integration
- Package manifest format
- Build pipeline expectations
- Language server capabilities

## 14. Diagnostics and Reporting
- Error message structure
- Lints and compiler hints
- Debug info conventions

## Appendices
- Formal typing and effect rules
- EBNF grammar listing
- Glossary of terms
- Revision history

---
*Prepared for Phase 1 authoring. Contributions welcome via RFC process.*
