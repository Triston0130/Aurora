# Aurora Parser vs. LRM/EBNF Audit (Item 1)

_Date: 2024-03-17_

This document records the gaps between the **Aurora Language Reference Manual** (`docs/spec/language_reference.md`) / **grammar draft** (`docs/spec/grammar.ebnf`) and the current prototype parser/AST implementation in `compiler/prototypes/src`. It will serve as the backlog for parser feature work (Item 1 of the end-to-end plan).

## Legend
- **Spec** — production or section in the published grammar/LRM.
- **Status** — ✔ implemented, ✖ missing, △ partial/needs refinement.
- **Notes** — observations, required work, or file/line references.

## 1. Items & Modules
| Spec | Status | Notes |
| --- | --- | --- |
| `Item` includes `ZoneDecl` | △ | Parser recognizes `zone` items and captures args/body; visibility modifiers still rejected and semantics to be wired later. |
| Nested modules & `use` grammar (`crate`, `self`, `super`, leading `::`, glob) | ✔ | `parse_use_decl` handles brace groups (e.g. `use foo::{bar, baz::*};`), reserved heads, leading `::`, globs, and aliases. |
| Attributes accept expression arguments | △ | Parser now accepts arbitrary expressions and `key = value` pairs; semantic interpretation (e.g., constant folding) still pending. |
| Zone attribute/declaration arguments (key-value expressions) | △ | Parser hands expressions through; runtime only accepts simple literals/idents (others rejected). |

## 2. Generic Parameters & Lifetimes
| Spec | Status | Notes |
| --- | --- | --- |
| Lifetime parameters (`'a`) and lifetime lists | △ | Lifetimes lexed and stored in `GenericParam`; lifetime bounds like `'a: 'b` still pending. |
| Function parameters as full patterns | ✖ | `parse_function_signature` insists on identifier names, not general `Pattern`. |
| Higher-kinded bounds & `where` clauses | △ | Basic `T: Trait` and equality parsed, but no support for `T: Trait1 + Trait2 where` chaining beyond simple list; revisit once trait solver lands. |

## 3. Patterns
| Spec | Status | Notes |
| --- | --- | --- |
| Pattern alternation (`p1 | p2`) | ✔ | Parser/AST emit `Pattern::Or`; inference validates each branch. |
| Slice patterns (`[a, b, ..]`) | ✔ | Parser captures prefix/rest/suffix; inference introduces element type vars. |
| Binding patterns `name @ pattern` | ✔ | Represented via `Pattern::Binding`; environment insertions handled. |
| `move` patterns | ✔ | `move` keyword wraps patterns with ownership transfer semantics. |

## 4. Expressions
| Spec | Status | Notes |
| --- | --- | --- |
| Await expressions | ✔ | Both postfix (`future.await`) and prefix (`await future`) forms map to `Expr::Await`. |
| Actor literals (`actor Foo { ... }`) | △ | Parsed into `Expr::Actor` and carried through HIR; MIR/runtime lowering still a stub. |
| `spawn` expression | △ | Parsed as a prefix unary form and lowered as a runtime call; scheduling semantics pending. |
| `zone IDENT(args) block` expression | △ | Parsed into `Expr::Zone` with args/body; runtime semantics pending. |
| `move expr` | ✖ | Not recognized. |
| `zone` postfix operator (entering zone in expression context) | ✖ | Missing. |
| `return` / `break` / `continue` as expressions | ✖ | Treated as block-only statements. |

## 5. Effect Rows & Zones
| Spec | Status | Notes |
| --- | --- | --- |
| Effect rows `<E1, E2 | ρ>` with label arguments | △ | Labels now carry expression arguments; solver currently ignores argument payloads. |
| Associated effect aliasing (`effect Output = <IO | ρ>`) | △ | Syntax accepted, but RHS limited by effect-row parser gap. |
| Zone statements/blocks | △ | Parsed and analysed; capability enforcement still minimal. |

## 6. Statements & Blocks
| Spec | Status | Notes |
| --- | --- | --- |
| Items inside blocks (`Statement = Let | Item | Expr`) | △ | Nested functions/zone decls parsed and wired into inference; other item kinds still ignored. |
| `zone` scoped statements | △ | Parsed via `Expr::Zone` in expression statements; enforcement/semantic checks outstanding. |
| `while let` / `if let` sugar | ✖ | Unsupported. |

## 7. Types
| Spec | Status | Notes |
| --- | --- | --- |
| Lifetimes in reference types (`&'a T`) | △ | References now accept optional lifetime names; lifetime inference/enforcement still TODO. |
| Trait objects `dyn Trait` | ✔ | `parse_type` recognizes `dyn`; inference records the path + generics. |
| Raw pointers `*const T`, `*mut T` | ✔ | Pointer syntax parsed and lowered to `Type::RawPointer`. |
| Function types with effect clause | △ | Syntax accepted, but effect-row limitations remain. |
| Trait references with generic args (`Trait<'a, T>`) | △ | Lifetime arguments parsed; constraint solver still ignores variance relationships. |

## 8. Lexical & Tokens
| Spec | Status | Notes |
| --- | --- | --- |
| Unicode identifiers | ✖ | Lexer currently restricts to ASCII alphanumeric/underscore. |
| Lifetime tick (`'`) tokenization | ✔ | Lexer emits dedicated lifetime tokens (including `'static`). |
| Range operator `..=` | ✔ | Added to operator table alongside `..`/`...`. |
| Keyword parity (`actor`, `spawn`, `do`, etc.) | ✖ | Lexer still trails the reserved list (recently added `as`, more remain). |

## 9. Diagnostics
| Spec | Status | Notes |
| --- | --- | --- |
| Spanful parse errors | ✖ | `ParseError` carries expected/found but no source spans. |
| Multi-note diagnostics | ✖ | Single error message only. |

## 10. Snapshot Coverage
| Spec | Status | Notes |
| --- | --- | --- |
| Example coverage for all constructs | △ | Snapshot suite exercises zones and pattern forms; failure cases still outstanding. |

---

### Immediate Follow-Up (for Plan Item 1b/1c)
1. Decide interim language subset vs. spec alignment; document any intentional deviations.
2. Prioritize upcoming parser enhancements (type pointer/trait-object grammar, lifetime bounds) and update lexer accordingly.
3. After each parser addition, extend snapshot suite with minimal positive/negative examples and refresh spec text.
