# Aurora Implementation Research Links

The following peer-reviewed papers anchor specific design decisions or future-facing questions for Aurora's compiler and runtime strategy.

1. **Daan Leijen. "Koka: Programming with Row Polymorphic Effect Types." EPTCS 153 (2014), pp. 100-126. [arXiv:1406.2061](https://arxiv.org/abs/1406.2061).**
   - Validates our use of row-polymorphic effect rows (see `effect-solver` crate) and motivates the union + substitution machinery we now thread through HIR/MIR.

2. **Daan Leijen. "Type Directed Compilation of Row-Typed Algebraic Effects." POPL 2017.**
   - Guides how effect rows can be lowered into explicit runtime data while preserving inference results—relevant as we extend MIR lowering beyond simple calls.

3. **G. Plotkin and M. Pretnar. "Handlers of Algebraic Effects." ESOP 2009.**
   - Establishes algebraic effect semantics that align with our structured diagnostics and future effect handler representation.

4. **M. Tofte and J.-P. Talpin. "Implementation of the Typed Call-by-Value λ-Calculus using a Stack of Regions." POPL 1994.**
   - Directly informs Aurora's zone/region model, especially our plan to tag pointers with region lifetimes in the type system.

5. **Trevor Jim, Greg Morrisett, Dan Grossman, et al. "Cyclone: A Safe Dialect of C." USENIX 2002.**
   - Provides the practical precedent for combining region inference with safe pointers—the blueprint for our region-capability validation.

6. **Ralf Jung, Jacques-Henri Jourdan, Robbert Krebbers, Derek Dreyer. "RustBelt: Securing the Foundations of the Rust Programming Language." POPL 2018.**
   - Supplies the semantic justification we need when threading borrow-checked types from constraints into MIR and, later, into unsafe FFI boundaries.

7. **Ralf Jung et al. "Stacked Borrows: An Aliasing Model for Rust." POPL 2020.**
   - Inspires how we might model aliasing rules when we introduce mutable references and unsafe escapes into Aurora's MIR.

8. **Chris Lattner et al. "MLIR: A Compiler Infrastructure for the End of Moore's Law." arXiv:2002.11054 (2020).**
   - Shapes our multi-level IR roadmap (HIR → MIR → LLVM) and confirms the value of dialect-like layering for zones (GPU, realtime) before LLVM lowering.

9. **Chris Lattner and Vikram Adve. "LLVM: A Compilation Framework for Lifelong Program Analysis & Transformation." CGO 2004.**
   - Underpins our assumption that LLVM is the ultimate codegen target once MIR is stabilized, especially for zone metadata via function attributes.

10. **Gul Agha. "Actors: A Model of Concurrent Computation in Distributed Systems." MIT Press, 1986.**
    - Grounds Aurora's actor runtime: message-passing semantics and single-threaded mailboxes map onto our zone-aware scheduler roadmap.

11. **Perceus Team (Leijen, et al.). "Perceus: Garbage Free Reference Counting with Reuse." PLDI 2021.**
    - Critical for our optional cycle-detecting reference counting story and motivates optimizations before MIR lowering (eliminate redundant inc/dec pairs).

12. **Danel Ahman, James McKinna, and Claudio Russo. "Structured Concurrency for Effect Handlers." ECOOP 2022 (LIPIcs 222).**
    - Reinforces the structured concurrency effect typing we propagate and hints at how MIR should represent cancellation/capability boundaries.

13. **Stephen Dolan, Leo White, KC Sivaramakrishnan. "Concurrent System Programming with Effect Handlers." ICFP 2017.**
    - Offers operational insight into lowering async/await and actor interactions into state machines—informing future MIR transformations.

These references guide the next implementation phases: richer effect solving, MIR control-flow generalization, borrow-aware optimizations, and zone-specific backends.
