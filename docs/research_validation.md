# Task 26: Research Validation Plan

Task 26 requires three complementary strands of evidence before Aurora can be
considered production-ready:

1. **Mechanised proofs / formal verification** of the core calculus.
2. **Empirical performance & safety studies** exercising the runtime.
3. **Developer usability feedback** gathered from structured studies.

This document summarises the deliverables, tooling choices, and owners for each
strand. Detailed artefacts live under `research/`.

## 1. Core Calculus Verification

- **Target:** The effect-aware, region-annotated lambda calculus used in the
  `compiler/prototypes` passes.
- **Proof System:** Lean 4 (preferred) with an alternative Coq path if Lean
  automation proves insufficient.
- **Artefacts:** See `research/proofs/core_calculus/` (Lean project scaffold with
  syntax/typing/reduction modules and placeholder soundness theorems).
- **Milestones:**
  1. Encode syntax + typing rules (`lean/src/syntax.lean`).
  2. Prove progress/preservation theorems for the core calculus.
  3. Extend with effect-row lemmas and zone capability constraints.

## 2. Empirical Performance & Safety Studies

- **Benchmark Harness:** Cargo crate under `research/benchmarks/` using Criterion.
- **Implemented Scenarios:**
  - `actor_spawn_join` (scheduler throughput)
  - `zone_handoff` (region transfer engine)
  - `timer_wheel` (batch timer scheduling)
  - `ffi_roundtrip` (libc `strlen` via FFI manager)
- **Workloads:** Actor pipeline, GPU blur, realtime control loop, FFI boundary.
- **Reporting:** Benchmarks produce JSON + Markdown summaries stored in
  `research/benchmarks/results/` (to be generated).
- **Safety Checks:** Each benchmark verifies invariants (no leaked regions,
  deadlines met) and logs violations for later analysis.

## 3. Developer Usability Feedback

- **Study Design:** Survey + task-based usability sessions documented in
  `research/usability/survey_outline.md`.
- **Participants:** Minimum of 8 practitioners (systems, PL, and GPU engineers).
- **Instruments:**
  - Pre-study survey (background, experience).
  - Task scripts (build/run sample, extend zone policy, write effect handler).
  - Post-study questionnaire capturing SUS scores and qualitative feedback.
- **Analysis:** Compile findings in `research/usability/findings.md` after each
  wave.

## Integration & Reporting

- Update this plan as artefacts land.
- Publish periodic progress notes in `research/notes/`.
- Reference this document from PRs targeting Task 26 deliverables.

Once the mechanised proofs, benchmark reports, and usability findings are
complete (with artefacts committed and summarised), Task 26 can be marked done.
