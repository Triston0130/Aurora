# Developer Usability Study Outline

This document describes the planned usability study for Aurora tooling and
language ergonomics.

## Objectives

1. Assess how quickly developers can set up the workspace and run examples.
2. Measure the clarity of documentation (Aurora Book, README, API overview).
3. Evaluate ergonomics of zones/effects in small coding tasks.

## Participant Profile

- 8–10 engineers with backgrounds across systems, PL, and GPU programming.
- Mix of Rust/Go/C++ experience; at least 3 with formal methods exposure.

## Session Structure (90 minutes)

1. **Pre-study survey (10 min):** Background, comfort with concurrency/PL.
2. **Onboarding task (20 min):** Clone repo, run `./tools/ci.sh`, execute web server example.
3. **Language task (25 min):** Implement a simple effect handler or modify zone policy following documentation.
4. **Tooling task (20 min):** Use `aurorafmt`, `auroralint`, and `aurorals` to address provided issues.
5. **Debrief & post-study survey (15 min):** SUS questionnaire, open feedback.

## Data Collection

- Screen recordings (with consent) or detailed moderator notes.
- Time-on-task, task completion rates, error counts.
- SUS (System Usability Scale) scores and qualitative quotes.

## Logistics

- Conduct sessions remotely over video conferencing; share cloud dev environment if needed.
- Provide participants with the `Participant Guide` (to be written) ahead of time.
- Offer honoraria/gift cards for completed sessions.

## Analysis Plan

- Compute SUS scores per participant and overall.
- Thematically code qualitative feedback (affinity diagramming).
- Summarise blockers, documentation gaps, and tooling requests in
  `research/usability/findings.md`.

Update this outline as tasks or tooling evolve.
