# Talk Outline: Aurora – Effectful Systems Programming Across Zones

**Duration:** 30 minutes

## 1. Motivation (5 min)
- Heterogeneous workloads spanning CPU, GPU, realtime controllers.
- Pain points: fragmented APIs, weak safety guarantees, duplicated tooling.

## 2. Language Foundations (7 min)
- Ownership + effect typing overview.
- Regions and hand-offs with code examples.
- Trait solver and diagnostics.

## 3. Runtime Architecture (7 min)
- Structured concurrency supervisors.
- Zone manager capabilities (GPU, realtime, sandboxed I/O).
- Memory and I/O services coordinating through the scheduler.

## 4. Developer Experience (5 min)
- Standard library highlights.
- Tooling suite demo (aurorapm, aurorafmt, auroralint, aurorals, auroradebug).
- CI workflows and contribution process.

## 5. Case Studies (3 min)
- Web server sample (CPU zone + async IO).
- GPU image filter pipeline.
- Realtime controller with deadlines.

## 6. Roadmap & Call to Action (3 min)
- Upcoming milestones (Tasks 26–27).
- Research collaborations and publication plans.
- How to get involved (issues, design records, outreach materials).

Prepare live demos using the examples and the `aurora_cli` driver; keep a backup
recording of each zone scenario in case of technical issues.
