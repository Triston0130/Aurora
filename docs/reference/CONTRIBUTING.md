# Contributing to Aurora

Thank you for your interest in shaping the Aurora Programming Language. This guide documents our expectations for contributions of any form—code, documentation, research, design artefacts, or community support.

## 1. Core Principles
1. **Soundness before convenience**
   - Changes must preserve or improve the language’s safety guarantees (ownership, effect typing, structured concurrency).
2. **Research-grade transparency**
   - Every contribution should reference prior work, experiments, or design notes whenever possible.
3. **Teaching mindset**
   - Code, docs, and tooling should help others learn; favour clarity over cleverness.

## 2. Code of Conduct
Aurora follows the [Contributor Covenant v2.1](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By participating, you agree to uphold our commitment to respectful, inclusive collaboration. Report issues to the language team leads listed in `docs/GOVERNANCE.md`.

## 3. Getting Started
1. Read the README and roadmap to understand the project direction.
2. Review the numbered tasks in `docs/TODO.md` to see active priorities.
3. Join the Aurora Zulip/Discord (links forthcoming) and introduce yourself.
4. Fork the repository, create a feature branch, and ensure your environment meets the prerequisites in `docs/reference/DEV_SETUP.md` (to be published).

## 4. Contribution Workflow
1. **Issue discovery**
   - Search existing issues/PRs to avoid duplication.
   - If opening a new issue, reference the task number from `docs/TODO.md` and include motivation, context, and acceptance criteria.
2. **Design discussion**
   - Significant changes require an RFC (see `docs/RFC_PROCESS.md`). Minor fixes can proceed after a lightweight discussion in issue comments or the #implementation channel.
3. **Development**
   - Follow the style guide (pending) and ensure code is documented and tested.
   - Run `cargo fmt`, `cargo clippy`, and `cargo test --workspace` before pushing.
4. **Pull request**
   - Title prefix: `[Task #N] Summary` to map back to the master checklist.
   - Provide a concise summary, motivation, design rationale, tests performed, and follow-up tasks.
   - Link the PR to the relevant issue and task number.
5. **Review**
   - Expect at least two approvals for substantial changes (compiler/runtime/tooling) and one approval for docs/tests.
   - Address review feedback with follow-up commits; squash before merge unless history is instructive.
6. **Merge**
   - Maintainers use “Rebase and merge” to keep history linear.

## 5. Testing Expectations
- Unit tests for new modules/functions.
- Integration tests for end-to-end flows (compiler → MIR → runtime).
- Property/fuzz tests when applicable (lexer/parser/borrow checker).
- Benchmarks for performance-sensitive changes (runtime, MIR passes).

## 6. Documentation Expectations
- Update relevant docs (LRM, Aurora Book, API references) when behaviour changes.
- Example code must compile with the latest toolchain.
- Add changelog entries for user-facing updates.

## 7. Release Readiness Checklist
Before a milestone release, contributors should:
1. Ensure their features are covered by docs and automated tests.
2. Confirm failure modes are documented and produce actionable diagnostics.
3. Participate in the release retrospective to capture lessons learned.

## 8. Recognition
We celebrate contributions via:
- Monthly contributor roundups in the Aurora newsletter.
- Attribution in release notes and the AUTHORS file (to be added).
- Opportunities to co-author blog posts, talks, and academic papers.

## 9. Questions & Support
- Join the community chat.
- Email the core team (see `docs/GOVERNANCE.md`).
- File an issue with `[Question]` in the title for asynchronous responses.

We’re building Aurora intentionally and with care—thank you for helping shape its future.
