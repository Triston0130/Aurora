# Automation Plan

The self-assessment suite focuses on three pillars:

1. **Environment & Build Health** – run the same checks as CI, ensuring new
   contributors encounter no surprises (`AURORA_CI_MODE=lint ./tools/ci.sh`).
2. **Sample Workflow Validation** – compile and run representative programs
   (web server, GPU filter, realtime controller) to ensure documentation and
   tooling remain accurate.
3. **Documentation & Diagnostics Audit** – scan for broken links, missing code
   snippets, and lingering TODO markers.

All tasks are orchestrated by `tools/usability-self-assess.sh`, which produces a
Markdown summary containing:

- Command transcript and exit status
- Timing information for major steps
- Detected warnings (lint/doc scanners)
- Links to generated artefacts (e.g., benchmark collations)

The script is idempotent and designed to run unattended (e.g., on a nightly
cron job or as part of CI).

## Output

Executed runs create logs under `research/usability/automation/output/` with a
filename pattern `YYYYMMDD-HHMMSS.log`. A symlink `latest.log` always points to
the most recent run.

## Extending the Suite

1. Add new commands to `tools/usability-self-assess.sh`.
2. Update this README with the metric rationale.
3. If logs require post-processing, add a helper under
   `research/usability/automation/scripts/` (future work).
