# Automated Developer Experience Assessment

Task 26 now relies on a self-sustaining, automated evaluation pipeline rather
than external participant studies. The goals remain the same—identify friction
points in documentation, tooling, and workflows—but the measurements are
captured via repeatable scripts run by the Aurora team.

Key entry points:

- [`automation/README.md`](automation/README.md) – defines the scenarios and
  metrics we collect.
- [`automation/run.sh`](automation/run.sh) – orchestrates the checks.
- [`automation/output/`](automation/output/) – timestamped logs from the most
  recent self-assessment runs.

Run the assessment locally with:

```bash
./tools/usability-self-assess.sh
```

This mirrors the automated pipeline and generates updated logs under
`research/usability/automation/output/`.
