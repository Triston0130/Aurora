# GPU Image Filter Sample (Placeholder)

This directory currently holds a minimal program that mimics a GPU blur kernel
using the prototype compiler. The implementation simply performs an integer
addition so that `aurora_cli --emit=llvm` produces IR without triggering parser
errors.

The full GPU pipeline (data transfers, executors, region hand-off) will return
in a future milestone.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  --emit=llvm examples/gpu_image_filter/image_filter.aur
```

The `--emit=llvm` flag prints the generated IR, useful for confirming the
prototype backend remains functional.
