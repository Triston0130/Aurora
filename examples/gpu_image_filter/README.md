# GPU Image Filter Sample

Demonstrates a GPU zone kernel that applies a simple blur filter to an image
region. CPU actors orchestrate data transfers while the GPU zone performs the
heavy computation.

## Highlights
- Zone declaration describing GPU requirements.
- Region hand-off from CPU → GPU → CPU using `runtime/memory` transfer APIs.
- Timer-based progress logging while the kernel executes.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  --emit=llvm examples/gpu_image_filter/image_filter.aur
```

The `--emit=llvm` flag prints the generated GPU kernel IR so you can inspect the
lowered code.

## Experiments
- Adjust the kernel radius and observe how the runtime schedules additional GPU
  workgroups.
- Replace the blur with an edge-detection filter.
- Extend the example to stream frames from a live camera feed.
