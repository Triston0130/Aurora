# Realtime Controller Sample

A control loop executes inside the realtime zone with millisecond deadlines
while CPU actors feed telemetry and collect diagnostics.

## Highlights
- Realtime zone enforces a 2 ms deadline per cycle.
- Region hand-off carries sensor data between CPU and realtime actors.
- Supervisor cancels the controller if deadlines are missed, demonstrating
  structured cancellation cascades.

## Running

```bash
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/realtime_controller/realtime_controller.aur
```

The program prints controller outputs and deadline statistics to stdout. Use the
`AURORA_RT_JITTER` environment variable inside the example to simulate sensor
noise or CPU contention.

## Possible Extensions
- Integrate with actual hardware via the FFI layer.
- Feed the telemetry into the GPU image filter example for computer vision.
- Emit metrics to the web server sample for live dashboards.
