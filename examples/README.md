# Aurora Examples

This directory will host canonical Aurora programs illustrating language features.

## Included Samples
- `web_server/` – structured concurrency HTTP server running in the CPU zone.
- `gpu_image_filter/` – GPU blur kernel coordinated via the zone manager.
- `realtime_controller/` – deadline-aware control loop in the realtime zone.

Each sample contains runnable `.aur` source and a README outlining architecture,
runtime expectations, and suggested experiments.

## Upcoming Ideas
- `effects/` – effect polymorphism and handler composition patterns.
- `ffi/` – bridging to a native C library (e.g., SQLite).
- `tooling/` – examples demonstrating package manager + formatter integration.

Contributions are welcome: follow the template in the existing directories
where each example ships both code and documentation.
