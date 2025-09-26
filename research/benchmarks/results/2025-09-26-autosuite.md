- Commit: `3ed42d0ee0ee4795f8fbd0b5db9aefe204cd726a`

| Benchmark | Mean (ms) | Median (ms) | Throughput |
| --- | --- | --- | --- |
| ffi_roundtrip_strlen/strlen_batch_512 | 0.528 | 0.527 | -- |
| gpu_executor_identity/gpu_batch_64 | 1.023 | 1.021 | -- |
| realtime_executor_schedule/schedule_batch_128 | 1.931 | 1.931 | -- |
| scheduler_actor_spawn_join/spawn_join_batch_100 | 1.622 | 1.649 | -- |
| timer_wheel_schedule/schedule_batch_128 | 2.609 | 2.607 | -- |
| zone_handoff_round_trip/handoff_batch_64 | 0.006 | 0.006 | -- |
