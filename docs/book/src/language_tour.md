# Language Tour

Aurora blends familiar systems-programming constructs with novel effect and
zone abstractions. This chapter introduces the core syntax that underpins the
prototype.

## Ownership & Regions

```aur
fn accumulate(buffer: &mut [Int32], payload: &[Int32]) -> Result<(), IOError> {
    for (dst, src) in buffer.iter_mut().zip(payload.iter()) {
        *dst += *src;
    }
    Ok(())
}
```

Borrowing follows Rust-inspired rules, but regions are explicit and can be
handed off between zones. Region utilities live in `stdlib::region` and the
runtime's `ZoneManager` enforces the transfers.

## Effect-Annotated Functions

```aur
fn read_config(path: String) -> Result<Config, IOError!IO> {
    let bytes = std::fs::read(path)?;
    deserialize(bytes)
}
```

Effect rows annotate potential side-effects. Handlers (`handle IO { ... }`) can
scoped-capture the effects and route them to actors or zone services.

## Structured Concurrency

```aur
actor Logger(ctx) {
    loop {
        match ctx.recv() {
            Message::Log(entry) => write_log(entry)?,
            Message::Shutdown => break,
        }
    }
}

fn run() {
    supervisor zone CPU {
        let logger = spawn Logger;
        let worker = spawn Worker { logger };
        await worker;
    }
}
```

Supervisors, cancellation propagation, and deterministic shutdown semantics are
built into the runtime. Zones gate resource access (GPU, realtime, sandboxed
I/O) and appear directly in the type/effect system.

## Zones & Offloading

```aur
zone GPU kernel blur(image: Region<Image>, radius: Int32) -> Region<Image> {
    launch threads_for(image) {
        convolve(pixel(), radius)
    }
}
```

A `zone` block describes a capability surface. The compiler lowers these blocks
into structured MIR and the runtime schedules them on the appropriate executor
(GPU supervisor, realtime kernel, etc.).

## Async & Timers

```aur
async fn poll_temperature(sensor: SensorHandle) -> Result<(), Timeout!IO> {
    loop {
        let reading = sensor.next().await?;
        emit(reading);
        timer::sleep(Duration::from_secs(1)).await?;
    }
}
```

Async functions compose with the structured concurrency model: awaiting a task
participates in supervisor lifetimes and inherits zone policies.

The examples directory includes runnable programs that exercise each of these
concepts. Use them as a reference while experimenting with the language.
