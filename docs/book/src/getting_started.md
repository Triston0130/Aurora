# Getting Started

This chapter explains how to build the Aurora workspace, execute the
verification suite, and try the sample applications.

## Prerequisites

- Rust 1.75 or newer (Rustup recommended).
- `rustfmt` and `clippy` components (`rustup component add rustfmt clippy`).
- LLVM tools (`rustup component add llvm-tools-preview`) if you want to inspect
  emitted IR from the compiler experiments.

## Clone & Bootstrap

```bash
git clone https://github.com/Triston0130/Aurora.git
cd Aurora
rustup component add rustfmt clippy
```

Run the one-stop CI helper to ensure the workspace builds on your machine:

```bash
./tools/ci.sh                # fmt + clippy + tests + doc tests
AURORA_CI_MODE=lint ./tools/ci.sh   # lint stage only
AURORA_CI_MODE=test ./tools/ci.sh   # test/doc stage only
```

The script mirrors the GitHub Actions matrix so local runs match CI exactly.

## Building the Prototype Compiler

The `aurora_cli` driver lives in `compiler/prototypes`. Use it to compile sample
programs or dump LLVM IR:

```bash
# Compile and emit diagnostics for a sample program
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur

# Emit LLVM IR alongside diagnostics
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  --emit=llvm examples/gpu_image_filter/image_filter.aur
```

## Running Sample Applications

Each example directory includes a `README.md` describing architecture and how to
experiment with the code. A typical session looks like this:

```bash
# start the web server sample under the CPU zone
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/web_server/web_server.aur

# execute the real-time controller scenario
cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/realtime_controller/realtime_controller.aur
```

Some samples interact with the runtime crates for timers or zone transfers. If a
scenario requires auxiliary services (e.g., a mock sensor loop), the example
README lists the prerequisite commands.

## Regenerating Documentation

The Aurora Book is stored in `docs/book`. Use `mdbook` if you want to render it
locally:

```bash
cargo install mdbook
mdbook serve docs/book
```

Navigate to `http://localhost:3000` to explore the live documentation while you
work on the language.
