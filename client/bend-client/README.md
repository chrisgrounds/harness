# bend-client

A terminal client for Harness written entirely in [Bend 2](https://github.com/bendlang/bend).
JSON, UTF-8, framing, rendering, and session decisions are implemented in Bend;
networking and stdin use Bend's built-in Base effects. There is no project C
adapter, json-c dependency, or `@unsafe` code.

Start the updated Harness server, then run from this directory:

```sh
nix run path:.
# Optional colors and driver budget:
nix run path:. -- --color --steps 100000
# Connect to a different local port:
nix run path:. -- --port 7777
```

The server now listens on `127.0.0.1:7777` for newline-delimited JSON and keeps
its original Unix socket for existing clients. The Bend client requires this
TCP endpoint; it cannot connect to the old Unix-only server.

Enter sends a line; `/quit`, Ctrl-D at an empty line, or Ctrl-C exits.
Whitespace-only lines are ignored. Input uses the terminal's standard line
editing, without history or a multiline composer. Submitted input is displayed
with a `you` label after Enter. Output is plain text by default; `--color` enables
cyan user text, green agent replies, and yellow tool activity.

The client displays each completed turn chronologically, including tool-only
assistant messages, tool results, and multiple tool cycles. Earlier history is
omitted. Replies preserve newlines and indentation while removing terminal
control characters. Missing content and optional throughput values are handled.
Replies arrive as completed turns, without token streaming.

Requests are limited to 1024 encoded UTF-8 JSON bytes, excluding the newline.
Oversized requests are rejected locally and you can enter a shorter prompt.
TCP framing handles reads that split JSON or UTF-8 characters. Input lines are
capped at 1 MiB, replies at 8 MiB, and JSON nesting at 64 levels. Malformed replies
and connection errors terminate the session; requests are never retried.

The driver consumes a finite budget, defaulting to 100000 steps. `--steps N`
accepts 0 through 10000000. A step processes input or a reply chunk, so this is
not a turn count. Exhaustion closes the session with a nonzero status. The
budget bounds driver recursion, not wall-clock time: Base I/O can block.

## Build

The Nix flake supports `x86_64-linux`, pins Bend **2.0.39**, and supplies Clang:

```sh
nix build path:.
nix flake check path:.
nix develop path:. -c ./build.sh
./build/bend-client --help
```

Without Nix, install Bend 2.0.39 and Clang 14+, then run `./build.sh`.
Use `BEND=/path/to/bend` and `CC=/path/to/clang` to override the executables.
The build checks `PROOF.bend` and `src/main.bend` before native compilation.
Socket tests are not part of the build or flake checks.

Bend's native backend generates C internally and relies on its runtime and OS
for effects. “Entirely in Bend” describes the application source; it does not
remove the compiler's C backend or make networking mathematically pure.

## Implementation and proofs

- `src/bytes.bend`, `src/json.bend`, `src/reply.bend`: pure UTF-8, JSON, and message decoding.
- `src/frame.bend`: bounded newline framing across read chunks.
- `src/protocol.bend`: typed messages, current-turn selection, and safe rendering.
- `src/session.bend`: pure transitions producing `Display`, `Send`, and `Stop` actions.
- `src/main.bend`: CLI and structurally decreasing driver interpreting those actions with Base I/O.
- `LAWS.bend`: 18 obligations for tool visibility, prompt submission, history selection,
  ordering, nonempty agent output, request limits, failure/quit/EOF/budget behavior,
  and terminal control filtering.
- `PROOF.bend`: proofs by reduction, rewriting, case analysis, and induction.

All 18 obligations pass both Bend's checker and the independent BendTT kernel
with **ALL PROOFS CHECK**, without holes or unsafe proofs. Reproduce the kernel
check using Lean **4.34.0** (`lean` and `leanc` on PATH, or its elan toolchain):

```sh
bend PROOF.bend --verdict
```

Bend builds and caches its kernel on the first invocation. Alternatively, set
`BENDTT=/path/to/bendtt` to a kernel built from this Bend release's `bendtt.lean`.
The ordinary build uses Bend's checker; independent verification is a separate
command requiring Lean or the prebuilt kernel.

The laws refer to the pure state machine actually used by the driver and its
renderer. They prove action sequences, byte-limit decisions, history exclusion,
and sanitization. They do not prove JSON/UTF-8 codec round trips, physical socket
delivery, OS resource cleanup, or eventual completion of blocking I/O. The
compiler, Base runtime, and effect interpreter remain part of the trusted boundary.
