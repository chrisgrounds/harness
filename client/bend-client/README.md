# bend-client

A terminal client for Harness written in [Bend 2](https://github.com/bendlang/bend),
with cyan user prompts, green agent replies, yellow tool activity, and optional
tokens-per-second values. It keeps a single Unix socket connection across turns.

Start the Harness server, then run from this directory:

```sh
nix run path:.
# Optional socket path (default: /tmp/harness.sock):
nix run path:. -- /path/to/harness.sock
```

Enter sends a line; `/quit`, Ctrl-D at an empty prompt, or Ctrl-C exits.
Whitespace-only lines are ignored. Input uses the terminal's standard line
editing; this client does not implement Up/Down history or a multiline composer.
Set `NO_COLOR=1` for plain text. Redirected output also omits color and prompts.
Replies preserve line breaks and indentation, with terminal control characters
removed from server text.

The client displays the current turn in chronological order, including tool-only
assistant messages, tool results, and multiple tool cycles. Earlier history and
the echoed user message are omitted. It handles JSON and UTF-8 split across reads,
missing/null content, and missing/null throughput values. Messages appear when
the server sends the completed turn, rather than streaming individual tokens.

The server currently reads each request with a single 1024-byte `recv`. The client
rejects requests exceeding 1024 encoded JSON bytes before sending them, and lets
you enter a shorter message. That server framing limitation still applies even
to accepted requests. Connection or malformed-response errors terminate the
session with a nonzero status; failed requests are never retried automatically.
Replies are limited to 64 MiB and 64 JSON nesting levels.

## Build and test

The Nix flake supports `x86_64-linux` and pins Bend **2.0.39** and json-c **0.19**,
including release archive checksums. It provides the compiler, Clang, and Python:

```sh
nix build path:.
nix flake check path:.
nix develop path:. -c ./build.sh
nix develop path:. -c python3 tests/test_socket.py
./build/bend-client --help
```

Without Nix, install Bend 2.0.39, Clang 14+, `pkg-config`, and json-c 0.19 or newer
development headers, then run `./build.sh`. Use `BEND=/path/to/bend`
and `CC=/path/to/clang` to override the executables. Python 3 is needed only for
the regression tests. The built executable requires json-c at runtime, but does
not require Bend, Python, or a separate socket bridge.

## Implementation

- `src/main.bend`: CLI, input handling, session loop, and error handling.
- `src/protocol.bend`: typed messages, current-turn selection, rendering, and
  foreign-effect declarations.
- `src/host.c`: POSIX socket/terminal effects and JSON encoding/decoding using
  json-c. Blocking input and exchanges run on Bend's I/O worker threads.
- `LAWS.bend`: the client contract, with 15 obligations covering ten behavioral
  requirements.
- `PROOF.bend`: proof bodies for all 15 obligations, plus induction lemmas for
  filtering, string/list conversion, and safe formatting. The laws are unchanged.

Bend's Base library currently provides TCP/UDP effects, but no Unix-domain socket
connector or JSON codec, so this client uses its documented C foreign-effect
mechanism. The build emits C from Bend and links it with json-c. Only the native
target is supported; running `bend src/main.bend` directly uses the JavaScript
target and will fail because these effects have no JavaScript implementation.

The host adapter depends on Bend 2.0.39's generated constructor layout; upgrading
Bend requires reviewing that boundary and rerunning the socket tests. The
externally driven session loop uses `@unsafe` recursion. Consequently, Bend's
`--check-only` proof gate reports unsafe/foreign dependencies: this application
is not claimed to be formally verified. Normal compilation still checks types.

Tests launch a local mock Unix socket server; no API key or running Harness
server is required. They cover multiple turns/tool cycles, fragmented Unicode,
size boundaries and recovery, optional fields, malformed replies, disconnection,
terminal controls, and CLI errors. Set `BEND_CLIENT` to test another built binary.

## Proof status

```sh
nix develop path:. -c bend PROOF.bend
# Request the independent kernel check as well:
nix develop path:. -c bend PROOF.bend --verdict
```

All 15 proof bodies pass Bend's type checker with no holes or assumed helper
laws. Eleven follow by reduction; the others use case analysis and induction.
Formatting sanitizes and indents in one traversal so its safety proof follows
each character decision directly.

**The certification gate still exits nonzero with `SOME PROOFS FAIL`.** It rejects
the imported C effects and `@unsafe` session loop, including laws whose types
refer to them. Neither command currently produces `ALL PROOFS CHECK`; the
independent kernel check is blocked by that dependency gate. In particular,
these proofs do not certify C JSON encoding, socket delivery, or termination of
the interactive session. The I/O equalities describe client action sequences
relative to those trusted definitions.

Full certification requires separating a terminating, pure session transition
model from the effect interpreter and expressing the I/O laws against that
model. Do not remove dependencies from the checker, add `@unsafe` proofs, or
weaken the laws to turn the gate green. Integration tests remain necessary for
the native boundary.
