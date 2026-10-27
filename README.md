# Harness

A Haskell LLM harness for type-driven development, with a Brick terminal interface, an AF_UNIX server, and a shared `core` package.

Each message is a requirement and starts a *holey run*, which moves through these steps:

1. Creating types
2. Creating functions (signatures only, bodies left as `undefined`)
3. Creating properties (invariants the types can't express)
4. Checking skeleton (the code type-checks before any implementation)
5. Implementing functions (one cheap agent per function, in parallel)
6. Type checking
7. Running properties

A failed verification loops back and starts a new iteration:

| Failure | Next |
| --- | --- |
| The skeleton does not type check | Redesign from step 1 with the errors |
| An agent submits no implementation | Reimplement that function |
| Type errors | Reimplement the functions whose lines (or added imports) the errors are in, each with its own errors; all functions if none can be blamed |
| Properties fail | Reimplement the functions the failing properties mention, with the counterexamples; redesign if the same properties failed last iteration too |

After four iterations the run gives up, leaving its files in place. The client shows the current step, per-function progress during implementation, the iteration count, and why each iteration looped back.

A run writes into the project the server is started in:

- the new module, in the first source directory of the library package chosen in step 1;
- `test/Holey<Module>Properties.hs`, a QuickCheck suite with one `prop_` per invariant;
- the package's cabal file: an `exposed-modules` line for the module and a `test-suite holey-<module>` stanza.

It refuses to overwrite files it did not create, warns when the git tree has uncommitted changes, and lists every file it touched. Verification uses the project's own `cabal`, so start the server inside the project's development shell.

| Package | Purpose |
| --- | --- |
| `core/` | `Run` and its steps, `Chat`, messages, models, tool definitions and requests, JSON instances, and stream protocol |
| `server/` | Run orchestration, model requests, tool execution, and the Unix socket listener |
| `client/hs-tui-client/` | Brick UI built directly around the shared `Run` type |

## Run

Build from the repository root with GHC and Cabal:

```sh
cabal build all
```

Set `INCEPTION_API_KEY` in your environment or a `.env` file, then start the server from the root of the Haskell project you want to work on:

```sh
cd ~/src/my-project
cabal run --project-dir ~/src/harness server
```

`server --demo` plays a scripted run without calling the model or touching any files.

In a second terminal:

```sh
cabal run hs-tui-client
```

The server uses Mercury 3. During design the model can use `ReadFile`, `Discover` and `Search` (a fixed-string grep over `.hs` and `.cabal` files), and records each step with `SetTypes`, `SetFunctions` or `SetProperties`. Each function is then implemented by its own agent, up to four at a time, which sees only the module's types, signatures and documentation, the properties that mention the function, and any feedback from a failed attempt; it submits its definition with `ImplementFunction`. Tool paths resolve against the server's working directory. Tool schemas are packaged with the server.

## Tests

```sh
cabal test all
```

- `core-tests`: JSON round-trips for the protocol types and the laws of the `Run` helpers.
- `server-tests`: properties of the pure parts of the server (function spans in rendered modules, attributing GHC errors to functions, reading QuickCheck output, the loop-back policy, and cabal file edits checked with Cabal-syntax), then end-to-end runs against a throwaway cabal project using the real `cabal` and GHC with a scripted model in place of Mercury 3: a first-time pass, a failed property, a type error, and a run that gives up. Set `HOLEY_KEEP_PROJECT=1` to keep the generated projects.

The root Nix flake includes all three packages and a shared development shell:

```sh
nix develop path:. -c cabal build all
nix build path:.
nix build 'path:.#hs-tui-client:exe:hs-tui-client'
```

## Interface

The client shows each run in order: the requirement, every step with its status, the functions being implemented while that step runs, and the reason for any failed step. The status line shows the current step and iteration. Runs happen in the background, so scrolling and drafting stay responsive while the server works.

| Key | Action |
| --- | --- |
| Enter | Send |
| Shift-Enter | Insert a newline |
| Alt-Up / Alt-Down | Recall sent messages and restore the current draft |
| Mouse wheel / Page Up / Page Down | Scroll the conversation |
| Ctrl-Home / Ctrl-End | First message / follow latest |
| Ctrl-C | Exit |
| Ctrl-D | Exit when the composer is empty |

The client accepts an optional socket path:

```sh
cabal run hs-tui-client -- /path/to/harness.sock
```

Piped input or output uses a plain transcript through the same client and shared types, printing one line per step change. `/quit` exits in either mode.

## Shared types and protocol

Server and client both depend on the local `core` library and `import Core`. `Run` holds a requirement's steps and their statuses, the iteration, per-function implementation tasks, and the artefact being built. `Chat` contains the full message history, model, and available tools. Its messages are stored newest first; the UI reverses them for display. Tool results retain their call IDs, and assistant messages retain tool requests and optional throughput metadata.

The only listener is `/tmp/harness.sock`, an `AF_UNIX` stream socket. Requests are `Requirement` JSON strings. The server replies to each with a stream of `ServerEvent` JSON objects: a `RunUpdated` snapshot of the full `Run` after every change, then one `TurnFinished`. Values are encoded as UTF-8 without newline framing. Incremental parsing handles split JSON/UTF-8, large messages, and adjacent values while preserving unread bytes. Each connection handles one run at a time.

## TODO

- **User checkpoints in holey runs.** Pause after the signatures and properties steps so the user can approve or edit the design before the parallel implementation step starts. Runs currently go straight through without stopping.
- **Changing existing modules.** A run can only add a new module. Extending existing types or wiring the new functions into existing call sites is left to the user.
- **A cheaper model for implementation.** Every step uses Mercury 3; implementation agents should use a cheaper model once one is configured.
