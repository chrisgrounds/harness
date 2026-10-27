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

A failed verification loops back to an earlier step and starts a new iteration. The client shows the current step, per-function progress during implementation, and the iteration count.

> The design steps and the skeleton check are backed by the model; implementation, type checking and running properties are not wired up yet.

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

The server uses Mercury 3. During design the model can use `ReadFile`, `Discover` and `Search` (a fixed-string grep over `.hs` and `.cabal` files), and records each step with `SetTypes`, `SetFunctions` or `SetProperties`. Tool paths resolve against the server's working directory. Tool schemas are packaged with the server.

`cabal test server-tests` runs requirements end to end against a throwaway cabal project, with a scripted model in place of Mercury 3. Set `HOLEY_KEEP_PROJECT=1` to keep the generated project.

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
