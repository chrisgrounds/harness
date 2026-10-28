# Harness

A Haskell LLM harness for type-driven development, with a Brick terminal interface, an AF_UNIX server, and a shared `core` package.

Each message is a requirement and starts a *holey run*, which moves through these steps:

1. Creating types: the modules to create or change, with their imports and types
2. Creating functions (signatures only, bodies left as `undefined`): new functions, and existing ones to change
3. Creating properties (invariants the types can't express)
4. Checking skeleton (the code type-checks before any implementation)
5. Implementing functions (one cheap agent per function, in parallel)
6. Type checking
7. Running properties

A run can create modules and change existing ones. In an existing module, a type declaration or designed function replaces the declaration of the same name or is added after the others (and to the export list, if there is one); every other line is left as it was. Existing functions that stop type checking because the design changed are *repairs*: the skeleton check turns each into a hole that keeps its documentation and signature, in any package, and an implementer rewrites its equations. Files are split into declarations by layout (a declaration starts at a column-0 line of code, with the comments directly above it), which is also how build errors are blamed on functions in any file.

A failed verification loops back and starts a new iteration:

| Failure | Next |
| --- | --- |
| The skeleton does not type check | Redesign from step 1 with the errors, unless they are all in existing functions: those become repairs and the skeleton is checked again |
| An agent submits no implementation | Reimplement that function |
| Type errors | Reimplement the functions whose declarations (or added imports) the errors are in, each with its own errors, including existing functions that broke; all functions if none can be blamed |
| Properties fail | Reimplement the functions the failing properties mention, with the counterexamples; redesign if the same properties failed last iteration too |

After ten iterations the run gives up, leaving its files in place. The client shows the current step, per-function progress during implementation, the iteration count, and why each iteration looped back.

A run writes into the project the client was started in (or `--project DIR`):

- new modules, in the first source directory of their component: a package's library (`demo`) or one of its executables (`demo:exe:app`);
- changes to existing modules, and repairs to existing functions anywhere in the project;
- `test/Holey<Module>Properties.hs` next to the main (first) module's package, a QuickCheck suite with one `prop_` per invariant;
- cabal files: each new module under its component's `exposed-modules` (library) or `other-modules` (executable), and a `test-suite holey-<module>` stanza named after the main module, so `cabal test` runs the properties locally and in CI. A library's suite depends on the package; an executable's suite compiles the executable's modules from its source directories with its dependencies, `default-extensions` and `default-language`.

Every write starts from the files as they were when the run first read them, and files a redesign stops changing are restored. It refuses to overwrite files it did not create, warns when the git tree has uncommitted changes, and lists every file it touched. Verification uses the project's own `cabal`: steps 4 and 6 run `cabal build all --enable-tests`, so code anywhere in the project that uses what changed, including test suites, is type checked and repaired. The project's existing test suites are built but not run. Start the server inside a development shell that can build the project.

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

Set `INCEPTION_API_KEY` in your environment or a `.env` file, then start the server:

```sh
cabal run server
```

`server --demo` plays a scripted run without calling the model or touching any files.

In a second terminal, start the client with the Haskell project to work on. It defaults to the current directory:

```sh
cabal run hs-tui-client -- --project ~/src/my-project
```

The server uses Mercury 3. During design the model can use `ReadFile`, `Discover` and `Search` (a fixed-string grep over `.hs` and `.cabal` files), and records each step with `SetTypes`, `SetFunctions` or `SetProperties`. Each function is then implemented by its own agent, up to four at a time, which sees the design (every module's new and changed types and every designed signature and documentation), its own module with the run's functions as holes, the function's previous definition if it existed, the properties that mention it, and any feedback from a failed attempt; it submits its definition with `ImplementFunction`. Tool paths resolve against the client's project directory. Tool schemas are packaged with the server.

## Tests

```sh
cabal test all
```

- `core-tests`: JSON round-trips for the protocol types and the laws of the `Run` helpers.
- `server-tests`: properties of the pure parts of the server (splitting modules into declarations losslessly, changing a module while keeping its other declarations, adding exports in each style of export list, blaming GHC errors on declarations and recording the package being built, reading QuickCheck output, the loop-back policy, and cabal file edits checked with Cabal-syntax), then end-to-end runs against throwaway cabal projects using the real `cabal` and GHC with a scripted model in place of Mercury 3: a first-time pass, a failed property, a type error, a run that gives up, a pass in an executable-only package that still builds afterwards, a change to an existing module that repairs a broken caller, and a change to a library that repairs an executable in another package. Set `HOLEY_KEEP_PROJECT=1` to keep the generated projects.

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

The only listener is `/tmp/harness.sock`, an `AF_UNIX` stream socket. Requests are `StartRun` JSON objects: the project directory and the requirement. The server replies to each with a stream of `ServerEvent` JSON objects: a `RunUpdated` snapshot of the full `Run` after every change, then one `TurnFinished`. Values are encoded as UTF-8 without newline framing. Incremental parsing handles split JSON/UTF-8, large messages, and adjacent values while preserving unread bytes. Each connection handles one run at a time.

## TODO

- **User checkpoints in holey runs.** Pause after the signatures and properties steps so the user can approve or edit the design before the parallel implementation step starts. Runs currently go straight through without stopping.
- **Changing instances and other non-function declarations that break.** Only functions are repaired; an instance or pattern synonym the design breaks sends the run back to design, which must replace it in `SetTypes`.
- **Running existing tests.** Runs build the project's test suites but do not run them, which would be slow. Running them would need a baseline taken before the run's first write, so suites that already fail are not blamed on the run.
- **Undoing a run.** A run that gives up leaves its changes in place, including edits to existing modules; undo them with git.
- **A cheaper model for implementation.** Every step uses Mercury 3; implementation agents should use a cheaper model once one is configured.
