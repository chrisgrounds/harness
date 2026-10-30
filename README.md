# Harness

A Haskell LLM harness for type-driven development, with a Brick terminal interface, a Dear ImGui desktop interface, an AF_UNIX server, and a shared `core` package.

Each message is a requirement and starts a *holey run*, which moves through these steps:

1. Creating types: the modules to create or change, with their imports and types
2. Creating functions (signatures only): new functions, and existing ones to change. Each has documentation kept as agent context, and for an existing function a change note that only its implementer sees. Neither is written into the code; existing declaration comments are preserved. Until they are implemented, new functions are `undefined` and changed ones keep their old equations, with a `_` pattern for each new parameter, or are `undefined` when those cannot fit the new signature
3. Creating properties (invariants the types can't express)
4. Checking skeleton (the code type-checks before any implementation)
5. Implementing functions (one agent per function, in parallel, on the model the design chose for it; each sees the design, its module, the project's existing types its signature uses and its properties)
6. Type checking
7. Running properties
8. Reviewing against the requirement: the design conversation checks every function the run wrote against the requirement, for what properties cannot test (when something happens, what reaches the user), and for duplication, and is told which modules and components the run changed and which it left alone. It may fix what it finds with `Refactor`, kept only if everything still builds and the properties pass. If meeting the requirement needs more than new definitions of the run's functions, such as changes to another module or component, it calls `RequirementNotMet` with the reason, and the run goes back to design with it. Otherwise its one-line conclusion goes in the run's history

A run can create modules and change existing ones. In an existing module, a type declaration or designed function replaces the declaration of the same name or is added after the others, and is exported if the module has an export list, so properties and other modules can use it; every other line is left as it was. Existing functions and instances that stop type checking because the design changed are *repairs*, in any package: the skeleton check turns a function into a hole that keeps its documentation and signature, and an instance into its head without methods, and an implementer rewrites the equations or the whole instance. A new module cannot be called `Main`. Files are split into declarations by layout (a declaration starts at a column-0 line of code, with the comments directly above it), which is also how build errors are blamed on functions in any file.

A failed verification loops back and starts a new iteration:

| Failure | Next |
| --- | --- |
| The skeleton does not type check | Errors in existing functions and instances, or in changed functions' old equations, become repairs or `undefined` and the skeleton is checked again. Errors in what one design step wrote go back to that step to correct, in the same iteration and up to three times: the property suite to step 3, a function's signature to step 2, a changed module's types and imports to step 1. Anything else is a redesign from step 1 with the errors |
| An agent submits no implementation | Reimplement that function |
| Type errors | Reimplement the functions whose declarations (or added imports) the errors are in, each with its own errors, including existing functions that broke; all functions if none can be blamed, and redesign if the same error comes back in the same place after that |
| Properties fail | Reimplement the functions the failing properties mention, with the counterexamples; redesign if the same properties failed last iteration too |

After ten iterations the run gives up. A run that does not finish, because it gave up, a step failed or something went wrong, puts every file it changed back as it was and removes the files and directories it created; start the server with `--keep-failed` to leave them for inspection. Every build's output, every model reply and each review's verdict are kept in the project's `dist-newstyle/holey/last-run.log`. The client shows the current step, per-function progress during implementation, the iteration count, and why each iteration looped back: what it reimplemented, or, for a redesign, the failure in one sentence (for a skeleton, the first GHC error and where it is).

A run writes into the project the client was started in (or `--project DIR`):

- new modules, in the first source directory of their component: a package's library (`demo`) or one of its executables (`demo:exe:app`);
- changes to existing modules, and repairs to existing functions anywhere in the project;
- `test/Holey<Module>Properties.hs` next to the main (first) module's package, a QuickCheck suite with one `prop_` per invariant, importing the run's modules its component can use (its own, and those of libraries it depends on);
- cabal files: each new module under its component's `exposed-modules` (library) or `other-modules` (executable), and a `test-suite holey-<module>` stanza named after the main module, so `cabal test` runs the properties locally and in CI. A library's suite depends on the package; an executable's suite compiles the executable's modules from its source directories with its dependencies, `default-extensions` and `default-language`.

Every write starts from the files as they were when the run first read them, files a redesign stops changing are restored, and a redesign with a different main module removes the run's earlier property suite. It refuses to overwrite files it did not create, warns when the git tree has uncommitted changes, and lists every file it touched. Verification uses the project's own `cabal`: steps 4 and 6 run `cabal build all --enable-tests`, so code anywhere in the project that uses what changed, including test suites, is type checked and repaired. The project's existing test suites are built but not run. Start the server inside a development shell that can build the project.

| Package | Purpose |
| --- | --- |
| `openai-auth/` | Shared local ChatGPT authentication and HTTPS support |
| `core/` | `Run` and its steps, `Chat`, messages, models, tool definitions and requests, JSON instances, and stream protocol |
| `server/` | Run orchestration, model requests, tool execution, and the Unix socket listener |
| `client/hs-tui-client/` | Brick UI built directly around the shared `Run` type |
| `client/hs-gui-client/` | Dear ImGui desktop UI over SDL2, around the same `Run` type; see its [README](client/hs-gui-client/README.md) |

## Run

Build from the repository root with GHC and Cabal:

```sh
cabal build all
```

The desktop client selects a model per chat. For terminal clients, choose the server default with `--model`: `mercury` (Inception's Mercury 3, the default) or Claude's `opus` (Opus 5.5), `sonnet` (Sonnet 5.5) or `haiku` (Haiku 5.5). Set `INCEPTION_API_KEY` for Mercury or `ANTHROPIC_API_KEY` for Claude in your environment or a `.env` file, then start the server:

```sh
cabal run server                      # Mercury 3
cabal run server -- --model opus      # Claude Opus 5.5
```

For ChatGPT subscription access, start the server and desktop client, then open **User settings** at the bottom of the sidebar. Choose **Continue with ChatGPT**, complete sign-in in your browser, All available models appear in each chat's right-aligned selector below the message box. **New chat** creates a separate transcript and draft; selecting a different chat restores its model and progress. You can switch accounts and refresh catalogs in settings without restarting the server.

For OpenAI models, the **Effort** selector beside the model picker controls reasoning effort for that chat. **Default** uses the model's own default; explicit levels are None, Minimal, Low, Medium, High, Extra high (`xhigh`) and Max. Supported levels depend on the model. The selection applies to every OpenAI call in the next run, including implementation and review, and can be changed between runs. Switching chats restores each chat's effort; new chats start at Default.

The client owns sign-in, credentials and model catalogs. Credentials are saved under its XDG config directory (`~/.config/harness/openai/accounts.json` by default), protected by owner-only permissions, atomic writes and a refresh lock. Public model catalogs live alongside it in `catalogs.json`. Each chat retains its account registration; the server requests a short-lived access token from that client before each model call, including parallel implementers. Refresh and ID tokens remain on the client. Runs are queued to prevent simultaneous edits to the working tree.

No OpenAI API key or Codex installation is needed. Model access and usage remain subject to your account, workspace and app limits; manage them in [ChatGPT Settings → Usage](https://chatgpt.com/settings/usage). This uses OpenAI's preview [ChatGPT plan usage flow](https://developers.openai.com/siwc/token-sharing-open-source) for local/open-source applications. Subscription failures stop the request; billing never switches automatically to an API key.

The terminal client continues to use the server's `--model` default. The existing `server login openai`, `models openai`, `accounts openai`, `use openai CLIENT_ID` and `logout openai` commands remain available for terminal workflows. The GUI selects its own model and account independently.

OpenAI uses the Responses API with streaming and no server-side conversation storage. The harness retains conversation and encrypted reasoning items in memory and executes its own namespaced tools through the existing agent loop. Every step and function implementation uses the chat's selected OpenAI model; model slugs are not ranked by capability. Chat transcripts currently live in the open client session. Mercury continues using Inception's chat completions API.

The model designs, and on Claude it also chooses which model implements each function: Haiku, Sonnet or, for the hardest, its own model. Without a choice, the next cheaper model than the design's implements it (Sonnet under Opus, Haiku under Sonnet), and a function that fails is reimplemented by the next more capable one. Mercury is the only Inception model, so a Mercury run implements with Mercury and is offered no choice.

In a second terminal, start the client with the Haskell project to work on. It defaults to the current directory:

```sh
cabal run hs-tui-client -- --project ~/src/my-project
```

Or start the desktop client, which takes the same arguments and needs SDL2 installed:

```sh
cabal run hs-gui-client -- --project ~/src/my-project
```

Claude is called through the Messages API with adaptive thinking at `high` effort, prompt caching, and, on Opus and Sonnet, the server-side fallback when a safety classifier declines a request; a refusal that is not rescued ends the agent's turn with a note. Claude's thinking is bound to the conversation that produced it, so each reply is sent back exactly as received and the design conversation offers the same tools at every step, refusing the other steps' ones; a thinking block the API can no longer use is dropped rather than failing the request. During design the model can use `ReadFile`, `ListFiles` and `RipGrep` (a fixed-string [ripgrep](https://github.com/BurntSushi/ripgrep) search over `.hs` and `.cabal` files, so `rg` must be installed), and records each step with `SetTypes`, `SetFunctions` or `SetProperties`. Each function is then implemented by its own agent, up to four at a time, which sees the design (every module's new and changed types and every designed signature and documentation), its own module with the run's functions as holes, the function's previous definition if it existed, the properties that mention it, and any feedback from a failed attempt; it submits its definition with `ImplementFunction`, which must define only that function (helpers go in a `where` clause). A record built without all its fields fails the build, not just a warning. Tool paths resolve against the client's project directory. Tool schemas are packaged with the server.

## Tests

```sh
cabal test all
```

- `core-tests`: JSON round-trips for the protocol types and the laws of the `Run` helpers.
- `hs-gui-client-tests`: the desktop client's state updates (sending, commands, recall, run updates, scrolling) and its palette.
- `server-tests`: OpenAI request/history and stream handling, signed identity validation, OAuth callback checks, concurrent token rotation and credential permissions; properties of the pure parts of the server (Claude requests that alternate turns, answer every tool call, send replies back unchanged and keep their earlier turns as a chat grows, reading Claude's replies, splitting modules into declarations losslessly, changing a module while keeping its other declarations, adding exports in each style of export list, blaming GHC errors on declarations and recording the package being built, reading QuickCheck output, the loop-back policy, and cabal file edits checked with Cabal-syntax), then end-to-end runs against throwaway cabal projects using the real `cabal` and GHC with a scripted model in place of the real one: a first-time pass, a failed property, a type error, a run that gives up and undoes its changes, a pass in an executable-only package that still builds afterwards, a change to an existing module that repairs a broken caller, a change to a library that repairs an executable in another package, and a redesign that changes the main module and removes the earlier suite. Set `HOLEY_KEEP_PROJECT=1` to keep the generated projects.

The root Nix flake includes every package and a shared development shell:

```sh
nix develop path:. -c cabal build all
nix build path:.
nix build 'path:.#hs-tui-client:exe:hs-tui-client'
```

Start the server and graphical client together with:

```sh
nix run . -- --project ~/src/my-project
nix run . -- --project ~/src/my-project --model opus
```

The launcher waits for the server to open a private socket before starting the client. Ctrl-C stops both processes and their child processes; closing the client or stopping the server also shuts down the pair. It accepts `--keep-failed` as well. `nix run .#harness` is the same command, and the project defaults to the current directory.

## Interface

The terminal client shows each run in order: the requirement, every step with its status, the functions being implemented while that step runs, and the reason for any failed step. The status line shows the current step and iteration. Runs happen in the background, so scrolling and drafting stay responsive while the server works.

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

The desktop client groups runs into chats with separate models and drafts, with a progress bar while functions are implemented, a `/tools` window, and the keys in [its README](client/hs-gui-client/README.md).

## Shared types and protocol

Server and client both depend on the local `core` library and `import Core`. `Run` holds a requirement's steps and their statuses, the iteration, per-function implementation tasks, and the artefact being built. `Chat` contains the full message history, model, and available tools. Its messages are stored newest first; the UI reverses them for display. Tool results retain their call IDs, and assistant messages retain tool requests and optional throughput metadata.

The only listener is `/tmp/harness.sock`, an `AF_UNIX` stream socket. Requests are `StartRun` JSON objects: the project directory and the requirement. The server replies to each with a stream of `ServerEvent` JSON objects: a `RunUpdated` snapshot of the full `Run` after every change, then one `TurnFinished`. Values are encoded as UTF-8 without newline framing. Incremental parsing handles split JSON/UTF-8, large messages, and adjacent values while preserving unread bytes. Each connection handles one run at a time.

## TODO

- **User checkpoints in holey runs.** Pause after the signatures and properties steps so the user can approve or edit the design before the parallel implementation step starts. Runs currently go straight through without stopping.
- **Repairing other declarations that break.** Functions and instances with methods are repaired; a pattern synonym or type family instance the design breaks sends the run back to design, which must replace it in `SetTypes`.
- **Running existing tests.** Runs build the project's test suites but do not run them, which would be slow. Running them would need a baseline taken before the run's first write, so suites that already fail are not blamed on the run.
