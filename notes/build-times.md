# Build times

Notes on why holey's builds are slow and what to do about it, for Haskell now and Rust later.

## What we measured

From the run of "rename and delete chats from a right-click menu" (gpt-6.1-sol, 30 Oct 2026):

| | Count | Time |
|---|---|---|
| Model calls | 57 | 682s (some concurrent) |
| Builds | 15 | ~168s |
| Tool calls (ReadFile, RipGrep, ...) | many | under 1s in total |

The builds were:

- **Skeleton check:** 77.8s, 17.5s, 9.5s, 6.3s, 15.4s. The first build was the longest, probably because the new modules forced a large part of the GUI client to recompile. The next three were fix-ups after the skeleton failed.
- **Type checking:** ten builds of 0.7s to 21.7s, one per attempt to reimplement `conversationStatus`.

Builds already run once per round, not once per implementer: `implementStep` runs every implementer (four at a time) and only then does `typeCheck` build. The cost is in what each build covers, not how often it runs.

## How holey builds today

Every build is `buildAll` in `server/src/Verify.hs`:

```
cabal build all --enable-tests --disable-optimization --builddir=dist-newstyle/holey-build -j1 --ghc-options=-j
```

The property suite runs with the same flags, so it doesn't rebuild. `-O0`, a separate build directory and GHC's `-j` (sections 2 and 5 below) are in place. Each build's duration and finish time is in the run log, and so is each property run.

- **`all`** builds every package in `cabal.project`: `openai-auth`, `core`, `server`, `hs-tui-client` and `hs-gui-client`. It arrived with "let holey change existing modules and repair the callers its changes break" (90a8a2d), because a change in `core` can break callers in any client, and holey needs to see those errors to repair them.
- **`--enable-tests`** builds every test suite too, so the generated property suite and any broken tests are caught.
- **`-j1`** builds one package at a time. `ghcErrors` decides which package an error belongs to from the most recent `Building library for X` line, so parallel builds would interleave output and put errors on the wrong package. Checked with cabal 3 and GHC 9.6.7: with `-j`, two packages' errors interleave character by character. GHC's own `-j` keeps each message whole.
- **`hs-tui-client`** keeps its modules in a library, so the executable and its property suites share one compiled copy. Before that, each of the three suites listed `src` as a source directory and compiled `UI.hs` again: four copies per build.

So a one-line change to `hs-gui-client` still checks that every other package is up to date, one at a time. Nothing else changes, so the cost of checking them is small but not free, and cabal's planning step runs every time.

## Haskell: what to do instead of `cabal build all`

### 1. Build only the affected packages and their reverse dependencies

Holey knows which files it changed (`touchedFiles`), and `findProject` already reads each component's `dependencies`. From these:

1. Map each touched file to its component (`sourceDirs` under `packageDir`).
2. Add every component in the project that depends on those components, directly or indirectly. These are the only places a broken caller can be.
3. Build just those targets: `cabal build --enable-tests core server hs-tui-client hs-gui-client` for a change in `core`, or only `hs-gui-client` for a change there.

For a GUI-only change, which is most of what we do, this skips the other four packages entirely. For a `core` change it builds the same as today, so we lose nothing in caller repair.

The generated property suite lives in one package, so `runProperties` already uses `buildTarget` for just that suite.

### 2. Let packages build in parallel

`-j1` is there only because of how we parse output. Two ways out:

- **Build each package separately**, one `cabal build` per target, in dependency order and at the same time where the dependency graph allows. Each output is then known to come from one package, so `ghcErrors` doesn't need the `Building ...` line.
- **Structured diagnostics.** GHC 9.10 added `-fdiagnostics-as-json`, which gives each error with its file and span. We're on 9.6.7, so this has to wait for an upgrade. Once it's available it removes the text parsing, and the ordering problem with it.

Within a package, `--ghc-options=-j` lets GHC compile independent modules in parallel. This is safe now, because errors from one package still come out under its own heading.

### 3. Type-check without generating code

The skeleton check and type-check rounds only need to know whether the code type-checks. `-fno-code` skips code generation and linking, which is most of a full build's time. Only the properties step needs real binaries.

Things to watch for:

- **Separate build directory:** changing GHC options makes cabal rebuild everything. Give these builds their own `--builddir` (for example `dist-newstyle/holey-check`) so they don't throw away the normal build and the user's own builds don't throw away ours.
- **Template Haskell:** a package that uses TH needs code for the modules it splices, so use `-fno-code` together with `-fwrite-interface`, or skip `-fno-code` for packages that use TH.

### 4. Keep a GHC session open (bigger change)

Most of a small rebuild is cabal and GHC starting up and loading interfaces, not compiling. A long-running `cabal repl` session per affected package (the way `ghcid` works), reloaded with `:reload` after each round, type-checks small changes in well under a second. Its downsides: it's one session per component, it uses a lot of memory on large projects, and it needs care with GHCi's output format. Worth trying once 1–3 are done, if type-check rounds are still the slow part.

### 5. Optimisation level

The run used the default `-O1` profile. For type-check and property builds, `-O0` compiles much faster. Like `-fno-code`, it should go in its own `--builddir` so we don't rebuild everything each time we switch between it and the user's builds.

## Rust and cargo

The same plan, with cargo's tools for each step.

### Type-check with `cargo check`

`cargo check` type-checks and borrow-checks without code generation or linking, usually several times faster than `cargo build`. Use it for the skeleton check and type-check rounds. Only the properties step needs `cargo test`.

### Only build the affected crates

Map touched files to their crates, add every workspace crate that depends on them (`cargo metadata` gives the dependency graph), and pass them with `-p`:

```
cargo check --all-targets -p core -p server -p gui
```

`--all-targets` is the equivalent of `--enable-tests`: it checks tests, examples and benches too, so broken test callers are caught. Fall back to `--workspace` if the graph can't be worked out.

### Use structured diagnostics

```
cargo check --message-format=json
```

Each compiler message comes back as JSON naming its crate, file, line, column and span, plus suggested fixes. That's much more reliable than parsing text, and it means parallel builds (cargo's default) are safe. Rust gets the fix Haskell has to wait for: no `-j1`.

### Keep the harness's target directory separate

Set `CARGO_TARGET_DIR` (or `--target-dir`) to a harness-only directory such as `target/holey`. Otherwise holey and rust-analyzer or the user's `cargo build` block each other on the target directory lock, and different feature flags make each side rebuild what the other built.

### Make test builds cheaper

For the properties step (`cargo test` or `cargo nextest`):

- **Less debug info:** set `debug = 0` or `debug = "line-tables-only"` in a harness profile. Debug info is much of the link time.
- **Faster linker:** `lld` or `mold` make linking test binaries much quicker, and that's often the biggest part of an incremental test build.
- **Only the generated suite:** run it with `cargo test -p <crate> --test <suite>` rather than every test in the workspace.
- **Build once, run separately:** `cargo test --no-run` builds without running, so build errors and failing properties are reported separately.

Incremental compilation is on by default in the dev profile, so keep it there.

### Further options

- **Skip build scripts:** `cargo check` still runs `build.rs` and procedural macros, which can dominate small checks. There isn't much to do about that beyond keeping the separate target directory warm.
- **Persistent checking:** rust-analyzer can run `cargo check` for us, but its output is harder to work with than running `cargo check` directly. Not worth it until the steps above are done.

## Suggested order

1. **Reverse-dependency targets** instead of `all` (Haskell section 1). This is the biggest win for GUI-only changes, and it's a small change to `buildAll`.
2. **`-fno-code` type-checking** in a separate build directory (section 3).
3. **Per-package builds in parallel** (section 2), then **JSON diagnostics** once we're on GHC 9.10 or later.
4. **Rust support** following the cargo section, with `cargo check --message-format=json` from the start.
