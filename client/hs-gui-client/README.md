# Harness desktop client

A [Dear ImGui](https://github.com/ocornut/imgui) window for Harness, built on the [dear-imgui](https://hackage.haskell.org/package/dear-imgui) bindings and SDL2. It shows the same runs as the terminal client, driven by the shared `Run` type from `core`, and talks to the server over the same socket.

It needs SDL2 (`brew install sdl2`, or your distribution's `libsdl2-dev`). Drawing goes through SDL's renderer (Metal on macOS), so neither OpenGL nor GLEW is needed; `cabal.project` sets dear-imgui's flags for that.

From the repository root, start the server and the client in separate terminals:

```sh
cabal run server
cabal run hs-gui-client -- --project ~/src/my-project
```

Pass a socket path after the options to use something other than `/tmp/harness.sock`.

| Key | Action |
| --- | --- |
| Enter / Shift-Enter | Send / newline |
| Alt-↑ / Alt-↓ (Option on macOS) | Recall sent messages and restore the draft |
| Mouse wheel / Page Up / Page Down | Scroll the conversation |
| Ctrl-Home / Ctrl-End (Cmd on macOS) | First message / latest |
| Esc | Close the tools window |
| Ctrl-Q (Cmd-Q on macOS) | Quit |

`/tools` opens a window listing every tool the harness can call, and `/quit` quits. The conversation follows the latest output while it is scrolled to the bottom; scroll up to stop following and back down to resume.

## Design

The sidebar names the project and lists every run, newest first, with what it is doing; click one to scroll to it. It hides in windows narrower than 860 points. At its foot are the server's model and the average token rate of its replies.

Each run opens with the requirement, set a weight up beside a bar in the user's colour, then hangs its eight steps on a rail. The rail fills in the passed colour down from each step that passes and drains when an iteration sends the run back, so how far a run got, and how often it looped, shows at a glance. The running step has a spinner on the rail; while functions are implemented, a progress bar and a line per function hang beneath it. Orange marks only what is running: the live step, the progress bar and the send button.

Type is Avenir Next in three real weights, with SF Mono for code: modules, functions, files and compiler errors. Prose wraps at under 80 characters a line. Elsewhere one sans-serif weight and the system's monospace stand in.

Movement eases rather than jumps: the progress bar, the rail, scrolling, the composer growing with its draft, and its edge lightening while typing. A new run fades in as it is sent. Nothing moves on hover. The window keeps drawing while it is resized, and once nothing is moving and no run is in progress, the frame loop sleeps until there is input.

## Layout

| Module | Purpose |
| --- | --- |
| `Client` | The connection to the server, published as `NetworkEvent`s |
| `Client.Commands`, `Client.Tokens` | `/` commands and the average token rate, as in the terminal client |
| `UI.State` | The interface's state and a pure `update` for each `Action`: what the user did, or news from the server |
| `Theme.Palette` | The palette, read from `themes/orng.toml`, the terminal client's colours |
| `app/UI` | The window: the frame loop, and drawing the state each frame into the actions it produces |
| `app/UI/Motion` | Values that ease toward their targets, keeping frames coming until they arrive |
| `app/UI/Widgets`, `app/UI/Fonts`, `app/Theme` | Text, drawn icons, the send button, the composer, fonts, and Dear ImGui's style |
| `app/ImGui/Extras`, `cbits/imgui_extras.cpp` | Dear ImGui calls the bindings leave out: scrolling, key chords, style values, fading, and the composer |

The library holds everything that does not touch the window, and `cabal test hs-gui-client` checks its properties.

`cbits/imgui` is a copy of the `imgui.h` and `imconfig.h` that dear-imgui 2.4.1 builds Dear ImGui 1.91.9b from, so the extras compile against the same structs; the cabal file pins that version, and the client refuses to start if the layouts differ. Updating dear-imgui means copying its headers again.

Dear ImGui 1.91's text boxes do not wrap, so a long line in the composer scrolls sideways; messages in the conversation wrap.
