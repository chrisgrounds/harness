# Harness desktop client

A [Dear ImGui](https://github.com/ocornut/imgui) window for Harness, built on the [dear-imgui](https://hackage.haskell.org/package/dear-imgui) bindings and SDL2. It shows the same runs as the terminal client, driven by the shared `Run` type from `core`, and talks to the server over the same socket.

It needs SDL2 (`brew install sdl2`, or your distribution's `libsdl2-dev`). Drawing goes through SDL's renderer (Metal on macOS), so neither OpenGL nor GLEW is needed; `cabal.project` sets dear-imgui's flags for that.

From the repository root, start the server and the client in separate terminals:

```sh
cabal run server
cabal run hs-gui-client -- --project ~/src/my-project
```

Use the cog icon at the bottom of the sidebar to open **User settings**, connect ChatGPT, switch or sign out of an account, and refresh its model catalog. Close settings with its X or Escape. Complete the OAuth sign-in in your browser; account setup runs in the background.

Use the model dropdown inside the bottom-right of the message box to select this chat's model, and the circular up-arrow button beside it to send. The pencil-and-page **New chat** icon creates an independent draft and transcript. Switching chats restores each chat's model, draft and progress, including updates received in the background. Models come from your connected ChatGPT accounts and the server's configured API providers. All available models appear automatically. Chats live for the current window session; account settings and model catalogs are saved on disk.

Pass a socket path after the options to use something other than `/tmp/harness.sock`.

| Key | Action |
| --- | --- |
| Enter / Shift-Enter | Send / newline |
| Alt-↑ / Alt-↓ (Option on macOS) | Recall sent messages and restore the draft |
| Mouse wheel / Page Up / Page Down | Scroll the conversation |
| Ctrl-Home / Ctrl-End (Cmd on macOS) | First message / latest |
| Ctrl-N / Ctrl-, (Cmd on macOS) | New chat / User settings |
| Esc | Close the tools window |
| Ctrl-Q (Cmd-Q on macOS) | Quit |

`/tools` opens a window listing every tool the harness can call, and `/quit` quits. The conversation follows the latest output while it is scrolled to the bottom; scroll up to stop following and back down to resume.

## Design

The sidebar names the project and lists chats once their first message is sent, newest first, with each chat's model and running state; click one to switch conversations. Drag the divider to resize the sidebar; User settings sits at its foot. It hides in windows narrower than 860 points. It also shows the average token rate.

Each run opens with the requirement, set a weight up beside a bar in the user's colour, then hangs its eight steps on a rail. The rail fills in the passed colour down from each step that passes and drains when an iteration sends the run back, so how far a run got, and how often it looped, shows at a glance. The running step has a spinner on the rail; while functions are implemented, a progress bar and a line per function hang beneath it. Orange marks only what is running: the live step, the progress bar and the send button.

Type is Avenir Next in three real weights, with SF Mono for code: modules, functions, files and compiler errors. Prose wraps at under 80 characters a line. Elsewhere one sans-serif weight and the system's monospace stand in.

Movement eases rather than jumps: the progress bar, the rail, scrolling, the composer growing with its draft, and its edge lightening while typing. A new run fades in as it is sent. Nothing moves on hover. The window keeps drawing while it is resized, and once nothing is moving and no run is in progress, the frame loop sleeps until there is input.

## Layout

| Module | Purpose |
| --- | --- |
| `Client` | The connection to the server, published as `NetworkEvent`s |
| `Client.Settings` | Account metadata and model catalogs; credentials stay in `openai-auth` |
| `Client.Commands`, `Client.Tokens` | `/` commands and the average token rate, as in the terminal client |
| `UI.State` | Pure application and conversation updates; chat actions carry their originating chat ID |
| `Theme.Palette` | The palette, read from `themes/orng.toml`, with orng Dark accents on neutral charcoal |
| `app/UI` | Window lifecycle, frame scheduling, top-level layout and action routing |
| `app/UI/Sidebar`, `app/UI/Settings`, `app/UI/Tools` | Navigation and auxiliary windows, each receiving only its own view data |
| `app/UI/Transcript` | Run rendering, scroll tracking and chat/run-scoped animations |
| `app/UI/Composer` | Text-buffer and focus state, model selection, and one measured layout shared by sizing and drawing |
| `UI.Motion` | Backend-independent animation values, sampled at most once per frame, with typed keys |
| `app/UI/Appearance`, `app/UI/Layout` | Shared read-only fonts/palette and small layout primitives |
| `app/UI/Widgets`, `app/UI/Fonts`, `app/Theme` | Haskell drawing and interaction for controls, text, icons, fonts and styling |
| `app/ImGui/Extras`, `cbits/imgui_extras.cpp` | Minimal missing bindings, ABI/struct access and the native text-input callback |

The library holds everything that does not touch the window, and `cabal test hs-gui-client` checks its properties and animation regressions. Connection failure and quitting are application-wide; drafts, model choices and runs belong to conversations. Components emit chat actions that the window tags with the chat shown in that frame, so navigation cannot redirect an edit or acknowledgement to a different chat.

Animation keys use the chat ID and each run's oldest-first ordinal, which stays stable as runs are appended. Animation values live with the session's chat history, with a fixed number of entries per run across retries. The frame loop supplies time; measuring and drawing the same animation returns the same value within a frame. Composer layout is measured once and reused when reserving transcript space and drawing controls.

`cbits/imgui` is a copy of the `imgui.h` and `imconfig.h` that dear-imgui 2.4.1 builds Dear ImGui 1.91.9b from, so the extras compile against the same structs; the cabal file pins that version, and the client refuses to start if the layouts differ. Updating dear-imgui means copying its headers again.

Dear ImGui 1.91's text boxes do not wrap, so a long line in the composer scrolls sideways; messages in the conversation wrap.
