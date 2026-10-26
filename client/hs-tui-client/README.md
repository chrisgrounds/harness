# Harness terminal client

A Brick interface using the `Chat`, `Message`, and tool types from the shared `core` package. The server's complete conversation is the UI's state, including model metadata, tool calls, tool results, and response speed.

From the repository root, start the server and client in separate terminals:

```sh
cabal run server
cabal run hs-tui-client
```

Pass a socket path after `--` to override `/tmp/harness.sock`.

The centered conversation view uses restrained colors, clear role labels, a scrollable transcript, compact tool previews, and a multiline composer. Network requests run in a worker thread; the interface remains usable while the waiting indicator animates. You can compose the next message while a reply is pending, then send it once the current turn completes.

| Key | Action |
| --- | --- |
| Enter / Alt-Enter | Send / newline |
| Alt-Up / Alt-Down | Recall input history; restore the draft |
| Page Up / Page Down | Scroll |
| Ctrl-Home / Ctrl-End | First message / follow latest |
| Ctrl-T | Expand or collapse tool output |
| F1 | Toggle shortcut help |
| Ctrl-C / Esc | Exit; Esc closes help first |
| Ctrl-D | Exit with an empty composer |

Set `NO_COLOR=1` for terminal default colors. Redirected input or output automatically uses a plain transcript. `/quit` exits in either mode. The client preserves fragmented socket responses, decodes the full shared `Chat`, and displays each turn once in plain mode.
