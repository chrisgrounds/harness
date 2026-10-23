# hs-client

A small, scrollback-friendly terminal UI for harness. Cyan `you` prompts and
green `agent` replies and yellow `tool` requests keep the conversation clear,
with no panels or extra chrome.
Multiline replies keep their indentation. Input supports editing and session history.

Start the harness server, then run from this directory:

```sh
cabal run hs-client
# Or use the pinned Nix environment:
nix develop path:. -c cabal run hs-client
```

```text
you     > explain this function
agent   > I'll read the file first.
tool    > ReadFile {"path":"src/Main.hs"}

you     >
```

Enter sends a message; Up/Down recalls previous input. `/quit`, Ctrl-D on an
empty prompt, or Ctrl-C exits. Pass a socket path with
`cabal run hs-client -- /path/to/harness.sock`; the default is `/tmp/harness.sock`.
Set `NO_COLOR=1` for plain text. Redirected output also omits colour.

The client reads the server's JSON responses and displays the newest agent
reply and its tool requests, including responses split across socket reads.
Each tool request shows its function name and arguments. Replies containing only
tool requests display those requests without a missing-text placeholder.
The server currently reads
requests in a single 1024-byte chunk, so larger encoded requests are rejected
locally. Requests are still subject to that server framing limitation.
