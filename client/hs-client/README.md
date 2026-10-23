# hs-client

A small, scrollback-friendly terminal UI for harness. Cyan `you` prompts and
green `agent` replies and yellow `tool` requests and results keep the conversation clear,
with no panels or extra chrome.
Multiline replies keep their indentation. Input supports editing and session history.

Start the harness server, then run from this directory:

```sh
cabal run hs-client
# Or use the pinned Nix environment:
nix develop path:. -c cabal run hs-client
```

```text
you   > explain this function
agent > I'll read the file first.
tool  > ReadFile {"path":"src/Main.hs"}
tool  > module Main (main) where ...
agent > This function connects to the harness server.
        150.0 tokens/s

you   >
```

Enter sends a message; Up/Down recalls previous input. `/quit`, Ctrl-D on an
empty prompt, or Ctrl-C exits. Pass a socket path with
`cabal run hs-client -- /path/to/harness.sock`; the default is `/tmp/harness.sock`.
Set `NO_COLOR=1` for plain text. Redirected output also omits colour.

The client reads the server's JSON responses and displays all agent messages,
tool requests, and tool results from the current turn in chronological order,
including responses split across socket reads. Earlier conversation history
and the echoed user message are not repeated.
Each tool request shows its function name and arguments. Replies containing only
tool requests display those requests without a missing-text placeholder.
When an agent message includes `tokens_per_second`, its speed appears underneath
the response with one decimal place. Missing or null speeds are omitted.
The server currently reads
requests in a single 1024-byte chunk, so larger encoded requests are rejected
locally. Requests are still subject to that server framing limitation.

Run the socket regression tests after building:

```sh
cabal build hs-client
python3 tests/test_socket.py
```
