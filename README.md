# Harness

A Haskell LLM harness with a Brick terminal interface, an AF_UNIX server, and a shared `core` package.

| Package | Purpose |
| --- | --- |
| `core/` | `Chat`, messages, models, tool definitions and requests, JSON instances, and stream protocol |
| `server/` | Model requests, tool execution, and the Unix socket listener |
| `client/hs-tui-client/` | Brick UI built directly around the shared `Chat` type |

## Run

Build from the repository root with GHC and Cabal:

```sh
cabal build all
```

Set `INCEPTION_API_KEY` in your environment or the root `.env` file, then start the server from the repository root:

```sh
cabal run server
```

In a second terminal:

```sh
cabal run hs-tui-client
```

The server uses Mercury 3 and provides `ReadFile` and `Discover`. Tool paths resolve against the server's working directory. Tool schemas are packaged with the server.

The root Nix flake includes all three packages and a shared development shell:

```sh
nix develop path:. -c cabal build all
nix build path:.
nix build 'path:.#hs-tui-client:exe:hs-tui-client'
```

## Interface

The client shows the chronological conversation, distinct user/assistant/tool entries, model and tool metadata, and response speed. Tool results display in full in the scrollable transcript. Requests run in the background, so scrolling and drafting remain responsive while the server works. The server returns a complete `Chat` after each turn, including any tool cycles.

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

Piped input or output uses a plain transcript through the same client and shared types. `/quit` exits in either mode.

## Shared types and protocol

Server and client both depend on the local `core` library and `import Core`. `Chat` contains the full message history, model, and available tools. Its messages are stored newest first; the UI reverses them for display. Tool results retain their call IDs, and assistant messages retain tool requests and optional throughput metadata.

The only listener is `/tmp/harness.sock`, an `AF_UNIX` stream socket. Requests are `Message` JSON objects and replies are complete `Chat` JSON objects, encoded as UTF-8 without newline framing. Incremental parsing handles split JSON/UTF-8, large messages, and adjacent values while preserving unread bytes. Each connection has its own conversation.
