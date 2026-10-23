# rs-gui-client

A native Rust + egui desktop client for Harness, with a dark theme, mint accents,
selectable conversation cards, expandable tool calls and results, and a multiline
composer. Socket I/O runs on a background thread so the window stays responsive.

Start the Harness server, then run from this directory:

```sh
nix run path:.
# Or develop with the pinned Rust toolchain and GUI libraries:
nix develop path:. -c cargo run
```

The default socket is `/tmp/harness.sock`. Override it with:

```sh
nix run path:. -- /path/to/harness.sock
# Or:
nix develop path:. -c cargo run -- /path/to/harness.sock
```

The Nix flake follows the other clients' per-directory layout and currently targets
`x86_64-linux`. It provides a development shell, a default package, and an app.
Both X11 and Wayland are supported; running the window requires a graphical
session and working OpenGL drivers. Cargo and Nix dependencies are locked.

If Nix cannot access your system's OpenGL drivers (for example, on a non-NixOS
host), use the bundled Mesa software renderer:

```sh
nix run path:.#software
```

- Enter sends; Shift+Enter inserts a newline.
- Alt+Up/Down recalls sent messages and restores your unfinished draft.
- `/quit` or the window's close button exits.
- Copy buttons and selectable text let you reuse replies. Tool details expand or
  collapse, with separately scrollable long outputs.
- Connection errors appear in the window. Reconnect starts a fresh server session
  and clears the displayed conversation; input history and the draft remain.

Like `hs-tui-client` (formerly `hs-client`), the client keeps one Unix socket connection and displays the
current turn in chronological order, without repeating the server's earlier
history or echoed user message. It handles JSON and UTF-8 split across socket
reads, tool-only assistant messages, multiple tool cycles, missing text, and
optional tokens-per-second values displayed with one decimal place.

The server currently receives requests in a single 1024-byte read. The composer
shows the encoded JSON size and rejects larger messages locally. This mirrors
the terminal client; the server's existing framing limitation still applies. Replies
appear when the server sends the completed turn, including its tool activity.
Failed requests are never automatically retried.

Build and verify:

```sh
nix build path:.
nix flake check path:.
nix develop path:. -c cargo test --locked
nix develop path:. -c cargo fmt --check
nix develop path:. -c cargo clippy --locked --all-targets -- -D warnings
```

Socket regression tests use a local mock server; no API key or display is needed.
