# Harness

This is an LLM-harness written in Haskell. It is split into a client-server architecture. The server runs on a UNIX socket and any client can connect to it.

The core type that clients need to follow is the `Chat` type which the server will send down the socket. It contains the entire history of messages and tool calls, along with tool definitions, and the model being used.

Tools are statically defined types, which define how they are implemented.

## Repo Structure

| | |
| ------------- | ------------- |
| server/  | Hand-written Haskell server code  |
| hs-client-minimal/  | Hand-written Haskell test client code |
| hs-client/  | LLM-written Haskell client code |
