# Harness

This is an LLM-harness written in Haskell. It is split into a client-server architecture. The server runs on a UNIX socket and any client can connect to it.

## Repo Structure

| | |
| ------------- | ------------- |
| server/  | Hand-written Haskell server code  |
| hs-client-minimal/  | Hand-written Haskell test client code |
| hs-client/  | LLM-written Haskell client code |

## TODO

1. Implement tool calls
2. Add verification loop
