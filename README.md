# Harness

This is an LLM-harness written in Haskell. It is split into a client-server architecture. The server listens to a UNIX socket and any client can connect to it via that socket.

The core type that clients need to follow is the `Chat` type which the server will send down the socket. It contains the entire history of messages and tool calls, along with tool definitions, and the model being used.

Tools are statically defined types, which define how they are implemented, and so far we have only `ReadFile` and `Discover`.

At the moment as well we hardcode the `Mercury3` model and to use it we need to provide a `INCEPTION_API_KEY` envvar.

## Repo Structure

| | |
| ------------- | ------------- |
| server/  | Hand-written Haskell server code  |
| hs-client-minimal/  | Hand-written Haskell test client code |
| hs-client/  | LLM-written Haskell client code |
