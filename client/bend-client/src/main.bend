import Base
import ./protocol.bend as P

def fail(socket: Socket, message: String) -> IO(Unit):
  do IO<Unit>:
    Socket.close(socket)
    IO.die(Unit, 1, "bend-client: " ++ message)

def show_turn(valid: Bool, messages: List<&2, P.Message>, color: Bool) -> IO(Unit):
  match valid:
    case False{}:
      IO.die(Unit, 1, "bend-client: server response contains no agent reply.")
    case True{}:
      IO.print(P.render(List.reverse(&2, P.Message, messages), color))

def reply(result: Result<&1, &1, U32 & String, List<&2, P.Message>>, socket: Socket,
  +color: Bool) -> IO(Socket):
  match result:
    case Fail{(code, message)}:
      do IO<Socket>:
        Socket.close(socket)
        IO.die(Socket, 1, "bend-client: " ++ message)
    case Done{messages}:
      do IO<Socket>:
        +turn : List<&2, P.Message> = P.current(messages)
        show_turn(P.has_agent(turn), turn, color)
        return socket

def unpack_reply(pair: Socket & Result<&1, &1, U32 & String, List<&2, P.Message>>, color: Bool) -> IO(Socket):
  (socket, result) = pair
  reply(result, socket, color)

def send(payload: Maybe<&2, String>, socket: Socket, color: Bool) -> IO(Socket):
  match payload:
    case None{}:
      do IO<Socket>:
        IO.print("Message too long for the server (1024 encoded bytes maximum).")
        return socket
    case Some{text}:
      do IO<Socket>:
        pair : Socket & Result<&1, &1, U32 & String, List<&2, P.Message>> <- P.exchange(socket, text)
        unpack_reply(pair, color)

def echo(interactive: Bool, color: Bool, text: String) -> IO(Unit):
  match interactive:
    case True{}:
      IO.pure(Unit, Unit{})
    case False{}:
      IO.write(P.line(color, "1;36", "you   ", text))

def prompt(interactive: Bool, color: Bool) -> IO(Unit):
  match interactive:
    case True{}:
      IO.write(P.paint(color, "1;36", "you   ") ++ "> ")
    case False{}:
      IO.pure(Unit, Unit{})

# This is the only unbounded recursion: the terminal session ends on EOF/quit.
@unsafe
def loop(socket: Socket, +interactive: Bool, +color: Bool) -> IO(Unit):
  do IO<Unit>:
    prompt(interactive, color)
    input : Result<&1, &1, U32 & String, Maybe<&2, String>> <- P.input()
    accept(input, socket, interactive, color)

@unsafe
def accept(input: Result<&1, &1, U32 & String, Maybe<&2, String>>, socket: Socket,
  +interactive: Bool, +color: Bool) -> IO(Unit):
  match input:
    case Fail{(code, message)}:
      fail(socket, message)
    case Done{None{}}:
      Socket.close(socket)
    case Done{Some{+text}}:
      command(String.eq(String.trim(text), "/quit"), String.is_empty(String.trim(text)),
        text, socket, interactive, color)

@unsafe
def command(quit: Bool, blank: Bool, +text: String, socket: Socket,
  +interactive: Bool, +color: Bool) -> IO(Unit):
  match quit blank:
    case True{} _:
      Socket.close(socket)
    case False{} True{}:
      loop(socket, interactive, color)
    case False{} False{}:
      do IO<Unit>:
        echo(interactive, color, text)
        payload : Maybe<&2, String> <- P.encode(text)
        socket : Socket <- send(payload, socket, color)
        loop(socket, interactive, color)

def session(path: String) -> IO(Unit):
  do IO<Unit>:
    socket : Socket <- IO.try(Socket, P.connect(path))
    interactive : Bool <- P.interactive()
    color : Bool <- P.color()
    loop(socket, interactive, color)

def usage() -> String:
  "Usage: bend-client [SOCKET]\nDefault: /tmp/harness.sock\nEnter sends; /quit, Ctrl-D or Ctrl-C exits."

def option(help: Bool, invalid: Bool, path: String) -> IO(Unit):
  match help invalid:
    case True{} _:
      IO.print(usage())
    case False{} True{}:
      IO.die(Unit, 1, usage())
    case False{} False{}:
      session(path)

def args(values: List<String>) -> IO(Unit):
  match values:
    case Nil{}:
      session("/tmp/harness.sock")
    case Con{+path, Nil{}}:
      option(String.eq(path, "--help"), String.starts_with(path, "-"), path)
    case Con{path, rest}:
      IO.die(Unit, 1, usage())

def main() -> IO(Unit):
  do IO<Unit>:
    values : List<String> <- IO.args()
    args(List.tail(&1, String, values))
