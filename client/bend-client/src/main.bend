import Base
import ./bytes.bend as Bytes
import ./frame.bend as Frame
import ./reply.bend as Reply
import ./protocol.bend as Protocol
import ./session.bend as Session

# All application I/O uses Base effects. The only recursive driver consumes fuel.
def Next() -> Type:
  Socket -> File -> Session.State -> List<&2, U32> -> Frame.Frame -> Frame.Frame -> IO(Unit)

def stop(socket: Socket, input: File, status: U32, message: String) -> IO(Unit):
  do IO<Unit>:
    Socket.close(socket)
    File.close(input)
    IO.die(Unit, status, message)

def sent(pair: Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit>, input: File,
  next: Socket -> File -> IO(Unit)) -> IO(Unit):
  (socket, result) = pair
  match result:
    case Done{unit}:
      next(socket, input)
    case Fail{((code, message), unsent)}:
      stop(socket, input, 1, "bend-client: send failed: " ++ message)

def actions(values: List<&2, Session.Action>, socket: Socket, input: File,
  next: Socket -> File -> IO(Unit)) -> IO(Unit):
  match values:
    case Nil{}:
      next(socket, input)
    case Con{Session.Display{text}, rest}:
      do IO<Unit>:
        IO.write(text)
        actions(rest, socket, input, next)
    case Con{Session.Send{bytes}, rest}:
      do IO<Unit>:
        result : Socket & Result<&1, &1, (U32 & String) & List<&2, U32>, Unit> <- TCP.send_bytes(socket, bytes)
        sent(result, input, s => f => actions(rest, s, f, next))
    case Con{Session.Stop{status, message}, rest}:
      stop(socket, input, status, message)

def apply(decision: Session.Decision, socket: Socket, input: File,
  next: Socket -> File -> Session.State -> IO(Unit)) -> IO(Unit):
  match decision:
    case Session.Decision{state, work}:
      actions(work, socket, input, s => f => next(s, f, state))

def fail(socket: Socket, input: File, message: String) -> IO(Unit):
  actions([Session.Stop{1, "bend-client: " ++ message}], socket, input,
    s => f => stop(s, f, 1, "unreachable"))

def input_text(text: Maybe<&2, String>, socket: Socket, input: File, pending: List<&2, U32>, color: Bool, next: Next()) -> IO(Unit):
  match text:
    case None{}:
      fail(socket, input, "invalid UTF-8 in prompt")
    case Some{text}:
      apply(Session.step(Session.Idle{}, Session.Entered{text}, color), socket, input,
        s => f => state => next(s, f, state, pending, Frame.start(1048576n), Frame.start(8388608n)))

def prompt_feed(feed: Frame.Feed, socket: Socket, input: File, color: Bool, next: Next()) -> IO(Unit):
  match feed:
    case Frame.TooLarge{}:
      fail(socket, input, "input line exceeds 1 MiB")
    case Frame.More{frame}:
      next(socket, input, Session.Idle{}, Nil{}, frame, Frame.start(8388608n))
    case Frame.Full{bytes, rest}:
      input_text(Bytes.decode(bytes), socket, input, rest, color, next)

def input_end(frame: Frame.Frame, socket: Socket, input: File, color: Bool, next: Next()) -> IO(Unit):
  match frame:
    case Frame.Frame{remaining, Nil{}}:
      apply(Session.step(Session.Idle{}, Session.EndOfInput{}, color), socket, input,
        s => f => state => next(s, f, state, Nil{}, Frame.start(1048576n), Frame.start(8388608n)))
    case Frame.Frame{remaining, Con{h, t}}:
      input_text(Bytes.decode(List.reverse(&2, U32, h <> t)), socket, input, Nil{}, color, next)

def input_read(pair: File & Result<&1, &1, U32 & String, List<&2, U32>>, socket: Socket,
  frame: Frame.Frame, color: Bool, next: Next()) -> IO(Unit):
  (input, result) = pair
  match result:
    case Fail{(code, message)}:
      fail(socket, input, message)
    case Done{Nil{}}:
      input_end(frame, socket, input, color, next)
    case Done{Con{h, t}}:
      prompt_feed(Frame.feed(h <> t, frame), socket, input, color, next)

def prompt(pending: List<&2, U32>, frame: Frame.Frame, socket: Socket, input: File,
  color: Bool, next: Next()) -> IO(Unit):
  match pending:
    case Nil{}:
      do IO<Unit>:
        pair : File & Result<&1, &1, U32 & String, List<&2, U32>> <- File.read_bytes(input, 4096)
        input_read(pair, socket, frame, color, next)
    case Con{h, t}:
      prompt_feed(Frame.feed(h <> t, frame), socket, input, color, next)

def decode_reply(bytes: List<&2, U32>) -> Maybe<&2, List<&2, Protocol.Message>>:
  do Maybe<&2, List<&2, Protocol.Message>>:
    text : String <- Bytes.decode(bytes)
    Reply.decode(text)

def reply_decoded(result: Maybe<&2, List<&2, Protocol.Message>>, socket: Socket, input: File,
  pending: List<&2, U32>, input_frame: Frame.Frame, color: Bool, next: Next()) -> IO(Unit):
  match result:
    case None{}:
      fail(socket, input, "invalid server response (JSON, UTF-8, or message schema)")
    case Some{messages}:
      apply(Session.step(Session.Waiting{}, Session.Received{messages}, color), socket, input,
        s => f => state => next(s, f, state, pending, input_frame, Frame.start(8388608n)))

def reply_feed(feed: Frame.Feed, socket: Socket, input: File, pending: List<&2, U32>, input_frame: Frame.Frame,
  color: Bool, next: Next()) -> IO(Unit):
  match feed:
    case Frame.TooLarge{}:
      fail(socket, input, "reply exceeds 8 MiB")
    case Frame.More{frame}:
      next(socket, input, Session.Waiting{}, pending, input_frame, frame)
    case Frame.Full{bytes, Nil{}}:
      reply_decoded(decode_reply(bytes), socket, input, pending, input_frame, color, next)
    case Frame.Full{bytes, Con{h, t}}:
      fail(socket, input, "unexpected bytes after reply frame")

def reply_read(pair: Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>>, input: File,
  pending: List<&2, U32>, input_frame: Frame.Frame, reply_frame: Frame.Frame, color: Bool, next: Next()) -> IO(Unit):
  (socket, result) = pair
  match result:
    case Fail{(code, message)}:
      fail(socket, input, message)
    case Done{None{}}:
      fail(socket, input, "server disconnected before a complete reply arrived")
    case Done{Some{bytes}}:
      reply_feed(Frame.feed(bytes, reply_frame), socket, input, pending, input_frame, color, next)

def tick(state: Session.State, socket: Socket, input: File, pending: List<&2, U32>, input_frame: Frame.Frame,
  reply_frame: Frame.Frame, color: Bool, next: Next()) -> IO(Unit):
  match state:
    case Session.Idle{}:
      prompt(pending, input_frame, socket, input, color, next)
    case Session.Waiting{}:
      do IO<Unit>:
        pair : Socket & Result<&1, &1, U32 & String, Maybe<&1, List<&2, U32>>> <- TCP.recv_bytes(socket, 4096)
        reply_read(pair, input, pending, input_frame, reply_frame, color, next)
    case Session.Stopped{}:
      stop(socket, input, 0, "")

def run(fuel: Nat, socket: Socket, input: File, state: Session.State, pending: List<&2, U32>,
  input_frame: Frame.Frame, reply_frame: Frame.Frame, +color: Bool) -> IO(Unit):
  match fuel:
    case 0n:
      apply(Session.step(state, Session.Exhausted{}, color), socket, input,
        s => f => state => stop(s, f, 1, "session event budget exhausted"))
    case 1n+p:
      tick(state, socket, input, pending, input_frame, reply_frame, color,
        s => f => state2 => buffer => frame1 => frame2 => run(p, s, f, state2, buffer, frame1, frame2, color))

def connected(result: Result<&1, &1, U32 & String, Socket>, input: File, fuel: Nat, color: Bool) -> IO(Unit):
  match result:
    case Fail{(code, message)}:
      do IO<Unit>:
        File.close(input)
        IO.die(Unit, 1, "bend-client: connect failed: " ++ message)
    case Done{socket}:
      run(fuel, socket, input, Session.Idle{}, Nil{}, Frame.start(1048576n), Frame.start(8388608n), color)

def session(port: U32, fuel: Nat, color: Bool) -> IO(Unit):
  do IO<Unit>:
    input : File <- IO.try(File, File.open("/dev/stdin", "r"))
    result : Result<&1, &1, U32 & String, Socket> <- TCP.connect("127.0.0.1", port)
    connected(result, input, fuel, color)

def usage() -> String:
  "Usage: bend-client [--port PORT] [--steps N] [--color]\nDefault: 127.0.0.1:7777, 100000 driver steps, plain text.\nEnter sends; /quit, Ctrl-D or Ctrl-C exits."

type Config is Data:
  Config{port: U32, steps: Nat, color: Bool}

def set_number(ok: Bool, port: U32, steps: Nat, color: Bool) -> Maybe<&2, Config>:
  match ok:
    case True{}:
      Some{Config{port, steps, color}}
    case False{}:
      None{}

def set_port(value: String, config: Config) -> Maybe<&2, Config>:
  match config:
    case Config{old, steps, color}:
      do Maybe<&2, Config>:
        +port : U32 <- U32.read(value)
        set_number(U32.is_ge(port, 1) && U32.is_le(port, 65535), port, steps, color)

def set_steps(value: String, config: Config) -> Maybe<&2, Config>:
  match config:
    case Config{port, old, color}:
      do Maybe<&2, Config>:
        +steps : U32 <- U32.read(value)
        set_number(U32.is_le(steps, 10000000), port, U32.to_nat(steps), color)

def parse_args(args: List<String>, config: Config) -> Maybe<&2, Config>:
  match args config:
    case Nil{} config:
      Some{config}
    case Con{"--color", rest} Config{port, steps, color}:
      parse_args(rest, Config{port, steps, True{}})
    case Con{"--port", Con{value, rest}} config:
      do Maybe<&2, Config>:
        next : Config <- set_port(value, config)
        parse_args(rest, next)
    case Con{"--steps", Con{value, rest}} config:
      do Maybe<&2, Config>:
        next : Config <- set_steps(value, config)
        parse_args(rest, next)
    case other config:
      None{}

def start(config: Maybe<&2, Config>) -> IO(Unit):
  match config:
    case None{}:
      IO.die(Unit, 1, usage())
    case Some{Config{port, steps, color}}:
      session(port, steps, color)

def args(values: List<String>) -> IO(Unit):
  match values:
    case Con{"--help", Nil{}}:
      IO.print(usage())
    case other:
      start(parse_args(other, Config{7777, 100000n, False{}}))

def main() -> IO(Unit):
  do IO<Unit>:
    values : List<String> <- IO.args()
    args(List.tail(&1, String, values))
