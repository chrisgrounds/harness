import Base
import ./bytes.bend as Bytes
import ./json.bend as Json
import ./protocol.bend as Protocol

type State is Data:
  Idle{}
  Waiting{}
  Stopped{}

type InputEvent is Data:
  Entered{text: String}
  Received{messages: List<&2, Protocol.Message>}
  Failed{message: String}
  EndOfInput{}
  Exhausted{}

type Action is Data:
  Display{text: String}
  Send{bytes: List<&2, U32>}
  Stop{status: U32, message: String}

type Decision is Data:
  Decision{state: State, actions: List<&2, Action>}

def encoded(text: String) -> List<&2, U32>:
  Bytes.encode(Json.request(text))

def fits(+bytes: List<&2, U32>) -> Bool:
  Nat.is_le(List.length(&2, U32, bytes), 1024n)

def reject_message() -> String:
  "Message too long for the server (1024 encoded bytes maximum).\n"

def accepted(ok: Bool, bytes: List<&2, U32>) -> Decision:
  match ok:
    case True{}:
      Decision{Waiting{}, [Send{List.append(&2, U32, bytes, [10])}]}
    case False{}:
      Decision{Idle{}, [Display{reject_message()}]}

def command(quit: Bool, blank: Bool, +text: String, color: Bool) -> Decision:
  match quit blank:
    case True{} _:
      Decision{Stopped{}, [Stop{0, ""}]}
    case False{} True{}:
      Decision{Idle{}, Nil{}}
    case False{} False{}:
      +bytes = encoded(text)
      accepted(fits(bytes), bytes)

def submit(+text: String, color: Bool) -> Decision:
  command(String.eq(String.trim(text), "/quit"), String.is_empty(String.trim(text)), text, color)

def display_turn(messages: List<&2, Protocol.Message>, color: Bool) -> String:
  Protocol.render(List.reverse(&2, Protocol.Message, messages), color) ++ "\n"

def reply(valid: Bool, messages: List<&2, Protocol.Message>, color: Bool) -> Decision:
  match valid:
    case True{}:
      Decision{Idle{}, [Display{display_turn(messages, color)}]}
    case False{}:
      Decision{Stopped{}, [Stop{1, "server response contains no agent reply"}]}

def step(state: State, event: InputEvent, color: Bool) -> Decision:
  match state event:
    case Stopped{} event:
      Decision{Stopped{}, Nil{}}
    case state Failed{message}:
      Decision{Stopped{}, [Stop{1, message}]}
    case state Exhausted{}:
      Decision{Stopped{}, [Stop{1, "session event budget exhausted"}]}
    case state EndOfInput{}:
      Decision{Stopped{}, [Stop{0, ""}]}
    case Idle{} Entered{text}:
      submit(text, color)
    case Waiting{} Received{messages}:
      +turn = Protocol.current(messages)
      reply(Protocol.has_agent(turn), turn, color)
    case state event:
      Decision{Stopped{}, [Stop{1, "unexpected session event"}]}

# The driver writes startup before attempting any blocking connection or input.
def startup(port: U32) -> String:
  Protocol.banner() ++ "Connecting to 127.0.0.1:" ++ U32.show(port) ++ "...\n"

# Only an idle session invites input; waiting/stopped sessions show no prompt.
def ready(state: State, color: Bool) -> List<&2, Action>:
  match state:
    case Idle{}:
      [Display{Protocol.user_prompt(color)}]
    case state:
      Nil{}
