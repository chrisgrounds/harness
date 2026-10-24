import Base

type Role is Data:
  User{}
  Agent{}
  Tool{}
  System{}

type Call is Data:
  Call{name: String, arguments: String}

type Message is Data:
  Message{role: Role, content: Maybe<&2, String>, calls: List<&2, Call>, speed: Maybe<&2, String>}

# The server sends newest-first history. The latest user message bounds a turn.
def current(messages: List<&2, Message>) -> List<&2, Message>:
  match messages:
    case Nil{}:
      Nil{}
    case Con{Message{User{}, text, calls, speed}, rest}:
      Nil{}
    case Con{Message{role, text, calls, speed}, rest}:
      Message{role, text, calls, speed} <> current(rest)

def is_agent(message: Message) -> Bool:
  match message:
    case Message{Agent{}, text, calls, speed}:
      True{}
    case Message{role, text, calls, speed}:
      False{}

def has_agent(messages: List<&2, Message>) -> Bool:
  List.any(~&2, ~Message, ~is_agent, messages)

def paint(color: Bool, code: String, text: String) -> String:
  match color:
    case True{}:
      "\u{1b}[" ++ code ++ "m" ++ text ++ "\u{1b}[0m"
    case False{}:
      text

def safe_char(c: Char) -> Bool:
  +n = Char.to_u32(c)
  Bool.or(Bool.or(U32.is_eq(n, 10), U32.is_eq(n, 9)),
    Bool.and(U32.is_ge(n, 32), Bool.or(U32.is_lt(n, 127), U32.is_gt(n, 159))))

def safe(text: String) -> String:
  String.from_list(List.filter(~Char, ~safe_char, String.to_list(text)))

# Sanitize and indent in one traversal. Keeping each character decision explicit
# lets PROOF.bend show that formatting cannot reintroduce unsafe controls.
def format_step(c: Char, rest: String, allowed: Bool, newline: Bool) -> String:
  match allowed newline:
    case False{} _:
      rest
    case True{} True{}:
      "\n          " ++ rest
    case True{} False{}:
      SCon{c, rest}

def format_body(text: String) -> String:
  match text:
    case SNil{}:
      ""
    case SCon{+h, t}:
      format_step(h, format_body(t), safe_char(h), Char.is_eq(h, '\n'))

def line(color: Bool, code: String, label: String, text: String) -> String:
  paint(color, code, label) ++ "> " ++ format_body(text) ++ "\n"

def render_calls(calls: List<&2, Call>, +color: Bool) -> String:
  match calls:
    case Nil{}:
      ""
    case Con{Call{name, arguments}, rest}:
      line(color, "33", "tool  ", name ++ " " ++ arguments) ++ render_calls(rest, color)

def render_empty(calls: List<&2, Call>, color: Bool) -> String:
  match calls:
    case Nil{}:
      line(color, "1;32", "agent ", "(no text reply)")
    case Con{call, rest}:
      ""

def render_text(text: Maybe<&2, String>, calls: List<&2, Call>, color: Bool) -> String:
  match text:
    case None{}:
      render_empty(calls, color)
    case Some{SNil{}}:
      render_empty(calls, color)
    case Some{SCon{h, t}}:
      line(color, "1;32", "agent ", SCon{h, t})

def render_speed(speed: Maybe<&2, String>, color: Bool) -> String:
  match speed:
    case None{}:
      ""
    case Some{text}:
      paint(color, "2", "        " ++ text ++ " tokens/s") ++ "\n"

def render_message(message: Message, +color: Bool) -> String:
  match message:
    case Message{Agent{}, text, +calls, speed}:
      render_text(text, calls, color) ++ render_calls(calls, color) ++ render_speed(speed, color)
    case Message{Tool{}, None{}, calls, speed}:
      line(color, "33", "tool  ", "(no tool output)")
    case Message{Tool{}, Some{text}, calls, speed}:
      line(color, "33", "tool  ", text)
    case Message{role, text, calls, speed}:
      ""

def render(messages: List<&2, Message>, +color: Bool) -> String:
  match messages:
    case Nil{}:
      ""
    case Con{message, rest}:
      render_message(message, color) ++ render(rest, color)
