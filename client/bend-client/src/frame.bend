import Base

# Incremental LF framing over bytes; UTF-8 is decoded only after the full frame.
# The delimiter is not part of the frame's byte budget.
type Frame is Data:
  Frame{remaining: Nat, reversed: List<&2, U32>}

type Feed is Data:
  More{frame: Frame}
  Full{bytes: List<&2, U32>, rest: List<&2, U32>}
  TooLarge{}

def feed_step(newline: Bool, byte: U32, rest: List<&2, U32>, frame: Frame,
  next: Frame -> Feed) -> Feed:
  match newline frame:
    case True{} Frame{remaining, reversed}:
      Full{List.reverse(&2, U32, reversed), rest}
    case False{} Frame{0n, reversed}:
      TooLarge{}
    case False{} Frame{1n+p, reversed}:
      next(Frame{p, byte <> reversed})

def feed(bytes: List<&2, U32>, frame: Frame) -> Feed:
  match bytes:
    case Nil{}:
      More{frame}
    case Con{+h, +t}:
      feed_step(U32.is_eq(h, 10), h, t, frame, next => feed(t, next))

def start(limit: Nat) -> Frame:
  Frame{limit, Nil{}}
