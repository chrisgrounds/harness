import Base

# Unicode scalar values only: reject surrogate code points and out-of-range input.
def scalar(+n: U32) -> Bool:
  U32.is_le(n, 1114111) && (U32.is_lt(n, 55296) || U32.is_gt(n, 57343))

def encode_char(n: U32, a: Bool, b: Bool, c: Bool) -> List<&2, U32>:
  match a b c:
    case True{} _ _:
      [n]
    case False{} True{} _:
      +v = n
      [U32.add(192, U32.div(v, 64)), U32.add(128, U32.mod(v, 64))]
    case False{} False{} True{}:
      +v = n
      [U32.add(224, U32.div(v, 4096)), U32.add(128, U32.mod(U32.div(v, 64), 64)), U32.add(128, U32.mod(v, 64))]
    case False{} False{} False{}:
      +v = n
      [U32.add(240, U32.div(v, 262144)), U32.add(128, U32.mod(U32.div(v, 4096), 64)), U32.add(128, U32.mod(U32.div(v, 64), 64)), U32.add(128, U32.mod(v, 64))]

def encode(text: String) -> List<&2, U32>:
  match text:
    case SNil{}:
      Nil{}
    case SCon{Chr{+n}, rest}:
      List.append(&2, U32, encode_char(n, U32.is_lt(n, 128), U32.is_lt(n, 2048), U32.is_lt(n, 65536)), encode(rest))

type Decoder is Data:
  Decoder{left: U32, code: U32, minimum: U32, reversed: String}

def choose(ok: Bool, value: Decoder) -> Maybe<&2, Decoder>:
  match ok:
    case True{}:
      Some{value}
    case False{}:
      None{}

def continuation(done: Bool, +code: U32, minimum: U32, left: U32, text: String) -> Maybe<&2, Decoder>:
  match done:
    case True{}:
      choose(U32.is_ge(code, minimum) && scalar(code), Decoder{0, 0, 0, SCon{Chr{code}, text}})
    case False{}:
      Some{Decoder{left, code, minimum, text}}

def extend(valid: Bool, +n: U32, +left: U32, code: U32, minimum: U32, text: String) -> Maybe<&2, Decoder>:
  match valid:
    case False{}:
      None{}
    case True{}:
      continuation(U32.is_eq(left, 1), U32.add(U32.mul(code, 64), U32.sub(n, 128)), minimum, U32.sub(left, 1), text)

def leading(ascii: Bool, two: Bool, three: Bool, four: Bool, n: U32, text: String) -> Maybe<&2, Decoder>:
  match ascii two three four:
    case True{} _ _ _:
      Some{Decoder{0, 0, 0, SCon{Chr{n}, text}}}
    case False{} True{} _ _:
      Some{Decoder{1, U32.sub(n, 192), 128, text}}
    case False{} False{} True{} _:
      Some{Decoder{2, U32.sub(n, 224), 2048, text}}
    case False{} False{} False{} True{}:
      Some{Decoder{3, U32.sub(n, 240), 65536, text}}
    case False{} False{} False{} False{}:
      None{}

def byte_start(fresh: Bool, +n: U32, left: U32, code: U32, minimum: U32, text: String) -> Maybe<&2, Decoder>:
  match fresh:
    case True{}:
      leading(U32.is_lt(n, 128), U32.is_ge(n, 194) && U32.is_le(n, 223), U32.is_ge(n, 224) && U32.is_le(n, 239), U32.is_ge(n, 240) && U32.is_le(n, 244), n, text)
    case False{}:
      extend(U32.is_ge(n, 128) && U32.is_le(n, 191), n, left, code, minimum, text)

def byte(n: U32, state: Decoder) -> Maybe<&2, Decoder>:
  match state:
    case Decoder{+left, code, minimum, text}:
      byte_start(U32.is_zero(left), n, left, code, minimum, text)

def finished(ok: Bool, text: String) -> Maybe<&2, String>:
  match ok:
    case True{}:
      Some{String.reverse(text)}
    case False{}:
      None{}

def finish(state: Decoder) -> Maybe<&2, String>:
  match state:
    case Decoder{left, code, minimum, text}:
      finished(U32.is_zero(left), text)

def decode_go(bytes: List<&2, U32>, state: Decoder) -> Maybe<&2, String>:
  match bytes:
    case Nil{}:
      finish(state)
    case Con{h, t}:
      do Maybe<&2, String>:
        next : Decoder <- byte(h, state)
        decode_go(t, next)

def decode(bytes: List<&2, U32>) -> Maybe<&2, String>:
  decode_go(bytes, Decoder{0, 0, 0, ""})
