import Base
import ./bytes.bend as Bytes

type Json is Data:
  JNull{}
  JBool{value: Bool}
  JString{value: String}
  JNumber{value: String}
  JArray{values: List<&2, Json>}
  JObject{fields: List<&2, Field>}

type Field is Data:
  Field{key: String, value: Json}

def space(+c: Char) -> Bool:
  Char.is_eq(c, ' ') || Char.is_eq(c, '\t') || Char.is_eq(c, '\n') || Char.is_eq(c, '\r')

def trim_step(white: Bool, h: Char, t: String, trimmed: Unit -> String) -> String:
  match white:
    case True{}:
      trimmed(Unit{})
    case False{}:
      SCon{h, t}

def trim(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, +t}:
      trim_step(space(h), h, t, u => trim(t))

def digit(+c: Char) -> Bool:
  +n = Char.to_u32(c)
  U32.is_ge(n, 48) && U32.is_le(n, 57)

def hex_value(n: U32, dec: Bool, lower: Bool, upper: Bool) -> Maybe<&2, U32>:
  match dec lower upper:
    case True{} _ _:
      Some{U32.sub(n, 48)}
    case False{} True{} _:
      Some{U32.sub(n, 87)}
    case False{} False{} True{}:
      Some{U32.sub(n, 55)}
    case False{} False{} False{}:
      None{}

def hex(c: Char) -> Maybe<&2, U32>:
  +n = Char.to_u32(c)
  hex_value(n, U32.is_ge(n, 48) && U32.is_le(n, 57), U32.is_ge(n, 97) && U32.is_le(n, 102), U32.is_ge(n, 65) && U32.is_le(n, 70))

type Scan is Data:
  Scan{mode: U32, digits: U32, code: U32, high: U32, reversed: String}

type ScanStep is Data:
  Keep{state: Scan}
  End{text: String}
  Bad{}

type Scanned is Data:
  Scanned{text: String, rest: String}

def scanned_char(ok: Bool, n: U32, text: String) -> ScanStep:
  match ok:
    case True{}:
      Keep{Scan{0, 0, 0, 0, SCon{Chr{n}, text}}}
    case False{}:
      Bad{}

def unicode_high(high: Bool, +n: U32, text: String) -> ScanStep:
  match high:
    case True{}:
      Keep{Scan{3, 0, 0, n, text}}
    case False{}:
      scanned_char(Bytes.scalar(n), n, text)

def unicode_done(low: Bool, +n: U32, high: U32, text: String) -> ScanStep:
  match low:
    case True{}:
      scanned_char(U32.is_ge(n, 56320) && U32.is_le(n, 57343), U32.add(65536, U32.add(U32.mul(U32.sub(high, 55296), 1024), U32.sub(n, 56320))), text)
    case False{}:
      unicode_high(U32.is_ge(n, 55296) && U32.is_le(n, 56319), n, text)

def unicode_more(done: Bool, mode: U32, digits: U32, +code: U32, high: U32, text: String) -> ScanStep:
  match done:
    case True{}:
      unicode_done(U32.is_eq(mode, 5), code, high, text)
    case False{}:
      Keep{Scan{mode, digits, code, high, text}}

def unicode_digit(value: Maybe<&2, U32>, mode: U32, +digits: U32, code: U32, high: U32, text: String) -> ScanStep:
  match value:
    case None{}:
      Bad{}
    case Some{n}:
      unicode_more(U32.is_eq(digits, 1), mode, U32.sub(digits, 1), U32.add(U32.mul(code, 16), n), high, text)

def expect_char(ok: Bool, state: Scan) -> ScanStep:
  match ok:
    case True{}:
      Keep{state}
    case False{}:
      Bad{}

def escaped(c: Char, text: String) -> ScanStep:
  match c:
    case '"':
      Keep{Scan{0, 0, 0, 0, SCon{'"', text}}}
    case '\\':
      Keep{Scan{0, 0, 0, 0, SCon{'\\', text}}}
    case '/':
      Keep{Scan{0, 0, 0, 0, SCon{'/', text}}}
    case 'n':
      Keep{Scan{0, 0, 0, 0, SCon{'\n', text}}}
    case 'r':
      Keep{Scan{0, 0, 0, 0, SCon{'\r', text}}}
    case 't':
      Keep{Scan{0, 0, 0, 0, SCon{'\t', text}}}
    case 'b':
      Keep{Scan{0, 0, 0, 0, SCon{'\u{8}', text}}}
    case 'f':
      Keep{Scan{0, 0, 0, 0, SCon{'\u{c}', text}}}
    case 'u':
      Keep{Scan{2, 4, 0, 0, text}}
    case other:
      Bad{}

def ordinary(c: Char, text: String) -> ScanStep:
  match c:
    case '"':
      End{String.reverse(text)}
    case '\\':
      Keep{Scan{1, 0, 0, 0, text}}
    case Chr{+n}:
      scanned_char(U32.is_ge(n, 32) && Bytes.scalar(n), n, text)

def scan_mode(mode: U32, digits: U32, code: U32, high: U32, text: String, c: Char) -> ScanStep:
  match mode:
    case 0:
      ordinary(c, text)
    case 1:
      escaped(c, text)
    case 2:
      unicode_digit(hex(c), 2, digits, code, high, text)
    case 3:
      expect_char(Char.is_eq(c, '\\'), Scan{4, 0, 0, high, text})
    case 4:
      expect_char(Char.is_eq(c, 'u'), Scan{5, 4, 0, high, text})
    case other:
      unicode_digit(hex(c), 5, digits, code, high, text)

def scan_step(c: Char, state: Scan) -> ScanStep:
  match state:
    case Scan{mode, digits, code, high, text}:
      scan_mode(mode, digits, code, high, text, c)

def scan_after(step: ScanStep, rest: String, next: Scan -> Maybe<&2, Scanned>) -> Maybe<&2, Scanned>:
  match step:
    case Bad{}:
      None{}
    case End{text}:
      Some{Scanned{text, rest}}
    case Keep{state}:
      next(state)

def scan(text: String, state: Scan) -> Maybe<&2, Scanned>:
  match text:
    case SNil{}:
      None{}
    case SCon{h, +t}:
      scan_after(scan_step(h, state), t, next => scan(t, next))

# JSON numbers have a strict grammar; keep their spelling to avoid precision loss.
def num_chars(+c: Char) -> Bool:
  digit(c) || Char.is_eq(c, '-') || Char.is_eq(c, '+') || Char.is_eq(c, '.') || Char.is_eq(c, 'e') || Char.is_eq(c, 'E')

def number_next(d: Bool, +z: Bool, minus: Bool, plus: Bool, dot: Bool, exp: Bool, state: U32) -> U32:
  match state:
    case 0:
      U32.add(U32.mul(Bool.to_u32(minus), 1), U32.add(U32.mul(Bool.to_u32(z), 2), U32.mul(Bool.to_u32(d && Bool.not(z)), 3)))
    case 1:
      U32.add(U32.mul(Bool.to_u32(z), 2), U32.mul(Bool.to_u32(d && Bool.not(z)), 3))
    case 2:
      U32.add(U32.mul(Bool.to_u32(dot), 4), U32.mul(Bool.to_u32(exp), 6))
    case 3:
      U32.add(U32.mul(Bool.to_u32(d), 3), U32.add(U32.mul(Bool.to_u32(dot), 4), U32.mul(Bool.to_u32(exp), 6)))
    case 4:
      U32.mul(Bool.to_u32(d), 5)
    case 5:
      U32.add(U32.mul(Bool.to_u32(d), 5), U32.mul(Bool.to_u32(exp), 6))
    case 6:
      U32.add(U32.mul(Bool.to_u32(minus || plus), 7), U32.mul(Bool.to_u32(d), 8))
    case other:
      U32.mul(Bool.to_u32(d), 8)

def number_check(ok: Bool, next: Unit -> Bool) -> Bool:
  match ok:
    case True{}:
      next(Unit{})
    case False{}:
      False{}

def number_valid(text: String, +state: U32) -> Bool:
  match text:
    case SNil{}:
      U32.is_eq(state, 2) || U32.is_eq(state, 3) || U32.is_eq(state, 5) || U32.is_eq(state, 8)
    case SCon{+h, t}:
      +s = number_next(digit(h), Char.is_eq(h, '0'), Char.is_eq(h, '-'), Char.is_eq(h, '+'), Char.is_eq(h, '.'), Char.is_eq(h, 'e') || Char.is_eq(h, 'E'), state)
      number_check(U32.is_ne(s, 0), u => number_valid(t, s))

def take_number_step(yes: Bool, h: Char, t: String, acc: String, next: String -> Scanned) -> Scanned:
  match yes:
    case True{}:
      next(SCon{h, acc})
    case False{}:
      Scanned{String.reverse(acc), SCon{h, t}}

def take_number(text: String, acc: String) -> Scanned:
  match text:
    case SNil{}:
      Scanned{String.reverse(acc), ""}
    case SCon{+h, +t}:
      take_number_step(num_chars(h), h, t, acc, a => take_number(t, a))

type Token is Data:
  Value{value: Json}
  OpenArray{}
  CloseArray{}
  OpenObject{}
  CloseObject{}
  Comma{}
  Colon{}

type Read is Data:
  Read{token: Token, rest: String}

def quoted(result: Scanned) -> Read:
  match result:
    case Scanned{text, rest}:
      Read{Value{JString{text}}, rest}

def number_result(ok: Bool, text: String, rest: String) -> Maybe<&2, Read>:
  match ok:
    case True{}:
      Some{Read{Value{JNumber{text}}, rest}}
    case False{}:
      None{}

def number_token(result: Scanned) -> Maybe<&2, Read>:
  match result:
    case Scanned{+text, rest}:
      number_result(number_valid(text, 0), text, rest)

def literal(ok: Bool, value: Json, rest: String) -> Maybe<&2, Read>:
  match ok:
    case True{}:
      Some{Read{Value{value}, rest}}
    case False{}:
      None{}

def token(text: String) -> Maybe<&2, Read>:
  match text:
    case SNil{}:
      None{}
    case SCon{'"', t}:
      do Maybe<&2, Read>:
        result : Scanned <- scan(t, Scan{0, 0, 0, 0, ""})
        return quoted(result)
    case SCon{'[', t}:
      Some{Read{OpenArray{}, t}}
    case SCon{']', t}:
      Some{Read{CloseArray{}, t}}
    case SCon{'{', t}:
      Some{Read{OpenObject{}, t}}
    case SCon{'}', t}:
      Some{Read{CloseObject{}, t}}
    case SCon{',', t}:
      Some{Read{Comma{}, t}}
    case SCon{':', t}:
      Some{Read{Colon{}, t}}
    case SCon{'t', +t}:
      literal(String.starts_with(t, "rue"), JBool{True{}}, String.drop(t, 3n))
    case SCon{'f', +t}:
      literal(String.starts_with(t, "alse"), JBool{False{}}, String.drop(t, 4n))
    case SCon{'n', +t}:
      literal(String.starts_with(t, "ull"), JNull{}, String.drop(t, 3n))
    case SCon{h, t}:
      number_token(take_number(SCon{h, t}, ""))

# Explicit parser stack: every transition consumes one token. No mutual or
# unbounded recursion, and nesting is limited independently of the input size.
type Frame is Data:
  Root{}
  Items{values: List<&2, Json>, empty: Bool}
  ItemComma{values: List<&2, Json>}
  Keys{fields: List<&2, Field>, empty: Bool}
  KeyColon{fields: List<&2, Field>, key: String}
  FieldValue{fields: List<&2, Field>, key: String}
  FieldComma{fields: List<&2, Field>}

type Parser is Data:
  Building{frames: List<&2, Frame>}
  Complete{value: Json}
  Invalid{}

def deliver(frames: List<&2, Frame>, value: Json) -> Parser:
  match frames:
    case Con{Root{}, Nil{}}:
      Complete{value}
    case Con{Items{values, empty}, rest}:
      Building{ItemComma{value <> values} <> rest}
    case Con{FieldValue{fields, key}, rest}:
      Building{FieldComma{Field{key, value} <> fields} <> rest}
    case other:
      Invalid{}

def expects(frames: List<&2, Frame>) -> Bool:
  match frames:
    case Con{Root{}, rest}:
      True{}
    case Con{Items{values, empty}, rest}:
      True{}
    case Con{FieldValue{fields, key}, rest}:
      True{}
    case other:
      False{}

def push(ok: Bool, frame: Frame, frames: List<&2, Frame>) -> Parser:
  match ok:
    case True{}:
      Building{frame <> frames}
    case False{}:
      Invalid{}

def close_array(empty: Bool, values: List<&2, Json>, rest: List<&2, Frame>) -> Parser:
  match empty:
    case True{}:
      deliver(rest, JArray{List.reverse(&2, Json, values)})
    case False{}:
      Invalid{}

def close_object(empty: Bool, fields: List<&2, Field>, rest: List<&2, Frame>) -> Parser:
  match empty:
    case True{}:
      deliver(rest, JObject{List.reverse(&2, Field, fields)})
    case False{}:
      Invalid{}

def consume(token: Token, +frames: List<&2, Frame>) -> Parser:
  match token frames:
    case Value{JString{key}} Con{Keys{fields, empty}, rest}:
      Building{KeyColon{fields, key} <> rest}
    case Value{value} frames:
      deliver(frames, value)
    case OpenArray{} frames:
      push(expects(frames) && Nat.is_le(List.length(&2, Frame, frames), 64n), Items{Nil{}, True{}}, frames)
    case OpenObject{} frames:
      push(expects(frames) && Nat.is_le(List.length(&2, Frame, frames), 64n), Keys{Nil{}, True{}}, frames)
    case CloseArray{} Con{Items{values, empty}, rest}:
      close_array(empty, values, rest)
    case CloseArray{} Con{ItemComma{values}, rest}:
      deliver(rest, JArray{List.reverse(&2, Json, values)})
    case CloseObject{} Con{Keys{fields, empty}, rest}:
      close_object(empty, fields, rest)
    case CloseObject{} Con{FieldComma{fields}, rest}:
      deliver(rest, JObject{List.reverse(&2, Field, fields)})
    case Comma{} Con{ItemComma{values}, rest}:
      Building{Items{values, False{}} <> rest}
    case Comma{} Con{FieldComma{fields}, rest}:
      Building{Keys{fields, False{}} <> rest}
    case Colon{} Con{KeyColon{fields, key}, rest}:
      Building{FieldValue{fields, key} <> rest}
    case other frames:
      Invalid{}

def advance(state: Parser, token: Token) -> Parser:
  match state:
    case Building{frames}:
      consume(token, frames)
    case other:
      Invalid{}

def finish(state: Parser) -> Maybe<&2, Json>:
  match state:
    case Complete{value}:
      Some{value}
    case other:
      None{}

def parse_read(read: Read, state: Parser, next: String -> Parser -> Maybe<&2, Json>) -> Maybe<&2, Json>:
  match read:
    case Read{token, rest}:
      next(rest, advance(state, token))

def parse_text(text: String, state: Parser, next: Read -> Parser -> Maybe<&2, Json>) -> Maybe<&2, Json>:
  match text:
    case SNil{}:
      finish(state)
    case SCon{h, t}:
      do Maybe<&2, Json>:
        read : Read <- token(SCon{h, t})
        next(read, state)

def parse_go(fuel: Nat, text: String, state: Parser) -> Maybe<&2, Json>:
  match fuel:
    case 0n:
      None{}
    case 1n+p:
      parse_text(trim(text), state, read => state2 =>
        parse_read(read, state2, rest => next => parse_go(p, rest, next)))

def parse(+text: String) -> Maybe<&2, Json>:
  parse_go(1n+String.length(text), text, Building{[Root{}]})

# Request encoding: controls are escaped, Unicode stays Unicode until UTF-8.
def hex_char(n: U32, small: Bool) -> Char:
  match small:
    case True{}:
      Chr{U32.add(n, 48)}
    case False{}:
      Chr{U32.add(n, 87)}

def escaped_control(control: Bool, +n: U32) -> String:
  match control:
    case False{}:
      SCon{Chr{n}, ""}
    case True{}:
      +lo = U32.mod(n, 16)
      "\\u00" ++ SCon{Chr{U32.add(48, U32.div(n, 16))}, SCon{hex_char(lo, U32.is_lt(lo, 10)), ""}}

def escape_char(c: Char) -> String:
  match c:
    case '"':
      "\\\""
    case '\\':
      "\\\\"
    case Chr{+n}:
      escaped_control(U32.is_lt(n, 32), n)

def escape(text: String) -> String:
  match text:
    case SNil{}:
      ""
    case SCon{h, t}:
      escape_char(h) ++ escape(t)

def request(text: String) -> String:
  "{\"role\":\"user\",\"content\":\"" ++ escape(text) ++ "\"}"
