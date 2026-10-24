import Base
import ./json.bend as J
import ./protocol.bend as P

def lookup_step(same: Bool, value: J.Json, next: Unit -> Maybe<&2, J.Json>) -> Maybe<&2, J.Json>:
  match same:
    case True{}:
      Some{value}
    case False{}:
      next(Unit{})

def lookup(fields: List<&2, J.Field>, +name: String) -> Maybe<&2, J.Json>:
  match fields:
    case Nil{}:
      None{}
    case Con{J.Field{key, value}, rest}:
      lookup_step(String.eq(key, name), value, u => lookup(rest, name))

def field(object: J.Json, name: String) -> Maybe<&2, J.Json>:
  match object:
    case J.JObject{fields}:
      lookup(fields, name)
    case other:
      None{}

def string(value: J.Json) -> Maybe<&2, String>:
  match value:
    case J.JString{text}:
      Some{text}
    case other:
      None{}

def optional(value: Maybe<&2, J.Json>) -> Maybe<&2, Maybe<&2, String>>:
  match value:
    case None{}:
      Some{None{}}
    case Some{J.JNull{}}:
      Some{None{}}
    case Some{J.JString{text}}:
      Some{Some{text}}
    case other:
      None{}

def role_value(user: Bool, agent: Bool, tool: Bool, system: Bool) -> Maybe<&2, P.Role>:
  match user agent tool system:
    case True{} _ _ _:
      Some{P.User{}}
    case False{} True{} _ _:
      Some{P.Agent{}}
    case False{} False{} True{} _:
      Some{P.Tool{}}
    case False{} False{} False{} True{}:
      Some{P.System{}}
    case other other2 other3 other4:
      None{}

def role(+text: String) -> Maybe<&2, P.Role>:
  role_value(String.eq(text, "user"), String.eq(text, "assistant"), String.eq(text, "tool"), String.eq(text, "system"))

def call(value: J.Json) -> Maybe<&2, P.Call>:
  do Maybe<&2, P.Call>:
    +fn : J.Json <- field(value, "function")
    name_value : J.Json <- field(fn, "name")
    name : String <- string(name_value)
    args_value : J.Json <- field(fn, "arguments")
    args : String <- string(args_value)
    return P.Call{name, args}

def calls(values: List<&2, J.Json>) -> Maybe<&2, List<&2, P.Call>>:
  match values:
    case Nil{}:
      Some{Nil{}}
    case Con{h, t}:
      do Maybe<&2, List<&2, P.Call>>:
        head : P.Call <- call(h)
        rest : List<&2, P.Call> <- calls(t)
        return head <> rest

def optional_calls(value: Maybe<&2, J.Json>) -> Maybe<&2, List<&2, P.Call>>:
  match value:
    case None{}:
      Some{Nil{}}
    case Some{J.JNull{}}:
      Some{Nil{}}
    case Some{J.JArray{values}}:
      calls(values)
    case other:
      None{}

def speed_format(small: Bool, value: F32, original: String) -> String:
  match small:
    case True{}:
      +tenths = F32.to_u32(F32.add(F32.mul(value, 10.0), 0.5))
      U32.show(U32.div(tenths, 10)) ++ "." ++ U32.show(U32.mod(tenths, 10))
    case False{}:
      original

def speed(value: Maybe<&2, J.Json>) -> Maybe<&2, Maybe<&2, String>>:
  match value:
    case None{}:
      Some{None{}}
    case Some{J.JNull{}}:
      Some{None{}}
    case Some{J.JNumber{+text}}:
      do Maybe<&2, Maybe<&2, String>>:
        +number : F32 <- F32.read(text)
        return Some{speed_format(F32.is_ge(number, 0.0) && F32.is_lt(number, 400000000.0), number, text)}
    case other:
      None{}

def message(+value: J.Json) -> Maybe<&2, P.Message>:
  do Maybe<&2, P.Message>:
    r : J.Json <- field(value, "role")
    name : String <- string(r)
    role : P.Role <- role(name)
    content : Maybe<&2, String> <- optional(field(value, "content"))
    requests : List<&2, P.Call> <- optional_calls(field(value, "tool_calls"))
    rate : Maybe<&2, String> <- speed(field(value, "tokens_per_second"))
    return P.Message{role, content, requests, rate}

def messages(values: List<&2, J.Json>) -> Maybe<&2, List<&2, P.Message>>:
  match values:
    case Nil{}:
      Some{Nil{}}
    case Con{h, t}:
      do Maybe<&2, List<&2, P.Message>>:
        head : P.Message <- message(h)
        rest : List<&2, P.Message> <- messages(t)
        return head <> rest

def array(value: J.Json) -> Maybe<&2, List<&2, P.Message>>:
  match value:
    case J.JArray{values}:
      messages(values)
    case other:
      None{}

def decode(text: String) -> Maybe<&2, List<&2, P.Message>>:
  do Maybe<&2, List<&2, P.Message>>:
    root : J.Json <- J.parse(text)
    values : J.Json <- field(root, "messages")
    array(values)
