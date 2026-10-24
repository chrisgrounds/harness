import Base
import ./src/protocol.bend as Protocol
import ./src/session.bend as Client
import ./src/bytes.bend as Bytes
import ./src/json.bend as Json

# Client contract over the pure state machine used by the executable.
# PROOF.bend discharges these obligations with Bend's independent kernel.
# The driver interprets Display/Send/Stop using Base I/O, which remains trusted.
# Visibility means emitting sanitized output for the current turn, not replaying
# older history. Send payloads are UTF-8 JSON followed by an LF frame delimiter.

# 1. LAW: must show the tool calls made by the harness.
# Each call contributes its name and arguments, in list order. In particular,
# tool-only assistant messages must not replace their calls with a placeholder.
law tool_calls_are_visible:
  for name: String
  for arguments: String
  for rest: List<&2, Protocol.Call>
  for color: Bool
  {Protocol.render_calls(Protocol.Call{name, arguments} <> rest, color) ==
    Protocol.line(color, "33", "tool  ", name ++ " " ++ arguments) ++
      Protocol.render_calls(rest, color) : String}

law tool_only_messages_show_calls:
  for calls: List<&2, Protocol.Call>
  for speed: Maybe<&2, String>
  for color: Bool
  {Protocol.render_message(Protocol.Message{Protocol.Agent{}, None{}, calls, speed}, color) ==
    Protocol.render_empty(calls, color) ++ Protocol.render_calls(calls, color) ++
      Protocol.render_speed(speed, color) : String}

# 2. LAW: users must be able to enter prompts.
# Ordinary input is encoded without changing the original text and checked
# against the byte limit before a single Send action can be produced.
law prompts_can_be_submitted:
  for text: String
  for color: Bool
  {Client.command(False{}, False{}, text, color) ==
    Client.accepted(Client.fits(Client.encoded(text)), Client.encoded(text)) : Client.Decision}

law accepted_prompts_are_sent_once:
  for bytes: List<&2, U32>
  {Client.accepted(True{}, bytes) ==
    Client.Decision{Client.Waiting{}, [Client.Send{List.append(&2, U32, bytes, [10])}]} : Client.Decision}

# The size decision is about encoded UTF-8 JSON bytes, excluding the LF frame.
law prompt_encoding_and_size:
  for text: String
  {Client.encoded(text) == Bytes.encode(Json.request(text)) : List<&2, U32>} &
  {Client.fits(Client.encoded(text)) ==
    Nat.is_le(List.length(&2, U32, Bytes.encode(Json.request(text))), 1024n) : Bool}

# 3. LAW: every tool result is visible, including missing-content placeholders.
law tool_results_are_visible:
  for text: String
  for calls: List<&2, Protocol.Call>
  for speed: Maybe<&2, String>
  for color: Bool
  {Protocol.render_message(Protocol.Message{Protocol.Tool{}, Some{text}, calls, speed}, color) ==
    Protocol.line(color, "33", "tool  ", text) : String}

law missing_tool_results_have_a_placeholder:
  for calls: List<&2, Protocol.Call>
  for speed: Maybe<&2, String>
  for color: Bool
  {Protocol.render_message(Protocol.Message{Protocol.Tool{}, None{}, calls, speed}, color) ==
    Protocol.line(color, "33", "tool  ", "(no tool output)") : String}

# 4. LAW: history is not repeated.
# Appending a user boundary and older history cannot change the selected turn.
# This also holds if newer already contains a more recent user boundary.
law older_history_is_excluded:
  for newer: List<&2, Protocol.Message>
  for text: Maybe<&2, String>
  for calls: List<&2, Protocol.Call>
  for speed: Maybe<&2, String>
  for older: List<&2, Protocol.Message>
  {Protocol.current(List.append(&2, Protocol.Message, newer,
      Protocol.Message{Protocol.User{}, text, calls, speed} <> older)) ==
    Protocol.current(newer) : List<&2, Protocol.Message>}

# 5. LAW: conversation order is preserved.
# A valid selected turn is printed oldest-first, reversing the server's order.
law turns_are_displayed_chronologically:
  for messages: List<&2, Protocol.Message>
  for color: Bool
  {Client.display_turn(messages, color) ==
    Protocol.render(List.reverse(&2, Protocol.Message, messages), color) ++ "\n" : String}

# The production state machine actually selects and renders the proven turn.
law received_turns_use_the_renderer:
  for messages: List<&2, Protocol.Message>
  for color: Bool
  {Client.step(Client.Waiting{}, Client.Received{messages}, color) ==
    Client.reply(Protocol.has_agent(Protocol.current(messages)),
      Protocol.current(messages), color) : Client.Decision}

# 6. LAW: no assistant message is silently dropped.
# The renderer emits text, calls, or a placeholder, even with absent/empty text.
law assistant_messages_are_not_silent:
  for text: Maybe<&2, String>
  for calls: List<&2, Protocol.Call>
  for speed: Maybe<&2, String>
  for color: Bool
  {String.is_empty(Protocol.render_message(
    Protocol.Message{Protocol.Agent{}, text, calls, speed}, color)) == False{} : Bool}

# 7. LAW: oversized prompts are never sent and the session stays usable.
# The premise refers to the actual byte length, not an external encoder verdict.
law oversized_prompts_are_rejected_without_sending:
  for bytes: List<&2, U32>
  for too_big: {Nat.is_le(List.length(&2, U32, bytes), 1024n) == False{} : Bool}
  {Client.accepted(Client.fits(bytes), bytes) ==
    Client.Decision{Client.Idle{}, [Client.Display{Client.reject_message()}]} : Client.Decision}

# 8. LAW: a failed exchange closes the session without retrying the request.
law failed_exchanges_are_not_retried:
  for message: String
  for color: Bool
  {Client.step(Client.Waiting{}, Client.Failed{message}, color) ==
    Client.Decision{Client.Stopped{}, [Client.Stop{1, message}]} : Client.Decision}

# 9. LAW: quit and EOF close without sending, including recognition of /quit.
law quit_does_not_send:
  for color: Bool
  {Client.step(Client.Idle{}, Client.Entered{"/quit"}, color) ==
    Client.Decision{Client.Stopped{}, [Client.Stop{0, ""}]} : Client.Decision}

law end_of_input_does_not_send:
  for color: Bool
  {Client.step(Client.Idle{}, Client.EndOfInput{}, color) ==
    Client.Decision{Client.Stopped{}, [Client.Stop{0, ""}]} : Client.Decision}

law exhausted_budget_closes:
  for color: Bool
  {Client.step(Client.Waiting{}, Client.Exhausted{}, color) ==
    Client.Decision{Client.Stopped{}, [Client.Stop{1, "session event budget exhausted"}]} : Client.Decision}

# 10. LAW: server text cannot inject terminal control characters.
# Test the plain output path: color mode intentionally adds trusted ANSI codes.
# safe_char permits newline/tab, printable ASCII, and non-C1 Unicode characters.
# Labels and ANSI codes are client-owned constants, never server-provided text.
law server_text_is_sanitized:
  for text: String
  {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(Protocol.safe(text))) ==
    True{} : Bool}

law plain_display_contains_no_unsafe_controls:
  for text: String
  {List.all(~&2, ~Char, ~Protocol.safe_char,
    String.to_list(Protocol.line(False{}, "", "agent ", text))) == True{} : Bool}

# 11. LAW: startup begins with a red BEND CLIENT banner before connection status.
# main writes this exact startup string before opening stdin or connecting.
law startup_banner_is_first:
  for port: U32
  {Client.startup(port) ==
    "\u{1b}[1;31mBEND CLIENT\u{1b}[0m\nConnecting to 127.0.0.1:" ++ U32.show(port) ++ "...\n" : String}

# 12. LAW: the user prompt is visible whenever the driver becomes ready.
# main interprets this action list on connection and after each transition.
law idle_shows_user_prompt:
  for color: Bool
  {Client.ready(Client.Idle{}, color) ==
    [Client.Display{Protocol.paint(color, "1;36", "user  ") ++ "> "}] : List<&2, Client.Action>}

law waiting_does_not_prompt_for_input:
  for color: Bool
  {Client.ready(Client.Waiting{}, color) == Nil{} : List<&2, Client.Action>}

law stopped_does_not_prompt_for_input:
  for color: Bool
  {Client.ready(Client.Stopped{}, color) == Nil{} : List<&2, Client.Action>}

# The production renderers retain role labels in both plain and color modes.
# Color wraps only the label; the body is still sanitized and line-terminated.
# Terminal line discipline already echoes typing after user_prompt. Submission
# must not emit Display or another prompt: exactly one Send enters Waiting.
# The executable always enables color, so the command uses True{} directly.
law submitted_prompts_are_not_echoed:
  for text: String
  for within_limit: {Client.fits(Client.encoded(text)) == True{} : Bool}
  {Client.command(False{}, False{}, text, True{}) ==
    Client.Decision{Client.Waiting{},
      [Client.Send{List.append(&2, U32, Client.encoded(text), [10])}]} : Client.Decision}

law agent_messages_have_a_visible_label:
  for color: Bool
  for text: String
  {Protocol.agent_line(color, text) ==
    Protocol.paint(color, "1;32", "agent ") ++ "> " ++ Protocol.format_body(text) ++ "\n" : String}

law tool_messages_have_a_visible_label:
  for color: Bool
  for text: String
  {Protocol.tool_line(color, text) ==
    Protocol.paint(color, "33", "tool  ") ++ "> " ++ Protocol.format_body(text) ++ "\n" : String}
