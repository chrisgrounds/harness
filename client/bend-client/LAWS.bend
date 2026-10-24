import Base
import ./src/protocol.bend as Protocol
import ./src/main.bend as Client

# Client contract. These are OPEN obligations, not completed proofs.
# A future PROOF.bend should import this file and discharge each claim.
#
# Scope: the client receives newest-first history from Harness. Visibility
# means producing terminal output, with control characters sanitized; it
# does not mean showing earlier turns again or preserving unsafe escape codes.
# I/O equalities specify the actions issued, not successful OS delivery.
# Foreign encoding/socket effects and the unsafe session loop remain trusted
# boundaries until they are replaced with, or connected to, a proved model.

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
# False/False is command's ordinary, nonblank, non-quit branch. The original
# text is passed unchanged to the encoder exactly once, and the session resumes
# after send returns. The following law specifies a single exchange for every
# payload accepted by the encoder. JSON content preservation remains a contract
# of the foreign encoder, exercised by the socket integration tests.
law prompts_can_be_submitted:
  for text: String
  for socket: Socket
  for interactive: Bool
  for color: Bool
  {Client.command(False{}, False{}, text, socket, interactive, color) ==
    (do IO<Unit>:
      Client.echo(interactive, color, text)
      payload : Maybe<&2, String> <- Protocol.encode(text)
      next : Socket <- Client.send(payload, socket, color)
      Client.loop(next, interactive, color)
    ) : IO(Unit)}

law accepted_prompts_are_sent_once:
  for payload: String
  for socket: Socket
  for color: Bool
  {Client.send(Some{payload}, socket, color) ==
    (do IO<Socket>:
      pair : Socket & Result<&1, &1, U32 & String, List<&2, Protocol.Message>> <- Protocol.exchange(socket, payload)
      Client.unpack_reply(pair, color)
    ) : IO(Socket)}

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
  {Client.show_turn(True{}, messages, color) ==
    IO.print(Protocol.render(List.reverse(&2, Protocol.Message, messages), color)) : IO(Unit)}

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
# None is the encoder's rejection of a request exceeding 1024 JSON bytes.
# This must report the rejection and return the same open socket, with no
# exchange. Measuring UTF-8 JSON bytes is a foreign-encoder obligation; this
# law must not be read as a proof of that C implementation's size calculation.
law oversized_prompts_are_rejected_without_sending:
  for socket: Socket
  for color: Bool
  {Client.send(None{}, socket, color) ==
    (do IO<Socket>:
      IO.print("Message too long for the server (1024 encoded bytes maximum).")
      return socket
    ) : IO(Socket)}

# 8. LAW: failures never trigger automatic resubmission.
# An exchange failure closes the connection and exits; it cannot issue another
# exchange, even if the server may already have processed the original request.
law failed_exchanges_are_not_retried:
  for code: U32
  for message: String
  for socket: Socket
  for color: Bool
  {Client.reply(Fail{(code, message)}, socket, color) ==
    (do IO<Socket>:
      Socket.close(socket)
      IO.die(Socket, 1, "bend-client: " ++ message)
    ) : IO(Socket)}

# 9. LAW: quit and end-of-input close the session without sending a prompt.
# State this at the input boundary so it covers command recognition as well.
law quit_does_not_send:
  for socket: Socket
  for interactive: Bool
  for color: Bool
  {Client.accept(Done{Some{"/quit"}}, socket, interactive, color) ==
    Socket.close(socket) : IO(Unit)}

law end_of_input_does_not_send:
  for socket: Socket
  for interactive: Bool
  for color: Bool
  {Client.accept(Done{None{}}, socket, interactive, color) ==
    Socket.close(socket) : IO(Unit)}

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
