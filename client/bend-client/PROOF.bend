# Proofs of the pure client contract, checked with bend PROOF.bend --verdict.
import Base
import ./src/protocol.bend as Protocol
import ./src/session.bend as Client
import ./LAWS.bend as Laws

# {==} is an equality proof by computation (reflexivity), not a hole.
# For these laws, unfolding the named definitions makes both sides identical
# for all their arguments. No case split or induction hypothesis is needed.
# An unfinished proof would use ?TODO instead.

def Laws.tool_calls_are_visible(name, arguments, rest, color):
  # render_calls on Con emits this call's line followed by render_calls(rest).
  {==}

def Laws.tool_only_messages_show_calls(calls, speed, color):
  # render_message selects Agent; render_text(None) reduces to render_empty.
  # The calls and optional speed remain appended exactly as the law requires.
  {==}

def Laws.prompts_can_be_submitted(text, color):
  # command(False, False) reduces to accepted with the encoded original text,
  # and its byte-limit decision, exactly matching the law.
  {==}

def Laws.accepted_prompts_are_sent_once(bytes):
  # accepted(True) constructs Waiting with one Send and no Display. Its
  # payload is bytes followed by LF. Both sides construct the same Decision.
  {==}

def Laws.tool_results_are_visible(text, calls, speed, color):
  # render_message on Tool/Some selects the tool line containing text.
  {==}

def Laws.missing_tool_results_have_a_placeholder(calls, speed, color):
  # render_message on Tool/None selects the literal "(no tool output)" line.
  {==}

def Laws.older_history_is_excluded(newer, text, calls, speed, older):
  match newer:
    case Nil{}:
      {==}
    case Con{Protocol.Message{Protocol.User{}, body, requests, rate}, rest}:
      {==}
    case Con{Protocol.Message{Protocol.Agent{}, body, requests, rate}, rest}:
      %Laws.older_history_is_excluded(rest, text, calls, speed, older) :
        {Protocol.Message{Protocol.Agent{}, body, requests, rate} <>
          Protocol.current(List.append(&2, Protocol.Message, rest,
            Protocol.Message{Protocol.User{}, text, calls, speed} <> older)) ==
          Protocol.Message{Protocol.Agent{}, body, requests, rate} <> _ : List<&2, Protocol.Message>}
      {==}
    case Con{Protocol.Message{Protocol.Tool{}, body, requests, rate}, rest}:
      %Laws.older_history_is_excluded(rest, text, calls, speed, older) :
        {Protocol.Message{Protocol.Tool{}, body, requests, rate} <>
          Protocol.current(List.append(&2, Protocol.Message, rest,
            Protocol.Message{Protocol.User{}, text, calls, speed} <> older)) ==
          Protocol.Message{Protocol.Tool{}, body, requests, rate} <> _ : List<&2, Protocol.Message>}
      {==}
    case Con{Protocol.Message{Protocol.System{}, body, requests, rate}, rest}:
      %Laws.older_history_is_excluded(rest, text, calls, speed, older) :
        {Protocol.Message{Protocol.System{}, body, requests, rate} <>
          Protocol.current(List.append(&2, Protocol.Message, rest,
            Protocol.Message{Protocol.User{}, text, calls, speed} <> older)) ==
          Protocol.Message{Protocol.System{}, body, requests, rate} <> _ : List<&2, Protocol.Message>}
      {==}

def Laws.turns_are_displayed_chronologically(messages, color):
  {==}

def Laws.assistant_messages_are_not_silent(text, calls, speed, color):
  match text calls color:
    case None{} Nil{} False{}:
      {==}
    case None{} Nil{} True{}:
      {==}
    case None{} Con{Protocol.Call{name, arguments}, rest} False{}:
      {==}
    case None{} Con{Protocol.Call{name, arguments}, rest} True{}:
      {==}
    case Some{SNil{}} Nil{} False{}:
      {==}
    case Some{SNil{}} Nil{} True{}:
      {==}
    case Some{SNil{}} Con{Protocol.Call{name, arguments}, rest} False{}:
      {==}
    case Some{SNil{}} Con{Protocol.Call{name, arguments}, rest} True{}:
      {==}
    case Some{SCon{h, t}} calls False{}:
      {==}
    case Some{SCon{h, t}} calls True{}:
      {==}

def Laws.oversized_prompts_are_rejected_without_sending(bytes, too_big):
  %too_big : {Client.accepted(Client.fits(bytes), bytes) == Client.accepted(_, bytes) : Client.Decision}
  {==}

def Laws.failed_exchanges_are_not_retried(message, color):
  {==}

def Laws.quit_does_not_send(color):
  {==}

def Laws.end_of_input_does_not_send(color):
  {==}

# Filtering retains only characters satisfying the display predicate.
law filtered_step:
  for h: Char
  for rest: List<&2, Char>
  for keep: Bool
  for hit: {keep == Protocol.safe_char(h) : Bool}
  for good: {List.all(~&2, ~Char, ~Protocol.safe_char, rest) == True{} : Bool}
  {List.all(~&2, ~Char, ~Protocol.safe_char, List.filter.put(Char, h, rest, keep)) == True{} : Bool}

def filtered_step(h, rest, keep, hit, good):
  match keep:
    case False{}:
      good
    case True{}:
      %hit : {List.all.if(u => List.all(~&2, ~Char, ~Protocol.safe_char, rest), _) == True{} : Bool}
      good

law filtered_safe:
  for xs: List<&2, Char>
  {List.all(~&2, ~Char, ~Protocol.safe_char,
    List.filter(~Char, ~Protocol.safe_char, xs)) == True{} : Bool}

def filtered_safe(xs):
  match xs:
    case Nil{}:
      {==}
    case Con{+h, +t}:
      filtered_step(h, List.filter(~Char, ~Protocol.safe_char, t), Protocol.safe_char(h), {==}, filtered_safe(t))

law list_roundtrip:
  for xs: List<&2, Char>
  {String.to_list(String.from_list(xs)) == xs : List<&2, Char>}

def list_roundtrip(xs):
  match xs:
    case Nil{}:
      {==}
    case Con{h, t}:
      %list_roundtrip(t) : {h <> String.to_list(String.from_list(t)) == h <> _ : List<&2, Char>}
      {==}

def Laws.server_text_is_sanitized(text):
  +xs = String.to_list(text)
  %filtered_safe(xs) :
    {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(String.from_list(
      List.filter(~Char, ~Protocol.safe_char, xs)))) == _ : Bool}
  %list_roundtrip(List.filter(~Char, ~Protocol.safe_char, xs)) :
    {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(String.from_list(
      List.filter(~Char, ~Protocol.safe_char, xs)))) ==
      List.all(~&2, ~Char, ~Protocol.safe_char, _) : Bool}
  {==}

# Formatting only inserts literal newline/space characters.
law format_step_safe:
  for h: Char
  for rest: String
  for allowed: Bool
  for newline: Bool
  for hit: {allowed == Protocol.safe_char(h) : Bool}
  for good: {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(rest)) == True{} : Bool}
  {List.all(~&2, ~Char, ~Protocol.safe_char,
    String.to_list(Protocol.format_step(h, rest, allowed, newline))) == True{} : Bool}

def format_step_safe(h, rest, allowed, newline, hit, good):
  match allowed newline:
    case False{} _:
      good
    case True{} True{}:
      good
    case True{} False{}:
      %hit : {List.all.if(u => List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(rest)), _) == True{} : Bool}
      good

law formatted_safe:
  for text: String
  {List.all(~&2, ~Char, ~Protocol.safe_char,
    String.to_list(Protocol.format_body(text))) == True{} : Bool}

def formatted_safe(text):
  match text:
    case SNil{}:
      {==}
    case SCon{+h, +t}:
      format_step_safe(h, Protocol.format_body(t), Protocol.safe_char(h), Char.is_eq(h, '\n'), {==}, formatted_safe(t))

# Appending a safe suffix to a safe string preserves the predicate.
law append_safe_step:
  for h: Char
  for t: String
  for suffix: String
  for allowed: Bool
  for hit: {allowed == Protocol.safe_char(h) : Bool}
  for good: {List.all.if(u => List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(t)), allowed) == True{} : Bool}
  for tail_good: {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(t)) == True{} : Bool} ->
    {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(t ++ suffix)) == True{} : Bool}
  {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(SCon{h, t} ++ suffix)) == True{} : Bool}

def append_safe_step(h, t, suffix, allowed, hit, good, tail_good):
  match allowed:
    case False{}:
      %good : {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(SCon{h, t} ++ suffix)) == _ : Bool}
      %hit : {List.all.if(u => List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(t ++ suffix)), _) == False{} : Bool}
      {==}
    case True{}:
      %hit : {List.all.if(u => List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(t ++ suffix)), _) == True{} : Bool}
      tail_good(good)

law append_safe:
  for text: String
  for +suffix: String
  for good: {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(text)) == True{} : Bool}
  for suffix_good: {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(suffix)) == True{} : Bool}
  {List.all(~&2, ~Char, ~Protocol.safe_char, String.to_list(text ++ suffix)) == True{} : Bool}

def append_safe(text, suffix, good, suffix_good):
  match text:
    case SNil{}:
      suffix_good
    case SCon{+h, +t}:
      append_safe_step(h, t, suffix, Protocol.safe_char(h), {==}, good,
        proof => append_safe(t, suffix, proof, suffix_good))

def Laws.plain_display_contains_no_unsafe_controls(text):
  +copy = text
  append_safe(Protocol.format_body(copy), "\n", formatted_safe(copy), {==})


def Laws.prompt_encoding_and_size(text):
  ({==}, {==})

def Laws.received_turns_use_the_renderer(messages, color):
  {==}

def Laws.exhausted_budget_closes(color):
  {==}

# These equalities unfold the display functions used by the native driver.
def Laws.startup_banner_is_first(port):
  {==}

def Laws.idle_shows_user_prompt(color):
  {==}

def Laws.waiting_does_not_prompt_for_input(color):
  {==}

def Laws.stopped_does_not_prompt_for_input(color):
  {==}

def Laws.submitted_prompts_are_not_echoed(text, within_limit):
  %within_limit :
    {Client.accepted(Client.fits(Client.encoded(text)), Client.encoded(text)) ==
      Client.accepted(_, Client.encoded(text)) : Client.Decision}
  {==}

def Laws.agent_messages_have_a_visible_label(color, text):
  {==}

def Laws.tool_messages_have_a_visible_label(color, text):
  {==}
