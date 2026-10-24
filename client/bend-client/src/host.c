// Bend 2.0.39 native effects. Protocol decisions and rendering live in Bend;
// this boundary owns POSIX I/O, JSON encoding, and typed JSON decoding.
#include <sys/un.h>
#include <json-c/json.h>
#include <signal.h>

static Term host_none(void) { return term_pak(CID(None), 0); }
static Term host_text(Env e, const char *s) { return io_str(e, s, strlen(s)); }
static Term host_string(Env e, json_object *v) {
  return io_str(e, json_object_get_string(v), json_object_get_string_len(v));
}

static Term host_connect(Env e, Term *f, IoWork *w) {
  u64 len;
  char *path = io_cstr(e, f[0], &len);
  struct sockaddr_un address = {.sun_family = AF_UNIX};
  int fd = -1, error = 0;
  if (len == 0 || len >= sizeof(address.sun_path) || memchr(path, 0, len)) {
    error = EINVAL;
  } else {
    memcpy(address.sun_path, path, len + 1);
    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0 || connect(fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
      error = errno;
      if (fd >= 0) close(fd);
    }
  }
  free(path);
  // A closed peer must produce an error, not terminate during send().
  signal(SIGPIPE, SIG_IGN);
  return error ? io_fail(e, error, "bend-client: could not connect to the Unix socket")
               : io_done(e, io_hand(fd));
}

static void host_input_call(IoWork *w) {
  char *line = NULL;
  size_t cap = 0;
  errno = 0;
  ssize_t len = getline(&line, &cap, stdin);
  w->data = line;
  w->made = len;
  w->code = len < 0 && !feof(stdin) ? (errno ? errno : EIO) : 0;
}
static Term host_input_pack(Env e, IoWork *w) {
  Term result;
  if (w->code) {
    result = io_fail(e, w->code, NULL);
  } else if (w->made < 0) {
    result = io_done(e, host_none());
  } else {
    size_t len = w->made;
    char *line = w->data;
    if (len && line[len - 1] == '\n') len--;
    if (len && line[len - 1] == '\r') len--;
    result = io_done(e, io_box(e, CID(Some), io_str(e, line, len)));
  }
  free(w->data);
  return result;
}
static Term host_input(Env e, Term *f, IoWork *w) {
  fflush(stdout);
  return io_work(w, host_input_call, host_input_pack);
}
static Term host_interactive(Env e, Term *f, IoWork *w) {
  return term_pak(isatty(STDIN_FILENO) && isatty(STDOUT_FILENO) ? CID(True) : CID(False), 0);
}
static Term host_color(Env e, Term *f, IoWork *w) {
  const char *term = getenv("TERM");
  bool color = isatty(STDOUT_FILENO) && !getenv("NO_COLOR") && (!term || strcmp(term, "dumb"));
  return term_pak(color ? CID(True) : CID(False), 0);
}
static Term host_encode(Env e, Term *f, IoWork *w) {
  u64 len;
  char *text = io_cstr(e, f[0], &len);
  if (len > 1024) { free(text); return host_none(); }
  json_object *object = json_object_new_object();
  json_object_object_add(object, "role", json_object_new_string("user"));
  json_object_object_add(object, "content", json_object_new_string_len(text, len));
  const char *encoded = json_object_to_json_string_ext(object, JSON_C_TO_STRING_PLAIN);
  Term result = strlen(encoded) > 1024 ? host_none()
    : io_box(e, CID(Some), host_text(e, encoded));
  json_object_put(object);
  free(text);
  return result;
}

static json_object *host_field(json_object *v, const char *name) {
  json_object *field = NULL;
  json_object_object_get_ex(v, name, &field);
  return field;
}
static bool host_is(json_object *v, enum json_type type) {
  return v && json_object_is_type(v, type);
}
static bool host_optional_string(json_object *v) {
  return !v || host_is(v, json_type_string);
}
static bool host_equal(json_object *v, const char *s) {
  return host_is(v, json_type_string) && json_object_get_string_len(v) == strlen(s)
    && !strcmp(json_object_get_string(v), s);
}

// Validate the entire structure before allocating Bend terms, so invalid
// replies cannot leave partially constructed values in the runtime heap.
static bool host_valid(json_object *root) {
  if (!host_is(root, json_type_object)) return false;
  json_object *messages = host_field(root, "messages");
  if (!host_is(messages, json_type_array)) return false;
  for (size_t i = 0; i < json_object_array_length(messages); i++) {
    json_object *m = json_object_array_get_idx(messages, i);
    if (!host_is(m, json_type_object)) return false;
    json_object *role = host_field(m, "role");
    if (!host_equal(role, "user") && !host_equal(role, "assistant")
        && !host_equal(role, "tool") && !host_equal(role, "system")) return false;
    if (!host_optional_string(host_field(m, "content"))) return false;
    json_object *speed = host_field(m, "tokens_per_second");
    if (speed && ((!host_is(speed, json_type_double) && !host_is(speed, json_type_int))
        || !isfinite(json_object_get_double(speed)))) return false;
    json_object *calls = host_field(m, "tool_calls");
    if (!calls) continue;
    if (!host_is(calls, json_type_array)) return false;
    for (size_t j = 0; j < json_object_array_length(calls); j++) {
      json_object *call = json_object_array_get_idx(calls, j);
      if (!host_is(call, json_type_object)) return false;
      json_object *fn = host_field(call, "function");
      if (!host_is(fn, json_type_object) || !host_is(host_field(fn, "name"), json_type_string)
          || !host_is(host_field(fn, "arguments"), json_type_string)) return false;
    }
  }
  return true;
}

static Term host_messages(Env e, json_object *root) {
  json_object *messages = host_field(root, "messages");
  Term list = term_pak(CID(Nil), 0);
  for (size_t i = json_object_array_length(messages); i-- > 0;) {
    json_object *m = json_object_array_get_idx(messages, i);
    json_object *role = host_field(m, "role");
    u64 role_id = host_equal(role, "user") ? 0
      : host_equal(role, "assistant") ? 1
      : host_equal(role, "tool") ? 2 : 3;
    Term calls = term_pak(CID(Nil), 0);
    json_object *array = host_field(m, "tool_calls");
    for (size_t j = array ? json_object_array_length(array) : 0; j-- > 0;) {
      json_object *fn = host_field(json_object_array_get_idx(array, j), "function");
      Term call = io_node(e, CID(Call), host_string(e, host_field(fn, "name")),
        host_string(e, host_field(fn, "arguments")));
      calls = io_node(e, CID(Con), call, calls);
    }
    Term speed = 0;
    json_object *number = host_field(m, "tokens_per_second");
    if (number) {
      char buffer[384]; // Enough for any finite double in fixed-point notation.
      snprintf(buffer, sizeof(buffer), "%.1f", json_object_get_double(number));
      speed = host_text(e, buffer);
    }
    // Bend 2.0.39 flattens Message's Role and Maybe fields into six slots.
    // Role is its ordinal; Maybe<String> is (present, string-or-zero).
    // Keep this layout in sync with protocol.bend and the pinned compiler.
    json_object *content = host_field(m, "content");
    u64 loc = heap_alloc(e, cls_fit(6));
    e.mem[loc] = role_id;
    e.mem[loc + 1] = content != NULL;
    e.mem[loc + 2] = io_seal(e, content ? host_string(e, content) : 0, CID(Message));
    e.mem[loc + 3] = io_seal(e, calls, CID(Message));
    e.mem[loc + 4] = number != NULL;
    e.mem[loc + 5] = io_seal(e, speed, CID(Message));
    Term message = term_ctr(CID(Message), loc);
    list = io_node(e, CID(Con), message, list);
  }
  return list;
}

typedef struct {
  char *payload;
  size_t length;
  json_object *reply;
  const char *error;
} HostExchange;

// json-c's per-buffer UTF-8 validation rejects characters split across reads.
// Validate the byte stream here, keeping partial code points between chunks.
static bool host_utf8(const unsigned char *bytes, size_t n, u32 *left, u32 *cp, u32 *minimum) {
  for (size_t i = 0; i < n; i++) {
    unsigned char c = bytes[i];
    if (*left) {
      if ((c & 0xc0) != 0x80) return false;
      *cp = (*cp << 6) | (c & 0x3f);
      if (--*left == 0 && (*cp < *minimum || *cp > 0x10ffff || (*cp >= 0xd800 && *cp <= 0xdfff))) return false;
    } else if (c >= 0xc2 && c <= 0xdf) {
      *left = 1; *cp = c & 0x1f; *minimum = 0x80;
    } else if (c >= 0xe0 && c <= 0xef) {
      *left = 2; *cp = c & 0x0f; *minimum = 0x800;
    } else if (c >= 0xf0 && c <= 0xf4) {
      *left = 3; *cp = c & 7; *minimum = 0x10000;
    } else if (c >= 0x80) {
      return false;
    }
  }
  return true;
}

static void host_exchange_call(IoWork *w) {
  HostExchange *state = (HostExchange *)w->data;
  size_t sent = 0;
  w->code = 0;
  while (sent < state->length) {
    ssize_t n = send(w->hand, state->payload + sent, state->length - sent, 0);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) { w->code = n < 0 ? errno : EIO; return; }
    sent += n;
  }
  json_tokener *parser = json_tokener_new_ex(64);
  json_tokener_set_flags(parser, JSON_TOKENER_STRICT);
  size_t total = 0;
  u32 utf8_left = 0, codepoint = 0, minimum = 0;
  while (true) {
    char buffer[4096];
    ssize_t n = recv(w->hand, buffer, sizeof(buffer), 0);
    if (n < 0 && errno == EINTR) continue;
    if (n <= 0) {
      w->code = n < 0 ? errno : EIO;
      state->error = "server disconnected before a complete reply arrived";
      break;
    }
    total += n;
    if (!host_utf8((unsigned char *)buffer, n, &utf8_left, &codepoint, &minimum)) {
      w->code = EINVAL;
      state->error = "invalid UTF-8 in server response";
      break;
    }
    if (total > 64 * 1024 * 1024) {
      w->code = EFBIG;
      state->error = "server reply exceeds 64 MiB";
      break;
    }
    state->reply = json_tokener_parse_ex(parser, buffer, n);
    enum json_tokener_error error = json_tokener_get_error(parser);
    if (error == json_tokener_continue) continue;
    if (error != json_tokener_success || utf8_left || !host_valid(state->reply)) {
      w->code = EINVAL;
      state->error = "invalid server response";
    }
    break;
  }
  json_tokener_free(parser);
}
static Term host_exchange_pack(Env e, IoWork *w) {
  HostExchange *state = (HostExchange *)w->data;
  Term result = w->code ? io_fail(e, w->code, state->error) : io_done(e, host_messages(e, state->reply));
  json_object_put(state->reply);
  free(state->payload);
  free(state);
  return io_tup(e, io_hand(w->hand), result);
}
static Term host_exchange(Env e, Term *f, IoWork *w) {
  HostExchange *state = io_mem(calloc(1, sizeof(*state)));
  u64 len;
  state->payload = io_cstr(e, f[1], &len);
  state->length = len;
  w->hand = io_hand_v(f[0]);
  w->data = (char *)state;
  return io_work(w, host_exchange_call, host_exchange_pack);
}

static void __attribute__((constructor)) host_register(void) {
  io_eff(CID(connect), host_connect);
  io_eff(CID(input), host_input);
  io_eff(CID(interactive), host_interactive);
  io_eff(CID(color), host_color);
  io_eff(CID(encode), host_encode);
  io_eff(CID(exchange), host_exchange);
}
