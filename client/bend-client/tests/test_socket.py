"""TCP framing regression tests. Run ./build.sh, then run this file with Python 3."""

import json
import os
from pathlib import Path
import socket
import subprocess
import tempfile
import threading
import time
import unittest


CLIENT_DIR = Path(__file__).resolve().parents[1]


def message(role, content=None, **fields):
    return {"role": role, "content": content, **fields}


def request(path):
    return message("assistant", tool_calls=[{
        "id": "call_1",
        "type": "function",
        "function": {"name": "ReadFile", "arguments": json.dumps({"path": path})},
    }])


class SocketTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = os.environ.get("BEND_CLIENT", str(CLIENT_DIR / "build/bend-client"))

    def run_chat(self, turns, prefix=""):
        errors = []
        with tempfile.TemporaryDirectory(prefix="bend-client-test-") as tmp:
            path = str(Path(tmp) / "server.sock")
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
                server.bind(("127.0.0.1", 0))
                port = str(server.getsockname()[1])
                server.listen(1)
                server.settimeout(5)

                def serve():
                    try:
                        conn, _ = server.accept()
                        with conn:
                            conn.settimeout(5)
                            history = []
                            for prompt, responses in turns:
                                incoming = b""
                                while True:
                                    chunk = conn.recv(4096)
                                    if not chunk:
                                        raise AssertionError("Client disconnected before sending input")
                                    incoming += chunk
                                    try:
                                        decoded = json.loads(incoming)
                                        break
                                    except (ValueError, UnicodeDecodeError):
                                        pass
                                self.assertEqual(decoded, message("user", prompt))
                                history += [decoded] + responses
                                payload = json.dumps(
                                    {"messages": list(reversed(history))}, ensure_ascii=False,
                                ).encode() + b"\n"
                                # Exercise incremental parsing, including split UTF-8.
                                for offset in range(0, len(payload), 7):
                                    conn.sendall(payload[offset:offset + 7])
                    except Exception as exc:
                        errors.append(exc)

                worker = threading.Thread(target=serve, daemon=True)
                worker.start()
                result = subprocess.run(
                    [self.binary, "--port", port],
                    input=prefix + "".join(prompt + "\n" for prompt, _ in turns) + "/quit\n",
                    capture_output=True, text=True, timeout=15,
                    env={**os.environ, "NO_COLOR": "1"},
                )
                worker.join(timeout=6)
                self.assertFalse(worker.is_alive())
                self.assertEqual(errors, [], result.stderr)
                self.assertEqual(result.returncode, 0, result.stderr)
                return result.stdout

    def test_tool_cycles_and_history(self):
        output = self.run_chat([
            ("read files", [
                request("a.hs"), message("tool", "first file"),
                message("assistant", "Reading another file."),
                request("b.hs"), message("tool", "second file"),
                message("assistant", "Both files read."),
            ]),
            ("thanks", [message("assistant", "You're welcome.")]),
        ])
        self.assertEqual(output,
            'you   > read files\n'
            'tool  > ReadFile {"path": "a.hs"}\n'
            'tool  > first file\n'
            'agent > Reading another file.\n'
            'tool  > ReadFile {"path": "b.hs"}\n'
            'tool  > second file\n'
            'agent > Both files read.\n\n'
            'you   > thanks\n'
            "agent > You're welcome.\n\n")

    def test_large_unicode_tool_result(self):
        contents = "café λ " * 1000
        output = self.run_chat([("read", [
            request("README.md"), message("tool", contents),
            message("assistant", "Done."),
        ])])
        self.assertIn("tool  > " + contents + "\nagent > Done.\n", output)
        self.assertNotIn("(no text reply)", output)

    def test_text_only(self):
        self.assertEqual(
            self.run_chat([("hello", [message("assistant", "Hi.")])]),
            "you   > hello\nagent > Hi.\n\n",
        )

    def test_optional_content_and_speed(self):
        output = self.run_chat([("hello", [
            message("assistant", tokens_per_second=123.45),
            request("a.hs"),
            message("tool"),
            message("assistant", "", tokens_per_second=None),
            message("assistant", "Done.", tokens_per_second=0),
        ])])
        self.assertEqual(output.count("(no text reply)"), 2)
        self.assertIn("123.5 tokens/s", output)
        self.assertIn("0.0 tokens/s", output)
        self.assertIn("(no tool output)", output)
        self.assertEqual(output.count("tokens/s"), 2)

    def test_terminal_controls_and_multiline(self):
        output = self.run_chat([("hello", [message("assistant", "a\n  b\t\x1b[31m\x07\x9b")])])
        self.assertIn("agent > a\n            b\t[31m\n", output)
        self.assertNotIn("\x1b", output)
        self.assertNotIn("\x07", output)
        self.assertNotIn("\x9b", output)

    def test_unicode_quoted_input(self):
        self.run_chat([('café λ 😀 "quotes" \\ backslash', [message("assistant", "ok")])])

    def test_size_boundary_and_recovery(self):
        # Compact JSON overhead is 28 bytes. Exactly 1024 bytes is accepted.
        output = self.run_chat([("a" * 996, [message("assistant", "ok")])],
                               prefix="a" * 997 + "\n" + "😀" * 250 + "\n" + '"' * 500 + "\n")
        self.assertEqual(output.count("Message too long"), 3)
        self.assertIn("agent > ok", output)

    def test_blank_lines(self):
        output = self.run_chat([("hello", [message("assistant", "hi")])], prefix="\n  \t\n")
        self.assertEqual(output.count("you   >"), 1)

    def raw_reply(self, payload, *, bytewise=False, framed=True):
        if framed:
            payload += b"\n"
        errors = []
        with tempfile.TemporaryDirectory(prefix="bend-raw-") as tmp:
            path = str(Path(tmp) / "s")
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
                server.bind(("127.0.0.1", 0))
                port = str(server.getsockname()[1])
                server.listen(1)
                server.settimeout(5)

                def serve():
                    try:
                        conn, _ = server.accept()
                        with conn:
                            conn.settimeout(5)
                            conn.recv(4096)
                            if bytewise:
                                for value in payload:
                                    conn.sendall(bytes([value]))
                                    time.sleep(0.001)
                            else:
                                conn.sendall(payload)
                    except (BrokenPipeError, ConnectionResetError):
                        pass  # Invalid responses may be rejected before all bytes arrive.
                    except Exception as exc:
                        errors.append(exc)

                worker = threading.Thread(target=serve, daemon=True)
                worker.start()
                result = subprocess.run([self.binary, "--port", port], input="hello\n/quit\n",
                                        text=True, capture_output=True, timeout=10)
                worker.join(6)
                self.assertFalse(worker.is_alive())
                self.assertEqual(errors, [])
                return result

    def test_each_utf8_byte_separate(self):
        payload = json.dumps({"messages": [message("assistant", "café λ 😀")]}, ensure_ascii=False).encode()
        result = self.raw_reply(payload, bytewise=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("café λ 😀", result.stdout)

    def test_malformed_json_and_schema(self):
        for payload in [b'{"messages": nope}', b'{}', b'[]', b'{"messages":null}',
                        b'{"messages":[{"role":"unknown"}]}',
                        b'{"messages":[{"role":"assistant","content":42}]}',
                        b'{"messages":[{"role":"assistant","tool_calls":[{}]}]}',
                        b'{"messages":[{"role":"assistant","tokens_per_second":"fast"}]}']:
            with self.subTest(payload=payload):
                result = self.raw_reply(payload)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("invalid server response", result.stderr)

    def test_no_agent_reply(self):
        result = self.raw_reply(b'{"messages":[{"role":"user","content":"hello"}]}')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no agent reply", result.stderr)

    def test_disconnect(self):
        for payload in [b'', b'{"messages":[']:
            with self.subTest(payload=payload):
                result = self.raw_reply(payload, framed=False)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("disconnected", result.stderr)

    def test_invalid_utf8(self):
        result = self.raw_reply(b'{"messages":[{"role":"assistant","content":"\xff"}]}')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("UTF-8", result.stderr)

    def test_help_arguments_and_missing_socket(self):
        help_result = subprocess.run([self.binary, "--help"], capture_output=True, text=True)
        self.assertEqual(help_result.returncode, 0)
        self.assertIn("127.0.0.1:7777", help_result.stdout)
        for args in [["--unknown"], ["a", "b"], ["/nonexistent/bend-test.sock"]]:
            with self.subTest(args=args):
                result = subprocess.run([self.binary, *args], capture_output=True, text=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertTrue(result.stderr)


if __name__ == "__main__":
    unittest.main()
