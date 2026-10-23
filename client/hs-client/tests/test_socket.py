"""Socket regression tests. Build hs-client, then run this file with Python 3."""

import json
import os
from pathlib import Path
import socket
import subprocess
import tempfile
import threading
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
        cls.binary = subprocess.check_output(
            ["cabal", "list-bin", "hs-client", "--offline"],
            cwd=CLIENT_DIR, text=True,
        ).strip()

    def run_chat(self, turns):
        errors = []
        with tempfile.TemporaryDirectory(prefix="hs-client-test-") as tmp:
            path = str(Path(tmp) / "server.sock")
            with socket.socket(socket.AF_UNIX) as server:
                server.bind(path)
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
                                ).encode()
                                # Exercise incremental parsing, including split UTF-8.
                                for offset in range(0, len(payload), 7):
                                    conn.sendall(payload[offset:offset + 7])
                    except Exception as exc:
                        errors.append(exc)

                worker = threading.Thread(target=serve, daemon=True)
                worker.start()
                result = subprocess.run(
                    [self.binary, path],
                    input="".join(prompt + "\n" for prompt, _ in turns) + "/quit\n",
                    capture_output=True, text=True, timeout=15,
                    env={**os.environ, "NO_COLOR": "1"},
                )
                worker.join(timeout=6)
                self.assertFalse(worker.is_alive())
                self.assertEqual(errors, [])
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


if __name__ == "__main__":
    unittest.main()
