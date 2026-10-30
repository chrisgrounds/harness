import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest


FAKE_PROCESS = r'''
import os
from pathlib import Path
import signal
import socket
import subprocess
import sys
import time

root = Path(os.environ["HARNESS_FAKE_ROOT"])
role = "child" if "child" in sys.argv else Path(sys.argv[0]).name
signal.signal(signal.SIGINT, lambda *_: sys.exit(0))
signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))
(root / (role + ".pid")).write_text(str(os.getpid()))
if role == "server" and "HARNESS_FAKE_SERVER_EXIT" in os.environ:
    sys.exit(int(os.environ["HARNESS_FAKE_SERVER_EXIT"]))
child = None
listener = None
try:
    if role == "server":
        child = subprocess.Popen([sys.executable, __file__, "child"])
        listener = socket.socket(socket.AF_UNIX)
        listener.bind(sys.argv[sys.argv.index("--socket") + 1])
        listener.listen()
    if role == "client" and "HARNESS_FAKE_CLIENT_EXIT" in os.environ:
        time.sleep(0.2)
        sys.exit(int(os.environ["HARNESS_FAKE_CLIENT_EXIT"]))
    while True:
        if role == "server" and (root / "stop-server").exists():
            sys.exit(37)
        time.sleep(0.05)
finally:
    if listener:
        listener.close()
    if child:
        try:
            child.wait(timeout=0.2)
        except subprocess.TimeoutExpired:
            pass
'''


class LauncherTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="harness-launch-test-")
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        for role in ["server", "client"]:
            executable = self.root / role
            executable.write_text(f"#!{sys.executable}\n" + FAKE_PROCESS)
            executable.chmod(0o755)

    def launch(self, **extra_environment):
        environment = dict(os.environ, HARNESS_FAKE_ROOT=str(self.root), **extra_environment)
        process = subprocess.Popen(
            [sys.executable, str(Path(__file__).with_name("run.py")),
             str(self.root / "server"), str(self.root / "client"),
             "--project", str(self.root)],
            env=environment,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        self.addCleanup(self.stop, process)
        return process

    def stop(self, process):
        if process.poll() is None:
            process.send_signal(signal.SIGTERM)
        process.communicate(timeout=12)

    def ready(self, process):
        deadline = time.monotonic() + 5
        while not all((self.root / (role + ".pid")).exists() for role in ["server", "client", "child"]):
            if process.poll() is not None or time.monotonic() >= deadline:
                self.fail("Launcher exited or did not start both processes in time.")
            time.sleep(0.02)

    def assert_stopped(self, process, expected):
        _, error = process.communicate(timeout=12)
        self.assertEqual(process.returncode, expected, error.decode())
        deadline = time.monotonic() + 2
        for pid_file in self.root.glob("*.pid"):
            pid = int(pid_file.read_text())
            while True:
                try:
                    os.kill(pid, 0)
                except ProcessLookupError:
                    break
                if time.monotonic() >= deadline:
                    self.fail(f"{pid_file.stem} process {pid} was left running.")
                time.sleep(0.02)

    def test_ctrl_c_stops_both_processes_and_server_descendants(self):
        process = self.launch()
        self.ready(process)
        process.send_signal(signal.SIGINT)
        self.assert_stopped(process, 130)

    def test_client_exit_stops_server_and_preserves_exit_status(self):
        process = self.launch(HARNESS_FAKE_CLIENT_EXIT="7")
        self.assert_stopped(process, 7)

    def test_server_failure_stops_client_and_descendants(self):
        process = self.launch()
        self.ready(process)
        (self.root / "stop-server").touch()
        self.assert_stopped(process, 37)

    def test_server_startup_failure_does_not_start_client(self):
        process = self.launch(HARNESS_FAKE_SERVER_EXIT="23")
        self.assert_stopped(process, 23)
        self.assertFalse((self.root / "client.pid").exists())


if __name__ == "__main__":
    unittest.main()
