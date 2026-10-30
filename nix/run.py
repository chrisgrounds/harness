import argparse
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time


def group_exists(process):
    process.poll()
    try:
        os.killpg(process.pid, 0)
        return True
    except ProcessLookupError:
        return False


def stop_processes(processes):
    signal.signal(signal.SIGINT, signal.SIG_IGN)
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    for sig, grace in [(signal.SIGINT, 5), (signal.SIGTERM, 2), (signal.SIGKILL, 1)]:
        for process in processes:
            try:
                os.killpg(process.pid, sig)
            except ProcessLookupError:
                pass
        deadline = time.monotonic() + grace
        while any(group_exists(process) for process in processes):
            if time.monotonic() >= deadline:
                break
            time.sleep(0.05)
        else:
            break
    for process in processes:
        process.wait()


def exit_status(code):
    return code if code >= 0 else 128 - code


def run(server, client, args):
    processes = []
    with tempfile.TemporaryDirectory(prefix="harness-") as directory:
        socket_path = Path(directory) / "server.sock"
        try:
            server_args = [server, "--socket", str(socket_path)]
            if args.model:
                server_args += ["--model", args.model]
            if args.keep_failed:
                server_args.append("--keep-failed")
            server_process = subprocess.Popen(server_args, start_new_session=True)
            processes.append(server_process)
            deadline = time.monotonic() + 30
            while not socket_path.is_socket():
                code = server_process.poll()
                if code is not None:
                    return exit_status(code) or 1
                if time.monotonic() >= deadline:
                    raise RuntimeError("Server did not open its socket within 30 seconds.")
                time.sleep(0.05)
            processes.append(
                subprocess.Popen(
                    [client, "--project", args.project, str(socket_path)],
                    start_new_session=True,
                )
            )
            while True:
                for process in processes:
                    code = process.poll()
                    if code is not None:
                        return exit_status(code)
                time.sleep(0.1)
        finally:
            stop_processes(processes)


def main():
    server, client, *arguments = sys.argv[1:]
    parser = argparse.ArgumentParser(
        prog="harness", description="Run the Harness server and graphical client together."
    )
    parser.add_argument("--project", default=os.getcwd(), metavar="DIR")
    parser.add_argument("--model", metavar="MODEL", help="server default model")
    parser.add_argument("--keep-failed", action="store_true")
    args = parser.parse_args(arguments)
    if not Path(args.project).is_dir():
        parser.error(f"no such project directory: {args.project}")

    def interrupted(signum, _frame):
        raise SystemExit(128 + signum)

    signal.signal(signal.SIGINT, interrupted)
    signal.signal(signal.SIGTERM, interrupted)
    try:
        return run(server, client, args)
    except (OSError, RuntimeError) as error:
        print(f"harness: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
