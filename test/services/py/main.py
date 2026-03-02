import sys
from server import start_server
from client import run_client

def main() -> None:
    args = sys.argv[1:]
    if not args:
        print("Usage: main.py <server|client> --host <host> --port <port>", file=sys.stderr)
        sys.exit(1)

    mode = args[0]
    host = "127.0.0.1"
    port = 18080
    i = 1
    while i < len(args):
        if args[i] == "--host" and i + 1 < len(args):
            host = args[i + 1]
            i += 2
        elif args[i] == "--port" and i + 1 < len(args):
            port = int(args[i + 1])
            i += 2
        else:
            i += 1

    if mode == "server":
        start_server(host, port)
    elif mode == "client":
        run_client(host, port)
    else:
        print(f"Unknown mode: {mode}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
