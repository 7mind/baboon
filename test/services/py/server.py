import signal
import sys
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler
from Generated.baboon_codecs import BaboonCodecContext
from Generated.baboon_service_wiring import BaboonMethodId, BaboonWiringException
from Generated.petstore.api.PetStore_Wiring import invoke_json_PetStore, invoke_ueba_PetStore
from petstore_impl import PetStoreImpl

ctx = BaboonCodecContext.compact()

impl = PetStoreImpl()

class PetStoreHandler(BaseHTTPRequestHandler):
    shutdown_hook: object = None

    def log_message(self, format: str, *args: object) -> None:
        pass

    def do_GET(self) -> None:
        if self.path == "/health":
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write(b"ok")
            return
        self.send_response(404)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(b"Not Found")

    def do_POST(self) -> None:
        if self.path == "/reset":
            impl.reset()
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write(b"ok")
            return

        if self.path == "/shutdown":
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write(b"ok")
            hook = PetStoreHandler.shutdown_hook
            if hook is not None:
                threading.Thread(target=hook, daemon=True).start()
            return

        parts = [p for p in self.path.split("/") if p]
        if len(parts) == 2:
            service, method = parts
            content_length = int(self.headers.get("Content-Length", 0))
            raw = self.rfile.read(content_length)
            content_type = self.headers.get("Content-Type", "")
            method_id = BaboonMethodId(service, method)
            try:
                if content_type == "application/octet-stream":
                    response_bytes = invoke_ueba_PetStore(method_id, raw, impl, ctx)
                    self.send_response(200)
                    self.send_header("Content-Type", "application/octet-stream")
                    self.end_headers()
                    self.wfile.write(response_bytes)
                    return
                else:
                    response_body = invoke_json_PetStore(method_id, raw.decode("utf-8"), impl, ctx)
                    self.send_response(200)
                    self.send_header("Content-Type", "application/json")
                    self.end_headers()
                    self.wfile.write(response_body.encode("utf-8"))
                    return
            except BaboonWiringException as e:
                self.send_response(404)
                self.send_header("Content-Type", "text/plain")
                self.end_headers()
                self.wfile.write(str(e).encode("utf-8"))
                return
            except Exception as e:
                self.send_response(500)
                self.send_header("Content-Type", "text/plain")
                self.end_headers()
                self.wfile.write(str(e).encode("utf-8"))
                return

        self.send_response(404)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(b"Not Found")


def start_server(host: str, port: int) -> None:
    server = HTTPServer((host, port), PetStoreHandler)

    def shutdown_server() -> None:
        server.shutdown()
        server.server_close()

    PetStoreHandler.shutdown_hook = shutdown_server

    def shutdown(signum: int, frame: object) -> None:
        shutdown_server()
        sys.exit(0)

    signal.signal(signal.SIGTERM, shutdown)
    signal.signal(signal.SIGINT, shutdown)
    print(f"Listening on {host}:{port}", flush=True)
    server.serve_forever()
