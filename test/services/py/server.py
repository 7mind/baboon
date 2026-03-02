import json
import signal
import sys
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler
from Generated.baboon_codecs import BaboonCodecContext
from Generated.petstore.api.petstore.addpet.In import In_JsonCodec as AddPetInCodec
from Generated.petstore.api.petstore.addpet.Out import Out_JsonCodec as AddPetOutCodec
from Generated.petstore.api.petstore.getpet.In import In_JsonCodec as GetPetInCodec
from Generated.petstore.api.petstore.getpet.Out import Out_JsonCodec as GetPetOutCodec
from Generated.petstore.api.petstore.listpets.In import In_JsonCodec as ListPetsInCodec
from Generated.petstore.api.petstore.listpets.Out import Out_JsonCodec as ListPetsOutCodec
from Generated.petstore.api.petstore.deletepet.In import In_JsonCodec as DeletePetInCodec
from Generated.petstore.api.petstore.deletepet.Out import Out_JsonCodec as DeletePetOutCodec
from petstore_impl import PetStoreImpl

ctx = BaboonCodecContext.compact()

HANDLERS: dict[str, tuple[object, object]] = {
    "addPet": (AddPetInCodec.instance(), AddPetOutCodec.instance()),
    "getPet": (GetPetInCodec.instance(), GetPetOutCodec.instance()),
    "listPets": (ListPetsInCodec.instance(), ListPetsOutCodec.instance()),
    "deletePet": (DeletePetInCodec.instance(), DeletePetOutCodec.instance()),
}

DISPATCH = {
    "addPet": lambda impl, inp: impl.add_pet(inp),
    "getPet": lambda impl, inp: impl.get_pet(inp),
    "listPets": lambda impl, inp: impl.list_pets(inp),
    "deletePet": lambda impl, inp: impl.delete_pet(inp),
}

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
            _service, method = parts
            if method in HANDLERS:
                content_length = int(self.headers.get("Content-Length", 0))
                body = self.rfile.read(content_length).decode("utf-8")
                try:
                    in_codec, out_codec = HANDLERS[method]
                    decoded = in_codec.decode(ctx, body)
                    result = DISPATCH[method](impl, decoded)
                    response_body = out_codec.encode(ctx, result)
                    self.send_response(200)
                    self.send_header("Content-Type", "application/json")
                    self.end_headers()
                    self.wfile.write(response_body.encode("utf-8"))
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
