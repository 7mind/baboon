import Foundation

func printUsageAndExit() -> Never {
    fputs("Usage: ServiceMain <server|client> --host <host> --port <port>\n", stderr)
    exit(1)
}

let args = CommandLine.arguments
guard args.count >= 2 else { printUsageAndExit() }

let mode = args[1]
var host = "127.0.0.1"
var port: UInt16 = 18080
var codec: ClientCodec = .both

var i = 2
while i < args.count {
    if args[i] == "--host", i + 1 < args.count {
        host = args[i + 1]
        i += 2
    } else if args[i] == "--port", i + 1 < args.count {
        guard let p = UInt16(args[i + 1]) else {
            fputs("Invalid port: \(args[i + 1])\n", stderr)
            exit(1)
        }
        port = p
        i += 2
    } else if args[i] == "--codec", i + 1 < args.count {
        guard let c = ClientCodec(rawValue: args[i + 1]) else {
            fputs("Invalid codec: \(args[i + 1]) (expected json|ueba|both)\n", stderr)
            exit(1)
        }
        codec = c
        i += 2
    } else {
        i += 1
    }
}

switch mode {
case "server":
    startServer(host: host, port: port)
case "client":
    runClient(host: host, port: port, codec: codec)
case "selftest":
    runSelfTest()
default:
    fputs("Unknown mode: \(mode)\n", stderr)
    exit(1)
}
