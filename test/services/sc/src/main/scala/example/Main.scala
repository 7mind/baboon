package example

object Main {
  def main(args: Array[String]): Unit = {
    assert(args.nonEmpty, "Usage: <server|client> --host <host> --port <port>")

    val mode = args(0)
    var host = "127.0.0.1"
    var port = 18080

    var i = 1
    while (i < args.length) {
      args(i) match {
        case "--host" =>
          assert(i + 1 < args.length, "--host requires a value")
          host = args(i + 1)
          i += 2
        case "--port" =>
          assert(i + 1 < args.length, "--port requires a value")
          port = args(i + 1).toInt
          i += 2
        case other =>
          throw new IllegalArgumentException(s"Unknown argument: $other")
      }
    }

    mode match {
      case "server" => PetStoreServer.start(host, port)
      case "client" => PetStoreClient.run(host, port)
      case _        => throw new IllegalArgumentException(s"Unknown mode: $mode")
    }
  }
}
