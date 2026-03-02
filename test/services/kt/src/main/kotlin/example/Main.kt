package example

fun main(args: Array<String>) {
    require(args.isNotEmpty()) {
        "Usage: <server|client> --host <host> --port <port>"
    }

    val mode = args[0]
    var host = "127.0.0.1"
    var port = 18080

    var i = 1
    while (i < args.size) {
        when (args[i]) {
            "--host" -> {
                host = args[i + 1]
                i += 2
            }
            "--port" -> {
                port = args[i + 1].toInt()
                i += 2
            }
            else -> i++
        }
    }

    when (mode) {
        "server" -> PetStoreServer.start(host, port)
        "client" -> PetStoreClient.run(host, port)
        else -> throw IllegalArgumentException("Unknown mode: $mode")
    }
}
