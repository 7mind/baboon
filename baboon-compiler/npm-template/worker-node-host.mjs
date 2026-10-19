// worker_threads entry for worker-acceptance.mjs (not published): serves on the port it is handed.
import { workerData } from "node:worker_threads";

import { serveBaboon } from "./worker-server.js";

serveBaboon(workerData.port);
