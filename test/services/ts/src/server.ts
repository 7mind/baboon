import * as http from 'node:http';
import { BaboonCodecContext } from './generated/BaboonSharedRuntime.js';
import type { BaboonMethodId } from './generated/BaboonSharedRuntime.js';
import { PetStore } from './generated/petstore/api/index.js';
import { PetStoreImpl } from './petstore_impl.js';

const ctx = BaboonCodecContext.Default;

function readBody(req: http.IncomingMessage): Promise<Buffer> {
  return new Promise((resolve, reject) => {
    const chunks: Buffer[] = [];
    req.on('data', (chunk) => chunks.push(chunk));
    req.on('end', () => resolve(Buffer.concat(chunks)));
    req.on('error', reject);
  });
}

export function startServer(host: string, port: number): void {
  const impl = new PetStoreImpl();

  const server = http.createServer(async (req, res) => {
    try {
      if (req.method === 'GET' && req.url === '/health') {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end('ok');
        return;
      }

      if (req.method === 'POST' && req.url === '/reset') {
        impl.reset();
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end('ok');
        return;
      }

      if (req.method === 'POST' && req.url === '/shutdown') {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end('ok');
        setImmediate(() => {
          server.close(() => process.exit(0));
        });
        return;
      }

      if (req.method === 'POST') {
        const parts = (req.url ?? '').split('/').filter(Boolean);
        if (parts.length === 2) {
          const [serviceName, methodName] = parts;
          const method: BaboonMethodId = { serviceName, methodName };
          const contentType = req.headers['content-type'] ?? '';
          const body = await readBody(req);

          if (contentType.includes('application/octet-stream')) {
            // UEBA: bytes -> generated invokeUeba -> bytes
            const responseBytes = PetStore.invokeUeba_PetStore(
              method,
              new Uint8Array(body),
              impl,
              ctx
            );
            res.writeHead(200, { 'Content-Type': 'application/octet-stream' });
            res.end(Buffer.from(responseBytes));
            return;
          }

          // JSON: string -> generated invokeJson -> string
          const responseBody = PetStore.invokeJson_PetStore(
            method,
            body.toString('utf-8'),
            impl,
            ctx
          );
          res.writeHead(200, { 'Content-Type': 'application/json' });
          res.end(responseBody);
          return;
        }
      }

      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('Not Found');
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : String(err);
      res.writeHead(500, { 'Content-Type': 'text/plain' });
      res.end(msg);
    }
  });

  server.listen(port, host, () => {
    console.log(`Listening on ${host}:${port}`);
  });

  process.on('SIGTERM', () => {
    server.close();
    process.exit(0);
  });
  process.on('SIGINT', () => {
    server.close();
    process.exit(0);
  });
}
