import * as http from 'node:http';
import { BaboonCodecContext } from './generated/BaboonSharedRuntime.js';
import { In_JsonCodec as AddPetInCodec } from './generated/petstore/api/petstore/addpet/in.js';
import { Out_JsonCodec as AddPetOutCodec } from './generated/petstore/api/petstore/addpet/out.js';
import { In_JsonCodec as GetPetInCodec } from './generated/petstore/api/petstore/getpet/in.js';
import { Out_JsonCodec as GetPetOutCodec } from './generated/petstore/api/petstore/getpet/out.js';
import { In_JsonCodec as ListPetsInCodec } from './generated/petstore/api/petstore/listpets/in.js';
import { Out_JsonCodec as ListPetsOutCodec } from './generated/petstore/api/petstore/listpets/out.js';
import { In_JsonCodec as DeletePetInCodec } from './generated/petstore/api/petstore/deletepet/in.js';
import { Out_JsonCodec as DeletePetOutCodec } from './generated/petstore/api/petstore/deletepet/out.js';
import { PetStoreImpl } from './petstore_impl.js';

const ctx = BaboonCodecContext.Default;

interface MethodHandler {
  decodeInput: (json: unknown) => unknown;
  invoke: (input: unknown) => unknown;
  encodeOutput: (result: unknown) => unknown;
}

function buildHandlers(impl: PetStoreImpl): Record<string, MethodHandler> {
  return {
    addPet: {
      decodeInput: (json) => AddPetInCodec.instance.decode(ctx, json),
      invoke: (input) => impl.addPet(input as any),
      encodeOutput: (result) => AddPetOutCodec.instance.encode(ctx, result as any),
    },
    getPet: {
      decodeInput: (json) => GetPetInCodec.instance.decode(ctx, json),
      invoke: (input) => impl.getPet(input as any),
      encodeOutput: (result) => GetPetOutCodec.instance.encode(ctx, result as any),
    },
    listPets: {
      decodeInput: (json) => ListPetsInCodec.instance.decode(ctx, json),
      invoke: (input) => impl.listPets(input as any),
      encodeOutput: (result) => ListPetsOutCodec.instance.encode(ctx, result as any),
    },
    deletePet: {
      decodeInput: (json) => DeletePetInCodec.instance.decode(ctx, json),
      invoke: (input) => impl.deletePet(input as any),
      encodeOutput: (result) => DeletePetOutCodec.instance.encode(ctx, result as any),
    },
  };
}

function readBody(req: http.IncomingMessage): Promise<string> {
  return new Promise((resolve, reject) => {
    const chunks: Buffer[] = [];
    req.on('data', (chunk) => chunks.push(chunk));
    req.on('end', () => resolve(Buffer.concat(chunks).toString('utf-8')));
    req.on('error', reject);
  });
}

export function startServer(host: string, port: number): void {
  const impl = new PetStoreImpl();
  const handlers = buildHandlers(impl);

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
          const [_service, method] = parts;
          const handler = handlers[method];
          if (handler) {
            const body = await readBody(req);
            const json = JSON.parse(body);
            const input = handler.decodeInput(json);
            const result = handler.invoke(input);
            const outputJson = handler.encodeOutput(result);
            const responseBody = JSON.stringify(outputJson);
            res.writeHead(200, { 'Content-Type': 'application/json' });
            res.end(responseBody);
            return;
          }
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
