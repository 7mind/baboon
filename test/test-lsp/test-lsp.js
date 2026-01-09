#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

const PROJECT_ROOT = path.resolve(__dirname, '../..');
const BABOON_BIN = path.join(PROJECT_ROOT, 'baboon-compiler/.jvm/target/graalvm-native-image/baboon');
const TEST_RESOURCES = path.join(PROJECT_ROOT, 'baboon-compiler/src/test/resources/baboon');

class LspClient {
  constructor(serverProcess) {
    this.serverProcess = serverProcess;
    this.buffer = Buffer.alloc(0);
    this.pendingRequests = new Map();
    this.nextId = 1;

    serverProcess.stdout.on('data', (data) => this.handleData(data));
    serverProcess.stderr.on('data', (data) => {
      console.error('[SERVER STDERR]', data.toString());
    });
  }

  handleData(data) {
    this.buffer = Buffer.concat([this.buffer, data]);
    this.processBuffer();
  }

  processBuffer() {
    while (true) {
      const headerEnd = this.buffer.indexOf('\r\n\r\n');
      if (headerEnd === -1) return;

      const header = this.buffer.slice(0, headerEnd).toString();
      const contentLengthMatch = header.match(/Content-Length: (\d+)/);
      if (!contentLengthMatch) {
        console.error('Invalid header:', header);
        return;
      }

      const contentLength = parseInt(contentLengthMatch[1], 10);
      const messageStart = headerEnd + 4;
      const messageEnd = messageStart + contentLength;

      if (this.buffer.length < messageEnd) return;

      const content = this.buffer.slice(messageStart, messageEnd).toString();
      this.buffer = this.buffer.slice(messageEnd);

      try {
        const message = JSON.parse(content);
        this.handleMessage(message);
      } catch (e) {
        console.error('Failed to parse message:', content, e);
      }
    }
  }

  handleMessage(message) {
    if (message.id !== undefined && this.pendingRequests.has(message.id)) {
      const { resolve, reject } = this.pendingRequests.get(message.id);
      this.pendingRequests.delete(message.id);

      if (message.error) {
        reject(new Error(`LSP Error ${message.error.code}: ${message.error.message}`));
      } else {
        resolve(message.result);
      }
    } else if (message.method) {
      console.log('[NOTIFICATION]', message.method, JSON.stringify(message.params, null, 2));
    }
  }

  send(message) {
    const content = JSON.stringify(message);
    const header = `Content-Length: ${Buffer.byteLength(content)}\r\n\r\n`;
    this.serverProcess.stdin.write(header + content);
  }

  request(method, params) {
    return new Promise((resolve, reject) => {
      const id = this.nextId++;
      this.pendingRequests.set(id, { resolve, reject });
      this.send({ jsonrpc: '2.0', id, method, params });

      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error(`Request ${method} timed out`));
        }
      }, 10000);
    });
  }

  notify(method, params) {
    this.send({ jsonrpc: '2.0', method, params });
  }
}

async function runTests() {
  console.log('=== Baboon LSP Integration Tests ===\n');

  // Check if native image exists
  if (!fs.existsSync(BABOON_BIN)) {
    console.error(`ERROR: Native image not found at ${BABOON_BIN}`);
    console.error('Run: npm run build-server');
    process.exit(1);
  }

  console.log(`Using server: ${BABOON_BIN}`);
  console.log(`Test resources: ${TEST_RESOURCES}\n`);

  // Start the LSP server
  const serverProcess = spawn(BABOON_BIN, ['--model-dir', TEST_RESOURCES, ':lsp'], {
    stdio: ['pipe', 'pipe', 'pipe']
  });

  const client = new LspClient(serverProcess);

  let passed = 0;
  let failed = 0;

  async function test(name, fn) {
    try {
      await fn();
      console.log(`✓ ${name}`);
      passed++;
    } catch (e) {
      console.log(`✗ ${name}`);
      console.log(`  Error: ${e.message}`);
      failed++;
    }
  }

  try {
    // Test 1: Initialize
    await test('Initialize', async () => {
      const result = await client.request('initialize', {
        processId: process.pid,
        rootUri: `file://${TEST_RESOURCES}`,
        capabilities: {},
        workspaceFolders: [
          { uri: `file://${TEST_RESOURCES}`, name: 'test' }
        ]
      });

      if (!result.capabilities) {
        throw new Error('No capabilities in response');
      }
      if (!result.capabilities.textDocumentSync) {
        throw new Error('No textDocumentSync capability');
      }
      if (!result.capabilities.hoverProvider) {
        throw new Error('No hoverProvider capability');
      }
      if (!result.capabilities.completionProvider) {
        throw new Error('No completionProvider capability');
      }
      if (!result.capabilities.definitionProvider) {
        throw new Error('No definitionProvider capability');
      }
    });

    // Test 2: Initialized notification
    await test('Initialized notification', async () => {
      client.notify('initialized', {});
      // Wait a bit for the server to process
      await new Promise(r => setTimeout(r, 500));
    });

    // Test 3: Open a document
    const testFile = path.join(TEST_RESOURCES, 'pkg0/pkg01.baboon');
    const testContent = fs.readFileSync(testFile, 'utf-8');
    const testUri = `file://${testFile}`;

    await test('textDocument/didOpen', async () => {
      client.notify('textDocument/didOpen', {
        textDocument: {
          uri: testUri,
          languageId: 'baboon',
          version: 1,
          text: testContent
        }
      });
      // Wait for diagnostics
      await new Promise(r => setTimeout(r, 1000));
    });

    // Test 4: Request document symbols
    await test('textDocument/documentSymbol', async () => {
      const symbols = await client.request('textDocument/documentSymbol', {
        textDocument: { uri: testUri }
      });

      if (!Array.isArray(symbols)) {
        throw new Error('Expected array of symbols');
      }
      console.log(`  Found ${symbols.length} symbols`);
    });

    // Test 5: Request hover over type reference "ForeignStruct" at line 17 (0-indexed: 16)
    await test('textDocument/hover', async () => {
      // Line 17 is: "   fld0: ForeignStruct"
      // ForeignStruct starts at character 10
      const hover = await client.request('textDocument/hover', {
        textDocument: { uri: testUri },
        position: { line: 16, character: 15 }
      });
      if (!hover) {
        throw new Error('Expected hover info for ForeignStruct');
      }
      if (!hover.contents || !hover.contents.value) {
        throw new Error('Expected hover contents');
      }
      console.log(`  Hover: "${hover.contents.value.substring(0, 50)}..."`);
    });

    // Test 6: Request completions at start of line (keyword position)
    await test('textDocument/completion (keywords)', async () => {
      const completions = await client.request('textDocument/completion', {
        textDocument: { uri: testUri },
        position: { line: 5, character: 0 }
      });

      if (!Array.isArray(completions)) {
        throw new Error('Expected array of completions');
      }
      console.log(`  Found ${completions.length} completions (keywords)`);
    });

    // Test 7: Request completions after colon (type position)
    await test('textDocument/completion (types after colon)', async () => {
      // Simulate typing "fld0: " - cursor after colon
      // We'll use a position that would be after a colon
      const completions = await client.request('textDocument/completion', {
        textDocument: { uri: testUri },
        position: { line: 16, character: 10 }  // After "   fld0: "
      });

      if (!Array.isArray(completions)) {
        throw new Error('Expected array of completions');
      }
      // Should have types and builtin types
      const hasTypes = completions.some(c => c.detail && !c.detail.includes('keyword'));
      if (!hasTypes) {
        throw new Error('Expected type completions');
      }
      console.log(`  Found ${completions.length} completions (types)`);
    });

    // Test 8: Go to definition
    await test('textDocument/definition', async () => {
      // Click on ForeignStruct reference at line 17
      const locations = await client.request('textDocument/definition', {
        textDocument: { uri: testUri },
        position: { line: 16, character: 15 }
      });

      if (!Array.isArray(locations) || locations.length === 0) {
        throw new Error('Expected definition location');
      }
      const loc = locations[0];
      console.log(`  Definition: ${loc.uri.split('/').pop()} line ${loc.range.start.line}`);
    });

    // Test 9: Completion with prefix filtering
    await test('textDocument/completion (prefix filter)', async () => {
      // Line 29 is: "    f: T1_E1"
      // Cursor after "f: T" is at character 8 (0-indexed)
      // "    f: T" = positions 0-7, cursor at 8 means after 'T'
      const completions = await client.request('textDocument/completion', {
        textDocument: { uri: testUri },
        position: { line: 28, character: 8 }  // After "    f: T"
      });

      if (!Array.isArray(completions)) {
        throw new Error('Expected array of completions');
      }
      // All completions should start with "T" or "t"
      const allStartWithT = completions.every(c => c.label.toLowerCase().startsWith('t'));
      if (!allStartWithT) {
        const nonT = completions.filter(c => !c.label.toLowerCase().startsWith('t')).map(c => c.label);
        throw new Error(`Expected all completions to start with T, but got: ${nonT.join(', ')}`);
      }
      console.log(`  Found ${completions.length} completions starting with 'T'`);
    });

    // Test 10: Shutdown
    await test('shutdown', async () => {
      const result = await client.request('shutdown', null);
      // Result should be null
    });

    // Test 11: Exit
    await test('exit', async () => {
      client.notify('exit', null);
      await new Promise(r => setTimeout(r, 500));
    });

  } catch (e) {
    console.error('Test suite error:', e);
    failed++;
  } finally {
    serverProcess.kill();
  }

  console.log(`\n=== Results: ${passed} passed, ${failed} failed ===`);
  process.exit(failed > 0 ? 1 : 0);
}

runTests().catch(e => {
  console.error('Fatal error:', e);
  process.exit(1);
});
